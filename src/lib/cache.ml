open Async.Std

module Result = Core.Std.Result
module String = Core.Std.String

module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end

module Make(Key:Key) (Value:Value) =
struct 
  type conn = Conn.t 
  type t = {conn:conn;bucket:string} 

  let get_conn t = t.conn
  let get_bucket t = t.bucket

  module Content = Robj.Content
  module Put = Opts.Put
  module Get = Opts.Get
  module Delete = Opts.Delete
  type 'a result = Value.t * 'a Robj.t 

  let encode_decode b =
    let e = Protobuf.Encoder.create () in
    Protobuf.Encoder.bytes b e; Protobuf.Encoder.to_bytes e
    
 let create ~conn ~bucket = {conn;bucket}
  let list_keys_stream cache consumer =
  Conn.do_request_stream
    cache.conn 
    (fun bytes -> let keys = List.map (fun b -> 
      Key.from_protobuf (Protobuf.Decoder.of_bytes (encode_decode b))) bytes in consumer keys) 
    (Request.list_keys cache.bucket)
    Response.list_keys 


  let list_keys cache =
  Conn.do_request
    cache.conn
    (Request.list_keys cache.bucket)
    Response.list_keys
  >>| function
    | Result.Ok keys ->
        Result.Ok (List.map (fun b -> Key.from_protobuf (Protobuf.Decoder.of_bytes (encode_decode b))) (List.concat keys))
    | Result.Error err ->
      Result.Error err

let serialize_key (k:Key.t) = 
  let e = Protobuf.Encoder.create () in 
  Key.to_protobuf k e; Protobuf.Encoder.to_bytes e

let get cache ?(opts = []) (k:Key.t) =
 Conn.do_request
    cache.conn
    (Request.get (Get.get_of_opts opts ~b:cache.bucket ~k:(serialize_key k)))
    Response.get
  >>| function
    | Result.Ok [robj] -> begin
      if Robj.contents robj = [] && Robj.vclock robj = None then
	Result.Error `Notfound
      else
	Result.Ok robj 
    end
    | Result.Ok _ ->
      Result.Error `Wrong_type
    | Result.Error err ->
      Result.Error err

let put cache ?(opts = []) ?(k:Key.t option) (robj:'a Robj.t) =
  Conn.do_request
    cache.conn
    (Request.put (Put.put_of_opts opts ~b:cache.bucket ~k:(match k with None -> None | Some k -> Some (serialize_key k)) robj))
    Response.put
  >>| function
    | Result.Ok [(robj, key)] ->
      Result.Ok (robj, key)
    | Result.Ok _ ->
      Result.Error `Wrong_type
    | Result.Error err ->
      Result.Error err

let delete cache ?(opts = []) (k:Key.t) =
  Conn.do_request
    cache.conn
    (Request.delete (Delete.delete_of_opts opts ~b:cache.bucket ~k:(serialize_key k)))
    Response.delete
  >>| function
    | Result.Ok [()] ->
      Result.Ok ()
    | Result.Ok _ ->
      Result.Error `Wrong_type
    | Result.Error err ->
      Result.Error err
(*
let index_search t ?(opts = []) ~b ~index query_type =
  let idx_s =
    Opts.Index_search.index_search_of_opts
      opts
      ~b
      ~index
      ~query_type
  in
  Conn.do_request
    t
    (Request.index_search ~stream:false idx_s)
    Response.index_search
  >>| function
    | Ok [results] ->
      Ok results
    | Ok _ ->
      Error `Wrong_type
    | Error err ->
      Error err
*)

let bucket_props t = Conn.bucket_props (get_conn t) (get_bucket t)
end

