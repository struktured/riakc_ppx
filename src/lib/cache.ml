open Async.Std

module Result = Core.Std.Result
module String = Core.Std.String

module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end

module Make(Key:Key) (Value:Value) =
struct 
  type conn = Conn.t 
  type t = {conn:conn;bucket:string} 
  module Resp = Response.Make(Key)(Value)
  module Req = Request.Make(Key)(Value)
  module Get = Opts.Get(Key)
  module Robj_kv = Robj.Make(Key)
  module Put = Opts.Put(Key)(Value)
  module Delete = Opts.Delete(Key)
  
  let create ~conn ~bucket = {conn;bucket}
  let list_keys_stream cache consumer =
  Conn.do_request_stream
    cache.conn 
    consumer 
    (Request.list_keys cache.bucket)
    Resp.list_keys

  let list_keys cache =
  Conn.do_request
    cache.conn
    (Request.list_keys cache.bucket)
    Resp.list_keys
  >>| function
    | Result.Ok keys ->
      Result.Ok (List.concat keys)
    | Result.Error err ->
      Result.Error err


let get cache ?(opts = []) k =
  Conn.do_request
    cache.conn
    (Req.get (Get.get_of_opts opts ~b:cache.bucket ~k))
    Resp.get
  >>| function
    | Result.Ok [robj] -> begin
      if Robj_kv.contents robj = [] && Robj_kv.vclock robj = None then
	Result.Error `Notfound
      else
	Result.Ok robj 
    end
    | Result.Ok _ ->
      Result.Error `Wrong_type
    | Result.Error err ->
      Result.Error err

let put cache ?(opts = []) ?k robj =
  Conn.do_request
    cache.conn
    (Req.put (Put.put_of_opts opts ~b:cache.bucket ~k robj))
    Resp.put
  >>| function
    | Result.Ok [(robj, key)] ->
      Result.Ok (robj, key)
    | Result.Ok _ ->
      Result.Error `Wrong_type
    | Result.Error err ->
      Result.Error err

let delete cache ?(opts = []) k =
  Conn.do_request
    cache.conn
    (Req.delete (Delete.delete_of_opts opts ~b:cache.bucket ~k))
    Resp.delete
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
end

