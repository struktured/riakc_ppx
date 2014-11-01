open Async.Std

module Result = Core.Std.Result
module String = Core.Std.String

module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end


type t = { r : Reader.t
	 ; w : Writer.t
	 }

type error = [ `Bad_conn | `Bad_payload | `Incomplete_payload | `Protobuf_encoder_error | `Unknown | `Wrong_type ]

let rec read_str r pos s = 
  Reader.read r ~pos s >>= function
    | `Ok l -> begin
      if (pos + l) <> String.length s then
	read_str r (pos + l) s
      else
	Deferred.return (Result.Ok s)
    end
    | `Eof ->
      Deferred.return (Result.Error `Bad_conn)

let parse_length preamble = raise (Invalid_argument "unsupported operation")
(*  Deferred.return (Response.parse_length preamble) *)

let read_payload r preamble =
  let open Deferred.Result.Monad_infix in
  parse_length preamble >>= fun resp_len ->
  let payload = String.create resp_len in
  read_str r 0 payload

let rec read_response r f c =
  let open Deferred.Result.Monad_infix in
  let preamble = String.create 4 in
  read_str r 0 preamble       >>= fun _ ->
  read_payload r preamble     >>= fun payload ->
  Deferred.return (f payload) >>= function
    | Response.More resp ->
      let open Deferred.Monad_infix in
      c resp >>= fun () ->
      read_response r f c
    | Response.Done resp ->
      let open Deferred.Monad_infix in
      c resp >>= fun () ->
      Deferred.return (Result.Ok ())

let do_request_stream t c g f =
  let open Deferred.Result.Monad_infix in
  Deferred.return (g ()) >>= fun request ->
  Writer.write t.w request;
  read_response t.r f c

let do_request t g f =
  let open Deferred.Monad_infix in
  let (r, w) = Pipe.create () in
  let c x    = Pipe.write_without_pushback w x; Deferred.return () in
  do_request_stream t c g f >>= function
    | Result.Ok () -> begin
      Pipe.close w;
      Pipe.to_list r >>| fun l ->
      Result.Ok l
    end
    | Result.Error err ->
      Deferred.return (Result.Error err)

let gen_consumer w = Pipe.write_without_pushback w |> Deferred.return

let connect ~host ~port =
  let connect () =
    Tcp.connect (Tcp.to_host_and_port host port)
  in
  Monitor.try_with connect >>| function
    | Result.Ok (_s, r, w) ->
      Result.Ok { r; w }
    | Result.Error _exn ->
      Result.Error `Bad_conn

let close t =
  Writer.close t.w >>= fun () ->
  Deferred.return (Result.Ok ())

let with_conn ~host ~port f =
  connect host port >>= function
    | Result.Ok c -> begin
      f c    >>= fun r ->
      close c >>= fun _ ->
      Deferred.return r
    end
    | Result.Error err ->
      Deferred.return (Result.Error err)

let ping t =
  do_request
    t
    Request.ping
    Response.ping
  >>| function
    | Result.Ok [()] ->
      Result.Ok ()
    | Result.Ok _ ->
      Result.Error `Wrong_type
    | Result.Error err ->
      Result.Error err

let client_id t =
  do_request
    t
    Request.client_id
    Response.client_id
  >>| function
    | Result.Ok [client_id] ->
      Result.Ok client_id
    | Result.Ok _ ->
      Result.Error `Wrong_type
    | Result.Error err ->
      Result.Error err

let server_info t =
  do_request
    t
    Request.server_info
    Response.server_info
  >>| function
    | Result.Ok [(node, version)] ->
      Result.Ok (node, version)
    | Result.Ok _ ->
      Result.Error `Wrong_type
    | Result.Error err ->
      Result.Error err

let bucket_props t bucket =
  do_request
    t
    (Request.bucket_props bucket)
    Response.bucket_props
  >>| function
    | Result.Ok [props] ->
      Result.Ok props
    | Result.Ok _ ->
      Result.Error `Wrong_type
    | Result.Error err ->
      Result.Error err

let list_buckets t =
  do_request
    t
    Request.list_buckets
    Response.list_buckets
  >>| function
      Result.Ok [buckets] ->
      Result.Ok buckets
    | Result.Ok _ ->
      Result.Error `Wrong_type
    | Result.Error err ->
      Result.Error err

module Make(Key:Key) (Value:Value) =
struct 
  type conn = t 
  type cache = {conn:conn;bucket:string} 
  module Resp = Response.Make(Key)(Value)
  module Req = Request.Make(Key)(Value)
  module Get = Opts.Get(Key)
  module Robj_kv = Robj.Make(Key)(Value)
  module Put = Opts.Put(Key)(Value)
  module Delete = Opts.Delete(Key)
  
  let create ~conn ~bucket = {conn;bucket}
  let list_keys_stream cache consumer =
  do_request_stream
    cache.conn 
    consumer 
    (Request.list_keys cache.bucket)
    Resp.list_keys

  let list_keys cache =
  do_request
    cache.conn
    (Request.list_keys cache.bucket)
    Resp.list_keys
  >>| function
    | Result.Ok keys ->
      Result.Ok (List.concat keys)
    | Result.Error err ->
      Result.Error err


let get t ?(opts = []) ~b k =
  do_request
    t
    (Req.get (Get.get_of_opts opts ~b ~k))
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

let put t ?(opts = []) ~b ?k robj =
  do_request
    t
    (Req.put (Put.put_of_opts opts ~b ~k robj))
    Resp.put
  >>| function
    | Result.Ok [(robj, key)] ->
      Result.Ok (robj, key)
    | Result.Ok _ ->
      Result.Error `Wrong_type
    | Result.Error err ->
      Result.Error err

let delete t ?(opts = []) ~b k =
  do_request
    t
    (Req.delete (Delete.delete_of_opts opts ~b ~k))
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
  do_request
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

