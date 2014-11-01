open Core.Std
open Async.Std

type t

type error= [ `Bad_conn | `Bad_payload | `Incomplete_payload | `Protobuf_encoder_error | `Unknown | `Wrong_type ]

val connect   : host:string -> port:int -> (t, [> error ]) Deferred.Result.t
val close     : t -> (unit, [> error ]) Deferred.Result.t

val with_conn :
  host:string ->
  port:int ->
  (t -> ('a, [> error ] as 'e) Deferred.Result.t) ->
  ('a, 'e) Deferred.Result.t

val ping        : t -> (unit, [> error | Response.error ]) Deferred.Result.t
val client_id   : t -> (string, [> error | Response.error ]) Deferred.Result.t
val server_info :
  t ->
  ((string option * string option), [> error | Response.error ]) Deferred.Result.t

val bucket_props : t -> string -> (Response.Props.t, [> error | Response.error ]) Deferred.Result.t

val list_buckets : t -> (string list, [> error | Response.error ]) Deferred.Result.t

module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end


module Make : functor(Key:Key) (Value:Value) ->
sig
  type conn = t
  type cache (*{conn:conn;bucket:string} *)

  val create: conn:conn -> bucket:string -> cache

  val list_keys :
    cache ->
    (Key.t list, [> error | Response.error ]) Deferred.Result.t

  val list_keys_stream :
    cache ->
    (Key.t list -> unit Deferred.t) ->
    (unit, [> error | Response.error ]) Deferred.Result.t

  val get :
    cache ->
    ?opts:Opts.Get(Key).t list ->
    Key.t ->
    ([ `Maybe_siblings ] Robj.Make(Key)(Value).t, [> Opts.Get(Key).error ]) Deferred.Result.t

  val put :
    cache ->
    ?opts:Opts.Put(Key)(Value).t list ->
    ?k:Key.t ->
    [ `No_siblings ] Robj.Make(Key)(Value).t ->
    (([ `Maybe_siblings ] Robj.Make(Key)(Value).t * Key.t option), [> Opts.Put(Key)(Value).error ]) Deferred.Result.t

  val delete :
    cache ->
    ?opts:Opts.Delete(Key).t list ->
    Key.t ->
    (unit, [> Opts.Delete(Key).error ]) Deferred.Result.t
(*
  val index_search :
    cache ->
    ?opts:Opts.Index_search.t list ->
    index:Key.t ->
    Opts.Index_search.Query.t ->
    (Response.Make(Key)(Value).index_search, [> Opts.Index_search.error ]) Deferred.Result.t
*)
end

