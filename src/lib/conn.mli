open Core.Std
open Async.Std

type t

type error = [ `Bad_conn ]

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

val bucket_props : t -> string -> (Response.props, [> error | Response.error ]) Deferred.Result.t
val list_buckets : t -> (string list, [> error | Response.error ]) Deferred.Result.t

module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end

module Make : functor(Key:Key) (Value:Value) ->
sig
  type conn = t
  type cache (*{conn:conn;bucket:string} *)

  val create: conn:conn -> bucket:string -> cache

  module type Robj = Robj.Make
  val list_keys :
    t ->
    (Key.t list, [> error | Response.error ]) Deferred.Result.t

  val list_keys_stream :
    cache ->
    (Key.t list -> unit Deferred.t) ->
    (unit, [> error | Response.error ]) Deferred.Result.t

  val get :
    cache ->
    ?opts:Opts.Get(Key)(Value).t list ->
    Key.t ->
    ([ `Maybe_siblings ] Robj(Key)(Value).t, [> Opts.Get.error ]) Deferred.Result.t

  val put :
    cache ->
    ?opts:Opts.Put(Key)(Value).t list ->
    ?k:Key.t ->
    [ `No_siblings ] Robj.t ->
    (([ `Maybe_siblings ] Robj(Key)(Value).t * Key.t option), [> Opts.Put.error ]) Deferred.Result.t

  val delete :
    cache ->
    ?opts:Opts.Delete(Key).t list ->
    Key.t ->
    (unit, [> Opts.Delete(Key).error ]) Deferred.Result.t

  val index_search :
    cache ->
    ?opts:Opts.Index_search.t list ->
    index:Key.t ->
    Opts.Index_search.Query.t ->
    (Response.index_search, [> Opts.Index_search.error ]) Deferred.Result.t
end

