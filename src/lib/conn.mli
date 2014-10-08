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

module Make : functor(Key:Key) (Value:Value) ->
  sig
    type conn = t
    type t (*{conn:conn;bucket:string} *)

    val create: conn:conn -> bucket:string -> t

    module type Robj = Robj.Make
val list_keys :
  t ->
  (Key.t list, [> error | Response.error ]) Deferred.Result.t

val list_keys_stream :
  t ->
  (Key.t list -> unit Deferred.t) ->
  (unit, [> error | Response.error ]) Deferred.Result.t

val get :
  t ->
  ?opts:Opts.Get(Key)(Value).t list ->
  Key.t ->
  ([ `Maybe_siblings ] Robj(Key)(Value).t, [> Opts.Get.error ]) Deferred.Result.t

val put :
  t ->
  ?opts:Opts.Put(Key)(Value).t list ->
  ?k:Key.t ->
  [ `No_siblings ] Robj.t ->
  (([ `Maybe_siblings ] Robj(Key)(Value).t * string option), [> Opts.Put.error ]) Deferred.Result.t

val delete :
  t ->
  ?opts:Opts.Delete(Key).t list ->
  Key.t ->
  (unit, [> Opts.Delete(Key).error ]) Deferred.Result.t

val index_search :
  t ->
  ?opts:Opts.Index_search.t list ->
  index:Key.t ->
  Opts.Index_search.Query.t ->
  (Response.index_search, [> Opts.Index_search.error ]) Deferred.Result.t
end

