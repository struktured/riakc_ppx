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

val bucket_props : t -> string -> (Response.Props.t, [> error | Response.error ]) Deferred.Result.t
val list_buckets : t -> (string list, [> error | Response.error ]) Deferred.Result.t

val list_keys :
  t ->
  string ->
  (string list, [> error | Response.error ]) Deferred.Result.t

val list_keys_stream :
  t ->
  string ->
  (string list -> unit Deferred.t) ->
  (unit, [> error | Response.error ]) Deferred.Result.t

val get :
  t ->
  ?opts:Opts.Get.t list ->
  b:string ->
  string ->
  ([ `Maybe_siblings ] Robj.t, [> Opts.Get.error ]) Deferred.Result.t

val put :
  t ->
  ?opts:Opts.Put.t list ->
  b:string ->
  ?k:string ->
  [ `No_siblings ] Robj.t ->
  (([ `Maybe_siblings ] Robj.t * string option), [> Opts.Put.error ]) Deferred.Result.t

val delete :
  t ->
  ?opts:Opts.Delete.t list ->
  b:string ->
  string ->
  (unit, [> Opts.Delete.error ]) Deferred.Result.t

val purge :
  t ->
  ?opts:Opts.Delete.t list ->
  b:string ->
  keys:string Core.Std.List.t ->
  (unit, [>Opts.Delete.error]) Deferred.Result.t
(*
val purge :
  t ->
  ?opts:'a list ->
  b:string ->
  keys:string Core.Std.List.t ->
  (unit,
   [> `Bad_conn
   | `Bad_payload
   | `Incomplete
   | `Incomplete_payload
   | `Overflow
   | `Unknown_type
   | `Wrong_type ])
    Core.Std._result Async_kernel.Deferred.t
 *)				 
val index_search :
  t ->
  ?opts:Opts.Index_search.t list ->
  b:string ->
  index:string ->
  Opts.Index_search.Query.t ->
  (Response.Index_search.t, [> Opts.Index_search.error ]) Deferred.Result.t
