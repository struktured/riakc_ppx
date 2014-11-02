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
val do_request  : t -> (unit -> (string, [> `Bad_conn ] as 'a) Deferred.Result.t) ->
  (String.t -> ('b Response.t, 'a) Deferred.Result.t) -> ('b list, 'a) Result.t Deferred.t

val do_request_stream  : 
  t -> 
  ('a -> unit Deferred.t) -> 
  (unit -> (string, [> `Bad_conn ] as 'b) Deferred.Result.t) ->
  (String.t -> ('a Response.t, 'b) Deferred.Result.t) ->
  (unit, 'b) Deferred.Result.t
