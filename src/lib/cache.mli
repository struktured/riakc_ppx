open Core.Std
open Async.Std

module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end

module Result = Core.Std.Result

module Make : functor(Key:Key) (Value:Value) ->
sig
  type conn = Conn.t
  type t  (*{conn:conn;bucket:string} *)

  val create: conn:conn -> bucket:string -> t

  val list_keys :
    t ->
    (Key.t list, [> Conn.error | Response.error ]) Deferred.Result.t

  val list_keys_stream :
    t ->
    (Key.t list -> unit Deferred.t) ->
    (unit, [> Conn.error | Response.error ]) Deferred.Result.t

  val get :
    t ->
    ?opts:Opts.Get(Key).t list ->
    Key.t ->
    ([ `Maybe_siblings ] Robj.Make(Key)(Value).t, [> Opts.Get(Key).error ]) Deferred.Result.t

  val put :
    t ->
    ?opts:Opts.Put(Key)(Value).t list ->
    ?k:Key.t ->
    [ `No_siblings ] Robj.Make(Key)(Value).t -> (* TODO what is the string option for here ?? *) 
    (([ `Maybe_siblings ] Robj.Make(Key)(Value).t * string option), [> Opts.Put(Key)(Value).error ]) Deferred.Result.t

  val delete :
    t ->
    ?opts:Opts.Delete(Key).t list ->
    Key.t ->
    (unit, [> Opts.Delete(Key).error ]) Deferred.Result.t
(*
  val index_search :
    t ->
    ?opts:Opts.Index_search.t list ->
    index:Key.t ->
    Opts.Index_search.Query.t ->
    (Response.Make(Key)(Value).index_search, [> Opts.Index_search.error ]) Deferred.Result.t
*)
end

