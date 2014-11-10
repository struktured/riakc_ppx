open Core.Std
open Async.Std

module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end

module Result = Core.Std.Result

module Make : functor(Key:Key) (Value:Value) ->
sig
(*  module Get : module type of Opts.Get
  module Robj : module type of Robj
  module Content : module type of Robj.Content
  module Put : module type of Opts.Put
  module Delete : module type of Opts.Delete
  *)

  type conn = Conn.t
  type t  

  val create: conn:conn -> bucket:string -> t

  val get_conn : t -> conn
  val get_bucket : t -> string

  val list_keys :
    t ->
    (Key.t list, [> Conn.error | Response.error ]) Deferred.Result.t

  val list_keys_stream :
    t ->
    (Key.t list -> unit Deferred.t) ->
    (unit, [> Conn.error | Response.error ]) Deferred.Result.t

  val get :
    t ->
    ?opts:Opts.Get.t list ->
    Key.t ->
    ([ `Maybe_siblings ] Robj.t, [> Opts.Get.error ]) Deferred.Result.t

  val put :
    t ->
    ?opts:Opts.Put.t list ->
    ?k:Key.t ->
    [ `No_siblings ] Robj.t -> (* TODO what is the string option for here ?? *) 
    (([ `Maybe_siblings ] Robj.t * string option), [> Conn.error | Opts.Put.error ]) Deferred.Result.t

  val delete :
    t ->
    ?opts:Opts.Delete.t list ->
    Key.t ->
    (unit, [> Opts.Delete.error ]) Deferred.Result.t
(*
  val index_search :
    t ->
    ?opts:Opts.Index_search.t list ->
    index:Key.t ->
    Opts.Index_search.Query.t ->
    (Response.Make(Key)(Value).index_search, [> Opts.Index_search.error ]) Deferred.Result.t
*)
  val bucket_props : 
    t -> (Response.Props.t , [> Conn.error | Response.error]) Deferred.Result.t
end

