open Core.Std
open Async.Std

module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end

module Result = Core.Std.Result

module Make : functor(Key:Key) (Value:Value) ->
sig
  module Get : module type of Opts.Get(Key) 
  module Robj : module type of Robj.Make(Key)(Value)
  module Content : module type of Robj.Content
  module Put : module type of Opts.Put(Key)(Value)
  module Delete : module type of Opts.Delete(Key)
  
  type conn = Conn.t
  type t  (*{conn:conn;bucket:string} *)

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
    ?opts:Get.t list ->
    Key.t ->
    ([ `Maybe_siblings ] Robj.t, [> Get.error ]) Deferred.Result.t

  val put :
    t ->
    ?opts:Put.t list ->
    ?k:Key.t ->
    [ `No_siblings ] Robj.t -> (* TODO what is the string option for here ?? *) 
    (([ `Maybe_siblings ] Robj.t * string option), [> Put.error ]) Deferred.Result.t

  val delete :
    t ->
    ?opts:Delete.t list ->
    Key.t ->
    (unit, [> Delete.error ]) Deferred.Result.t
(*
  val index_search :
    t ->
    ?opts:Opts.Index_search.t list ->
    index:Key.t ->
    Opts.Index_search.Query.t ->
    (Response.Make(Key)(Value).index_search, [> Opts.Index_search.error ]) Deferred.Result.t
*)
  val bucket_props : 
    t -> (Response.Props.t ,
                            [> `Bad_conn
                            | `Bad_payload
                            | `Incomplete_payload
                            | `Protobuf_encoder_error
                            | `Unknown_type
                            | `Wrong_type 
                            | `Overflow ]) Deferred.Result.t
end

