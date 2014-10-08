open Core.Std

module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end


module Make : functor(Key:Key) (Value:Value) ->
sig
type error = [`Unknown]
val ping         : unit -> (string, [> error ]) Result.t
val client_id    : unit -> (string, [> error ]) Result.t
val server_info  : unit -> (string, [> error ]) Result.t
val bucket_props : string -> unit -> (string, [> error ]) Result.t
val list_buckets : unit -> (string, [> error ]) Result.t
val list_keys    : string -> unit -> (string, [> error ]) Result.t
val get          : Opts.Get(Key).get -> unit -> (string, [> error ]) Result.t
val put          : Opts.Put(Key)(Value).put -> unit -> (string, [> error ]) Result.t
val delete       : Opts.Delete(Key).delete -> unit -> (string, [> error ]) Result.t
val index_search :
  stream:bool ->
  Opts.Index_search.index_search ->
  unit -> 
  (string, [> error ]) Result.t
end

