
module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end

type error = [`Unknown]

val ping         : unit -> (string, [> error ]) Core.Std.Result.t
val client_id    : unit -> (string, [> error ]) Core.Std.Result.t
val server_info  : unit -> (string, [> error ]) Core.Std.Result.t
val bucket_props : string -> unit -> (string, [> error ]) Core.Std.Result.t
val list_buckets : unit -> (string, [> error ]) Core.Std.Result.t
val list_keys    : string -> unit -> (string, [> error ]) Core.Std.Result.t

module Make : functor(Key:Key) (Value:Value) ->
sig
val get          : Opts.Get(Key).get -> unit -> (string, [> error ]) Core.Std.Result.t
val put          : Opts.Put(Key)(Value).put -> unit -> (string, [> error ]) Core.Std.Result.t
val delete       : Opts.Delete(Key).delete -> unit -> (string, [> error ]) Core.Std.Result.t
val index_search :
  stream:bool ->
  Opts.Index_search.index_search ->
  unit -> 
  (string, [> error ]) Core.Std.Result.t
end

