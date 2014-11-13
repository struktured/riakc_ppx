
module Result = Core.Std.Result

type error = [`Unknown_type]

val ping         : unit -> (string, [> error ]) Result.t
val client_id    : unit -> (string, [> error ]) Result.t
val server_info  : unit -> (string, [> error ]) Result.t
val bucket_props : string -> unit -> (string, [> error ]) Result.t
val list_buckets : unit -> (string, [> error ]) Result.t
val list_keys    : string -> unit -> (string, [> error ]) Result.t

val get          : Opts.Get.get -> unit -> (string, [> error ]) Result.t
val put          : Opts.Put.put -> unit -> (string, [> error ]) Result.t
val delete       : Opts.Delete.delete -> unit -> (string, [> error ]) Result.t
val index_search :
  stream:bool ->
  Opts.Index_search.index_search ->
  unit -> 
  (string, [> error ]) Result.t
