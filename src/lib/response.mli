
module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end
module Result = Core.Std.Result

type 'a t = More of 'a | Done of 'a

type error = [ `Bad_payload | `Incomplete_payload | `Protobuf_encoder_error | `Unknown_type | `Wrong_type | `Overflow ]
val error : string -> (unit t, [> error ]) Result.t

module Ping : sig
  type t = unit 
  include Protobuf_capable.S with type t:=t
end

val parse_length : string -> (int, [> error ]) Result.t

val ping : string -> (Ping.t t, [> error ]) Result.t

module Client_id : sig
  type t = string 
  include Protobuf_capable.S with type t:=t
end


val client_id    : string -> (Client_id.t t, [> error ]) Result.t

module Server_info : sig
  type t = (string option * string option) 
  include Protobuf_capable.S with type t:=t
end

val server_info  : string -> (Server_info.t t, [> error ]) Result.t

module Buckets : sig 
  type t = string list 
  include Protobuf_capable.S with type t:=t
end

val list_buckets : string -> (Buckets.t t, [> error ]) Result.t

module Props : sig 
  type t = {n_val: int option ; allow_mult: bool option } 
  include Protobuf_capable.S with type t:=t
end

val bucket_props : string -> (Props.t t, [> error ]) Result.t

module Make : functor (Key:Key) (Value:Value) ->
sig

type pair = (Key.t * string option) [@@deriving protobuf]
(*
module Keys : sig
  type t = Key.t list 
  include Protobuf_capable.S with type t:=t
end
*)
module List_keys : sig 
  type t = bytes list * bool option
  include Protobuf_capable.S with type t:=t 
end

val list_keys    : string -> (bytes list t, [> error ]) Result.t
val delete       : string -> (unit t, [> error ]) Result.t

module Get : sig
  type t = Robj.Content(Key) (Value).t list * string option * bool option
  include Protobuf_capable.S with type t:=t 
end
val get          : string -> ([ `Maybe_siblings ] Robj.Make(Key)(Value).t t, [> error ]) Result.t

module Put : sig 
 type t = Robj.Content(Key) (Value).t list * string option * string option 
 include Protobuf_capable.S with type t:=t 
end
val put          : string -> (([ `Maybe_siblings ] Robj.Make (Key) (Value).t * string option) t, [> error ]) Result.t

(*
module  Index_search : sig
  type t = Keys.t * pair list * string option * bool option 
end

val index_search :
  string ->
  (Index_search.t t, [> error ]) Result.t

val index_search_stream :
  string ->
  (Index_search.t t, [> error ]) Result.t
*)
end

