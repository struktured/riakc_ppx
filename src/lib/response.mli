
module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end


type error = [`Protobuf_encoder_error]
type client_id  = string [@@deriving Protobuf]
type server_info = (string option * string option) [@@deriving Protobuf]
type list_buckets = string list [@@deriving Protobuf]

module Make : functor (Key:Key) (Value:Value) ->
sig
type pair = (Key.t * string option) [@@deriving Protobuf]
type keys = Key.t list [@key 1] [@@deriving Protobuf]
type list_keys    = (keys * bool)  [@@deriving Protobuf]
type bucket_props = (int option * bool option) [@@deriving Protobuf]
type get          = (Robj.Content(Key) (Value).t list * string option * bool option) [@@deriving Protobuf]
type put          = (Robj.Content(Key) (Value).t list * string option * string option) [@@deriving Protobuf]
type index_search = (keys * pair list * string option * bool option) [@@deriving Protobuf]
end
