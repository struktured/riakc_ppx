open Core.Std

module type Key = Protobuf_capable.S
module type Value = Protobuf_capable.S

type pair = (Key.t * string option)
type keys = Key.t list

type client_id   = string [@@deriving Protobuf]
type server_info = (string option * string option) [@@deriving Protobuf]
type list_buckets = string list  [@@deriving Protobuf]
type list_keys    = (string list * bool)  [@@deriving Protobuf]
type bucket_props = (Int32.t option * bool option) [@@deriving Protobuf]
type get          = (Pb_robj.Content.t list * string option * bool option) [@@deriving Protobuf]
type put          = (Pb_robj.Content.t list * string option * string option) [@@deriving Protobuf]
type index_search = (keys * pair list * string option * bool option) [@@deriving Protobuf]
