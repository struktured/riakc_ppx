(*open Core.Std*)

(*type 'a list = 'a Core.Core_list.t [@key 1] [@@deriving Protobuf] *)
module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end

module Make (Key:Key) (Value:Value) =
struct
module Content = Protobuf_capable.Make(Robj.Content(Key)(Value))
type pair = (Key.t * string option) [@@deriving Protobuf]
type keys = Key.t list [@key 1] [@@deriving Protobuf]
type client_id   = string [@@deriving Protobuf]
type server_info = (string option * string option) [@@deriving Protobuf]
type list_buckets = string list  [@@deriving Protobuf]
type list_keys    = (string list * bool)  [@@deriving Protobuf]
type bucket_props = (Int32.t option * bool option) [@@deriving Protobuf]
type get          = (Content.t list [@key 1] * string option * bool option) [@@deriving Protobuf]
type put          = (Content.t list * string option * string option) [@@deriving Protobuf]
type index_search = (keys * pair list * string option * bool option) [@@deriving Protobuf]
end
