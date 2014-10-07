open Core.Std

let option_of_bool = function
  | Some true -> Some true
  | _         -> None


module type Key = Protobuf_capable.S
module type Value = Protobuf_capable.S

module Link(Key:Key) = struct
  type t = { bucket : string option [@key 1]
           ; key    : Key.t option [@key 2]
           ; tag    : string option [@key 3]
  } [@@deriving Protobuf]
end

module Link(Key:Key) = Protobuf_capable.Make (Link (Key))

module Link(Key:Key) = struct
  type t = { bucket : string option [@key 1]
           ; key    : Key.t option [@key 2]
           ; tag    : string option [@key 3]
  } [@@deriving Protobuf]
end

module Link(Key:Key) = Protobuf_capable.Make (Link (Key))



module Link(Key:Key) = struct
  type t = { bucket : string option [@key 1]
           ; key    : Key.t option [@key 2]
           ; tag    : string option [@key 3]
  } [@@deriving Protobuf]
end

module Link(Key:Key) = Protobuf_capable.Make (Link (Key))


module Pair (Key:Key) (Value:Value) = struct
        type t = { key   : Key.t [@key 1]
           ; value : Value.t option [@key 2]
	   } [@@@deriving Protobuf]
end
module Pair(Key:Key) (Value:Value) = Protobuf_capable.Make(Pair(Key) (Value))

module Content(Key:Key) (Value:Value) = struct
  module Link = Link(Key)
  module Pair = Pair (Key) (Value)
  type t = { value            : Value.t [@key 1]
	   ; content_type     : string option [@key 2]
	   ; charset          : string option [@key 3]
	   ; content_encoding : string option [@key 4]
           ; vtag             : string option [@key 5]
           ; links            : Link.t list   [@key 6]
           ; last_mod         : Int32.t option [@key 7]
           ; last_mod_usec    : Int32.t option [@key 8]
           ; usermeta         : Pair.t list [@key 9]
           ; indices          : Pair.t list [@key 10]
           ; deleted          : bool option [@key 11]
  } [@@deriving Protobuf]
end

module Content(Key:Key) (Value:Value) = Protobuf_capable.Make(Content (Key) (Value))

