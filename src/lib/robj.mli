open Core.Std


module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end

module Link : functor(Key:Key) -> 
sig 
  type t = { bucket : string option [@key 1]
           ; key    : Key.t option [@key 2]
           ; tag    : string option [@key 3]
           } [@@deriving Protobuf]
include Protobuf_capable.S with type t := t
end

module Pair : functor(Key:Key) (Value:Value) ->
sig
  type t = { key: Key.t  [@key 1]
  ; value : Value.t option [@key 2]
  } [@@deriving Protobuf]
  include Protobuf_capable.S with type t := t
end


module Content : functor (Key:Key) (Value:Value) ->
sig
  type t = { value            : Value.t [@key 1]
  ; content_type     : string option [@key 2]
  ; charset          : string option [@key 3]
  ; content_encoding : string option [@key 4]
  ; vtag             : string option [@key 5]
  ; links            : Link(Key).t list [@key 6]
  ; last_mod         : Int32.t option [@key 7]
  ; last_mod_usec    : Int32.t option [@key 8]
  ; usermeta         : Pair(Key) (Value).t list [@key 9]
  ; indices          : Pair(Key) (Value).t list [@key 10]
  ; deleted          : bool option [@key 11]
  } [@@deriving Protobuf]
include Protobuf_capable.S with type t := t
end
