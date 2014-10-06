open Core.Std


module type Key = Protobuf_capable.S
module type Value = Protobuf_capable.S

module Link : functor(Key:Key) -> 
sig include Protobuf_capable.S
  type t = { bucket : string option [@key 1]
           ; key    : Key.t option [@key 2]
           ; tag    : string option [@key 3]
           } [@@deriving Protobuf]
end

module Pair : functor(Key:Key) (Value:Value) ->
sig
  include Protobuf_capable.S
  type t = { key: Key.t  [@key 1]
  ; value : Value.t option [@key 2]
  } [@@deriving Protobuf]
end


module Content : sig
  type t = { value            : string
  ; content_type     : string option
  ; charset          : string option
  ; content_encoding : string option
  ; vtag             : string option
  ; links            : Link.t list
  ; last_mod         : Int32.t option
  ; last_mod_usec    : Int32.t option
  ; usermeta         : Pair.t list
  ; indices          : Pair.t list
  ; deleted          : bool option
  }

  val parse : t Protobuf.Parser.t
  val build : t -> (string, [> Protobuf.Builder.error ]) Result.t
end
