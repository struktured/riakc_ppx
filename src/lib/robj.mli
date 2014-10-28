module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end


module Link : functor(Key:Key) -> 
sig 
  type t = { bucket : string option [@key 1]
           ; key    : Key.t option [@key 2]
           ; tag    : string option [@key 3]
           } [@@deriving protobuf]
include Protobuf_capable.S with type t := t
end

module Pair : functor(Key:Key) (Value:Value) ->
sig
  type t = { key: Key.t  [@key 1]
  ; value : Value.t option [@key 2]
  } [@@deriving protobuf]
  include Protobuf_capable.S with type t := t
end


module Usermeta : functor(Key:Key) (Value:Value) ->
sig
  type t = { key : Key.t [@key 1]
           ; value : Value.t option [@key 2]
  } [@@deriving protobuf]

  val create : k:Key.t -> v:Value.t option -> t

  val key : t ->  Key.t
  val value : t -> Value.t option

  val set_key : Key.t -> t -> t 
  val set_value: Value.t option -> t -> t

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
  ; usermeta         : Usermeta(Key) (Value).t list [@key 9]
  ; indices          : Pair(Key) (Value).t list [@key 10]
  ; deleted          : bool option [@key 11]
  } [@@deriving protobuf]
include Protobuf_capable.S with type t := t
end

module Make : functor(Key:Key) (Value:Value) ->
sig
  type 'a t 
  val of_pb :
    Content (Key) (Value).t list ->
    string option ->
    bool option ->
    [ `Maybe_siblings ] t

  val to_pb : 'a t -> (Content (Key) (Value).t list * string option)

  val create       : Content (Key) (Value).t -> [ `No_siblings ] t
  val contents     : 'a t -> Content (Key) (Value).t list
  val content      : [ `No_siblings ] t -> Content (Key) (Value).t
  val vclock       : 'a t -> string option
  val unchanged    : 'a t -> bool

  val set_contents : Content (Key) (Value).t list -> 'a t -> [ `Maybe_siblings ] t
  val set_content  : Content (Key) (Value).t -> 'a t -> [ `No_siblings ] t
  val set_vclock   : string option -> 'a t -> 'a t 
end
