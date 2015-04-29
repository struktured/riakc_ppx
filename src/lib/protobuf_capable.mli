module E = Protobuf.Encoder
module D = Protobuf.Decoder

val proto_version : int

module type S =
  sig
    type t
    val from_protobuf : D.t -> t
    val to_protobuf : t -> E.t -> unit
  end

module type Raw_S =
  sig
    type t
    val to_string : t -> string
    val of_string : string -> t
  end

module Conversion :
  sig
    module Make : functor (S:S) ->
      sig
        include Raw_S with type t = S.t
      end
  end

val encode_decode : string -> string

type versioned = {version: int [@key 1]; data:string [@key 2]} [@@deriving protobuf]

val serialize_version : int -> ('a -> Protobuf.Encoder.t -> unit) -> 'a -> string

val serialize_proto : ('a -> Protobuf.Encoder.t -> unit) -> 'a  -> string
 
val deserialize_version : int -> (Protobuf.Decoder.t -> 'a) -> string -> 'a

val deserialize_proto : (Protobuf.Decoder.t -> 'a) -> string -> 'a
