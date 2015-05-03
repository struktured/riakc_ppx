module E = Protobuf.Encoder
module D = Protobuf.Decoder

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

(* The internal protocol version *)
let proto_version = 0

let encode_decode (b:string) =
    let e = Protobuf.Encoder.create () in
    Protobuf.Encoder.bytes (Bytes.of_string b) e; Protobuf.Encoder.to_string e

type versioned = {version: int [@key 1]; data:string [@key 2]} [@@deriving protobuf]

let serialize_version version to_protobuf (v:'a) =
  let e = Protobuf.Encoder.create () in
  match version with
  | 0 -> to_protobuf v e; Protobuf.Encoder.to_string e
  | n -> failwith("Unknown serializer protocol version: " ^ (string_of_int n))

let serialize_proto (to_protobuf : 'a -> Protobuf.Encoder.t -> unit) (v:'a) : string =
 let e = Protobuf.Encoder.create () in 
 let versioned = {version=proto_version; 
  data=serialize_version proto_version to_protobuf v} in
 versioned_to_protobuf versioned e;Protobuf.Encoder.to_string e

let deserialize_version version from_protobuf b =
  match version with 
  | 0 -> let d = Protobuf.Decoder.of_string b in from_protobuf d
  | n -> failwith("Unknown deserializer protocol version: " ^ (string_of_int n))

let deserialize_proto (from_protobuf:Protobuf.Decoder.t -> 'a) (b:string) : 'a =
  let d = Protobuf.Decoder.of_string b in 
  let versioned = versioned_from_protobuf d in
  deserialize_version versioned.version from_protobuf versioned.data

module Conversion =
  struct
    module Make(S:S) =
      struct
              type t = S.t
              let to_string t = serialize_proto S.to_protobuf t
              let of_string (s:string) = failwith("nyi")
      end
  end

