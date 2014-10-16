module E = Protobuf.Encoder
module D = Protobuf.Decoder

module type S = 
sig 
    type t 
    val from_protobuf : D.t -> t
    val to_protobuf : t -> E.t -> unit
end
