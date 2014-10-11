module E = Protobuf.Encoder
module D = Protobuf.Decoder

module type S = 
sig 
    type t [@@deriving Protobuf] 
end
