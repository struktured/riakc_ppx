
module E = Protobuf.Encoder
module D = Protobuf.Decoder

module type Protobuf_t = 
sig 
    type t [@@deriving Protobuf] 
    (* TODO To prevent merlin errors, would like to remove this! *)
    val t_to_protobuf : t -> E.t -> unit 
    val t_from_protobuf : D.t -> t
end

module type S = 
sig 
  include Protobuf_t 
  val from_protobuf : D.t -> t
  val to_protobuf : t -> E.t -> unit
end


module type Make = functor(Protobuf_t:Protobuf_t) -> S
