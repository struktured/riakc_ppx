module Bytes = 
struct
  include Bytes
  let to_protobuf t e = Protobuf.Encoder.bytes t e
  let from_protobuf d = Protobuf.Decoder.bytes d
  let show = to_string 
end

module String = 
struct
  include String
  let to_protobuf t e = Protobuf.Encoder.bytes (Bytes.of_string t) e
  let from_protobuf d = Bytes.to_string (Protobuf.Decoder.bytes d)
  let show t = t 
end

module Bool = struct
  include Core.Std.Bool
  type bool_t = private bool [@@deriving protobuf]
  let to_protobuf = bool_t_to_protobuf
  let from_protobuf = bool_t_from_protobuf
  let show b = to_string b
end

module Int = struct
  include Core.Std.Int
  let to_protobuf t e = Protobuf.Encoder.varint (Int64.of_int t) e
  let from_protobuf d = Int64.to_int (Protobuf.Decoder.varint d)
  let show b = to_string b
end



module Float = struct
  include Core.Std.Float
  type float_t = private float [@@deriving protobuf]
  let to_protobuf = float_t_to_protobuf
  let from_protobuf = float_t_from_protobuf
  let show t = to_string t
end
