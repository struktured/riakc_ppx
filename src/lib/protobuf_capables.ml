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
  let to_protobuf t e = Protobuf.Encoder.bits32 (if t then Int32.one else Int32.zero) e
  let from_protobuf d = (Protobuf.Decoder.bits32 d) == Int32.one
  let show b = to_string b
end

module Int = struct
  include Core.Std.Int
  let to_protobuf t e = Protobuf.Encoder.varint (Int64.of_int t) e
  let from_protobuf d = Int64.to_int (Protobuf.Decoder.varint d)
  let show b = to_string b
end

