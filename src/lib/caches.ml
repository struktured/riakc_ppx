
module Bytes = 
struct
  include Bytes
  let to_protobuf t e = Protobuf.Encoder.bytes t e
  let from_protobuf d = Protobuf.Decoder.bytes d
end

module BytesCache = Cache.Make(Bytes)(Bytes)



