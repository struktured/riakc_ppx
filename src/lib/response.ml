module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end
module Result = Core.Std.Result
open Result

type 'a t = More of 'a | Done of 'a
type error = [ `Bad_payload | `Incomplete_payload | `Protobuf_encoder_error | `Unknown_type | `Wrong_type | `Overflow ]


let error payload =
  failwith "nyi"
let parse_length s = 
  let to_int = Core.Std.Int32.to_int in
  let open Result.Monad_infix in
  let len = Bitstring.extract_fastpath_int32_be_unsigned s 0 (Int32.of_int 32) in
  match to_int len with
	| Some n ->
	  Ok n
	| None ->
	  Error `Overflow

module Ping = struct
  type t = unit 
  let from_protobuf (d:Protobuf.Decoder.t) = ()
  let to_protobuf t (e:Protobuf.Encoder.t) = ()
end

let ping = function
  | "\x02" ->
    Result.Ok (Done ((): Ping.t))
  | _ ->
    Result.Error `Bad_payload

let parse_mc s =
  let bits = Bitstring.bitstring_of_string s in
  let open Result.Monad_infix in
  let mc = Bitstring.takebits 8 bits in
  let bits = Bitstring.dropbits 8 bits in 
  try 
    let payload = Bitstring.takebits (Bitstring.bitstring_length bits) bits in
      Result.Ok (Core.Std.Char.of_string (Bitstring.string_of_bitstring mc), payload)
  with _ ->
      Result.Error `Incomplete_payload
 

let run mc mc_payload f =
  let open Result.Monad_infix in
  parse_mc mc_payload >>= function
    | (p_mc, payload) when p_mc = mc -> begin
      (*print_bytes ("got payload: " ^ (Bitstring.string_of_bitstring payload));  *)
      let decoder = Protobuf.Decoder.of_string (Bitstring.string_of_bitstring payload) in
      let r = f decoder in
      Result.Ok r
    end
    | (p_mc, payload) -> print_bytes ("payload error: " ^ (Bitstring.string_of_bitstring payload)); 
      Result.Error `Bad_payload


module Client_id = struct
  type t = string [@@deriving protobuf]
end

let client_id payload =
  let open Result.Monad_infix in
  run '\x04' payload Client_id.from_protobuf >>= fun client_id ->
  Result.Ok (Done client_id)

module Server_info = struct
  type t = (string option * string option) [@@deriving protobuf] 
end

let server_info payload = let open Result.Monad_infix in
 run '\x08' payload Server_info.from_protobuf >>= fun server_info -> Result.Ok (Done server_info)
module Buckets = struct 
  type t = string list [@@deriving protobuf]
end

let list_buckets payload = let open Result.Monad_infix in
 run '\x10' payload Buckets.from_protobuf >>= fun list_buckets -> Result.Ok (Done list_buckets)

module Props = struct
  type t = {n_val : int option [@key 1] [@default 0]; allow_mult: bool option [@key 2] [@default false]} [@@deriving protobuf]
end

module Nested(T:Protobuf_capable.S) = struct
  type t = { value: T.t [@key 1]} [@@deriving protobuf]
end

let bucket_props payload = let open Result.Monad_infix in
 let module Message = Nested(Props) in
 run '\x14' payload Message.from_protobuf >>= fun bucket_props -> Result.Ok (Done bucket_props.value)

module Make (Key:Key) (Value:Value) =
struct

type pair = (Key.t * string option) [@@deriving protobuf] 

(*
module Keys = struct
  type t = Key.t list [@@deriving protobuf] 
end
*)

module Robj_kv = Robj.Make (Key) (Value)
module List_keys = struct 
  type t = bytes list [@key 1] * (bool option [@key 2] [@default false]) [@@deriving protobuf] 
end

let list_keys payload =
  let open Result.Monad_infix in
  run '\x12' payload List_keys.from_protobuf >>= function
   | (keys, Some true) ->
      Result.Ok (Done keys)
   | (keys, _) ->
      Result.Ok (More keys)

let delete = function
  | "\x0E" ->
    Result.Ok (Done ())
  | _ ->
    Result.Error `Bad_payload

module Get = struct
  module Content = Robj.Content(Key)(Value)
  type t = Content.t list * string option * bool option [@@deriving protobuf]
end

let get payload =
  let open Result.Monad_infix in
  run '\x0A' payload Get.from_protobuf >>= fun (c, vclock, unchanged) ->
  Result.Ok (Done (Robj_kv.of_pb c vclock unchanged))


module Put = struct
  module Content = Robj.Content(Key)(Value)
  type t = Content.t list * string option * string option [@@deriving protobuf]
end

let put payload =
  let open Result.Monad_infix in
  run '\x0C' payload Put.from_protobuf >>= fun (c, vclock, key) ->
  Result.Ok (Done (Robj_kv.of_pb c vclock None, key))

module Index_search = struct
  type t = Key.t list * pair list * string option * bool option [@@deriving protobuf]

type index_search = { keys         : Key.t list [@key 1]
                    ; results      : (string * string option) list [@key 2]
                    ; continuation : string option [@key 3]
} [@@deriving protobuf]

end

(*
let index_search payload = let open Index_search in 
   let open Result.Monad_infix in
   run '\x1A' payload Index_search.from_protobuf >>= fun (ks, rs, cont, _d) ->
   Result.Ok (Done { keys = ks; results = rs; continuation = cont })

let index_search_stream payload =
  let open Result.Monad_infix in
  run '\x1A' payload Index_sarch.index_search >>= function
    | (ks, rs, _, Some false)
    | (ks, rs, _, None) ->
      Result.Ok (More { keys = ks; results = rs; continuation = None })
    | (ks, rs, cont, Some true) ->
      Result.Ok (Done { keys = ks; results = rs; continuation = cont })
*)
end

