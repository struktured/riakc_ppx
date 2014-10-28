module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end
module Result = Core.Std.Result

type 'a t = More of 'a | Done of 'a

type error = [ `Bad_payload | `Incomplete_payload | `Protobuf_encoder_error ]

let error payload =
  failwith "nyi"
let parse_length s = raise (Invalid_argument "parse_length")
(*  let bits = Bitstring.bitstring_of_string s in
  let to_int = Int32.to_int in
  let module Int32 = Old_int32 in
  let module Char = Old_char in
  let module String = Old_string in
  let open Result.Monad_infix in
  bitmatch bits with
    | { len : 32 : bigendian } -> begin
      match to_int len with
	| Some n ->
	  Ok n
	| None ->
	  Error `Overflow
    end
    | { _ } ->
      Error `Incomplete *)

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
  (*let module Int32 = Old_int32 in
  let module Char = Old_char in
  let module String = Old_string in *)
  let open Result.Monad_infix in
  let mc = Bitstring.get bits 8 in 
  try 
    let payload = Bitstring.subbitstring bits 8 (Bitstring.bitstring_length bits) in
      Result.Ok (Core.Std.Char.of_int_exn mc, payload)
  with _ ->
      Result.Error `Incomplete_payload
 


let run mc mc_payload f =
  let open Result.Monad_infix in
  parse_mc mc_payload >>= function
    | (p_mc, payload) when p_mc = mc -> begin
      (*P.State.create payload >>= fun s -> *)
      let decoder = Protobuf.Decoder.of_string (Bitstring.string_of_bitstring payload) in
      let r = f decoder in
(*      P.run f s              >>= fun (r, _) -> *)
      Result.Ok r
    end
    | _ ->
      Result.Error `Bad_payload


module Client_id = struct
  type t = string (*[@@deriving protobuf]*)
  let from_protobuf (d:Protobuf.Decoder.t) : t = raise (Invalid_argument "uimplemented")
  let to_protobuf t e = raise (Invalid_argument "unimplemented")
end

let client_id payload =
  let open Result.Monad_infix in
  run '\x04' payload Client_id.from_protobuf >>= fun client_id ->
  Result.Ok (Done client_id)

module Server_info = struct
  type t = (string option * string option) (*[@@deriving protobuf] *)
  let from_protobuf (d:Protobuf.Decoder.t) : t = raise (Invalid_argument "uimplemented")
  let to_protobuf t e = raise (Invalid_argument "unimplemented")
end

let server_info payload = let open Result.Monad_infix in
 run '\x08' payload Server_info.from_protobuf >>= fun server_info -> Result.Ok (Done server_info)
module Buckets = struct 
  type t = string list (*[@@deriving protobuf]*)
  let from_protobuf (d:Protobuf.Decoder.t) : t = raise (Invalid_argument "uimplemented")
  let to_protobuf t e = raise (Invalid_argument "unimplemented")
end

let list_buckets payload = let open Result.Monad_infix in
 run '\x10' payload Buckets.from_protobuf >>= fun list_buckets -> Result.Ok (Done list_buckets)

module Props = struct 
  type t = (int option * bool option) (*[@@deriving protobuf]*)
  let from_protobuf (d:Protobuf.Decoder.t) : t = raise (Invalid_argument "uimplemented")
  let to_protobuf t e = raise (Invalid_argument "unimplemented")
end

let bucket_props payload = let open Result.Monad_infix in
 run '\x14' payload Props.from_protobuf >>= fun bucket_props -> Result.Ok (Done bucket_props)

module Make (Key:Key) (Value:Value) =
struct

type pair = (Key.t * string option) (*[@@deriving protobuf] *)
let pair_from_protobuf (d:Protobuf.Decoder.t) : pair = raise (Invalid_argument "uimplemented")
let pair_to_protobuf t e = raise (Invalid_argument "unimplemented")

module Keys = struct
  type t = Key.t list (*[@@deriving protobuf] *)
  let from_protobuf (d:Protobuf.Decoder.t) : t = raise (Invalid_argument "uimplemented")
  let to_protobuf t e = raise (Invalid_argument "unimplemented")
end


module Robj_kv = Robj.Make (Key) (Value)
module List_keys = struct 
  type t = Keys.t * bool (*[@@deriving protobuf] *)
  let from_protobuf (d:Protobuf.Decoder.t) : t = raise (Invalid_argument "uimplemented")
  let to_protobuf t e = raise (Invalid_argument "unimplemented")
end

let list_keys payload =
  let open Result.Monad_infix in
  run '\x12' payload List_keys.from_protobuf >>= function
   | (keys, true) ->
      Result.Ok (Done keys)
   | (keys, _) ->
      Result.Ok (More keys)
 
let bucket_props payload = let open Result.Monad_infix in
 run '\x14' payload Props.from_protobuf >>= fun bucket_props -> Result.Ok (Done bucket_props)

let delete = function
  | "\x0E" ->
    Result.Ok (Done ())
  | _ ->
    Result.Error `Bad_payload

module Get = struct
  type t = Robj.Content(Key) (Value).t list * string option * bool option (*[@@deriving protobuf]*)
  let from_protobuf (d:Protobuf.Decoder.t):t = raise (Invalid_argument "from_protobuf")
  let to_protobuf t e = raise (Invalid_argument "unimplemented")
end

let get payload =
  let open Result.Monad_infix in
  run '\x0A' payload Get.from_protobuf >>= fun (c, vclock, unchanged) ->
  Result.Ok (Done (Robj_kv.of_pb c vclock unchanged))


module Put = struct
  type t = Robj.Content(Key) (Value).t list * string option * string option (*[@@deriving protobuf]*)
  let from_protobuf (d:Protobuf.Decoder.t):t = raise (Invalid_argument "from_protobuf")
  let to_protobuf t e = raise (Invalid_argument "unimplemented")
end

let put payload =
  let open Result.Monad_infix in
  run '\x0C' payload Put.from_protobuf >>= fun (c, vclock, key) ->
  Result.Ok (Done (Robj_kv.of_pb c vclock None, key))

module Index_search = struct
  type t = Keys.t * pair list * string option * bool option (*[@@deriving protobuf]*)
   let from_protobuf (d:Protobuf.Decoder.t):t = raise (Invalid_argument "from_protobuf")
  let to_protobuf t e = raise (Invalid_argument "unimplemented")

type index_search = { keys         : Key.t list (*[@key 1]*)
                    ; results      : (string * string option) list (*[@key 2]*)
                    ; continuation : string option (*[@key 3]*)
} (*[@@deriving protobuf]*)

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

