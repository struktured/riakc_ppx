module Old_int32 = Int32
module Old_char = Char
module Old_string = String

open Core.Std

module D = Protobuf.Decoder

type error = [ `Bad_payload | `Incomplete_payload | `Decoder_error]

type 'a t = More of 'a [@key 1]| Done of 'a [@key 2] [@@deriving Protobuf]

type props = { n_val      : int option [@key 1]
             ; allow_mult : bool option [@key 2]
} [@@deriving Protobuf]

type index_search = { keys         : string list [@key 1]
                    ; results      : (string * string option) list [@key 2]
                    ; continuation : string option [@key 3]
} [@@deriving Protobuf]

let parse_mc s =
  let bits = Bitstring.bitstring_of_string s in
  let module Int32 = Old_int32 in
  let module Char = Old_char in
  let module String = Old_string in
  let open Result.Monad_infix in
  try 
    let mc = String.get (Bitstring.string_of_bitstring (Bitstring.subbitstring bits 0 8)) 0 in
    let payload = Bitstring.subbitstring bits 8 ((Bitstring.bitstring_length bits) - 8) in
    Ok (mc, payload)
  with e -> Error `Incomplete_payload


let run mc mc_payload f =
  let open Result.Monad_infix in
  parse_mc mc_payload >>= function
    | (p_mc, payload) when p_mc = mc -> begin
      D.State.create payload >>= fun s ->
      D.run f s              >>= fun (r, _) ->
      Ok r
    end
    | _ ->
      Error `Bad_payload

let error payload =
  failwith "nyi"

let ping = function
  | "\x02" ->
    Ok (Done ())
  | _ ->
    Error `Bad_payload

let client_id payload =
  let open Result.Monad_infix in
  run '\x04' payload Pb_response.client_id >>= fun client_id ->
  Ok (Done client_id)

let server_info payload =
  let open Result.Monad_infix in
  run '\x08' payload Pb_response.server_info >>= fun server_info ->
  Ok (Done server_info)

let list_buckets payload =
  let open Result.Monad_infix in
  run '\x10' payload Pb_response.list_buckets >>= fun buckets ->
  Ok (Done buckets)

let list_keys payload =
  let open Result.Monad_infix in
  run '\x12' payload Pb_response.list_keys >>= function
    | (keys, false) ->
      Ok (More keys)
    | (keys, true) ->
      Ok (Done keys)

let bucket_props payload =
  let open Result.Monad_infix in
  run '\x14' payload Pb_response.bucket_props >>= fun (n_val, allow_mult) ->
  match n_val with
    | Some n_val32 -> begin
      match Int32.to_int n_val32 with
	| Some n_val ->
	  Ok (Done { n_val = Some n_val; allow_mult })
	| None ->
	  Error `Overflow
    end
    | None ->
      Ok (Done { n_val = None; allow_mult })

let get payload =
  let open Result.Monad_infix in
  run '\x0A' payload Pb_response.get >>= fun (c, vclock, unchanged) ->
  Ok (Done (Robj.of_pb c vclock unchanged))

let put payload =
  let open Result.Monad_infix in
  run '\x0C' payload Pb_response.put >>= fun (c, vclock, key) ->
  Ok (Done (Robj.of_pb c vclock None, key))

let delete = function
  | "\x0E" ->
    Ok (Done ())
  | _ ->
    Error `Bad_payload

 let index_search payload =
   let open Result.Monad_infix in
   run '\x1A' payload Pb_response.index_search >>= fun (ks, rs, cont, _d) ->
   Ok (Done { keys = ks; results = rs; continuation = cont })

let index_search_stream payload =
  let open Result.Monad_infix in
  run '\x1A' payload Pb_response.index_search >>= function
    | (ks, rs, _, Some false)
    | (ks, rs, _, None) ->
      Ok (More { keys = ks; results = rs; continuation = None })
    | (ks, rs, cont, Some true) ->
      Ok (Done { keys = ks; results = rs; continuation = cont })

let parse_length s =
  let bits = Bitstring.bitstring_of_string s in
  let to_int = Int32.to_int in
  let module Int32 = Old_int32 in
  let module Char = Old_char in
  let module String = Old_string in
  let open Result.Monad_infix in
  try 
    let len32 = Int32.of_string (Bitstring.string_of_bitstring (Bitstring.takebits 32 bits)) in
    match to_int len32 with
    Some n -> Ok n
    | None -> Error `Overflow
  with Invalid_argument _ -> Error `Incomplete
