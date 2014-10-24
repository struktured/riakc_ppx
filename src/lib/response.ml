module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end
module Result = Core.Std.Result

type 'a t = More of 'a | Done of 'a

type error = [ `Bad_payload | `Incomplete_payload | `Protobuf_encoder_error ]

let error payload =
  failwith "nyi"

module Ping = struct
  type t = unit [@@deriving Protobuf]
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


module Client_id : sig
  type t = string 
  include Protobuf_capable.S with type t:=t
end

let client_id payload =
  let open Result.Monad_infix in
  run '\x04' payload Pb_response.client_id >>= fun client_id ->
  Ok (Done client_id)




val client_id    : string -> (Client_id.t t, [> error ]) Result.t

module Server_info : sig
  type t = (string option * string option) 
  include Protobuf_capable.S with type t:=t
end

val server_info  : string -> (Server_info.t t, [> error ]) Result.t

module Buckets : sig 
  type t = string list 
  include Protobuf_capable.S with type t:=t
end

val list_buckets : string -> (Buckets.t t, [> error ]) Result.t

module Props : sig 
  type t = (int option * bool option) 
  include Protobuf_capable.S with type t:=t
end

val bucket_props : string -> (Props.t t, [> error ]) Result.t

module Make : functor (Key:Key) (Value:Value) ->
sig

type pair = (Key.t * string option) [@@deriving Protobuf]

module Keys : sig
  type t = Key.t list 
  include Protobuf_capable.S with type t:=t
end

module List_keys : sig 
  type t = Keys.t * bool
  include Protobuf_capable.S with type t:=t 
end

val list_keys    : string -> (List_keys.t t, [> error ]) Result.t
val delete       : string -> (unit t, [> error ]) Result.t

module Get : sig
  type t = Robj.Content(Key) (Value).t list * string option * bool option
  include Protobuf_capable.S with type t:=t 
end
val get          : string -> ([ `Maybe_siblings ] Robj.Make(Key)(Value).t t, [> error ]) Result.t

module Put : sig 
 type t = Robj.Content(Key) (Value).t list * string option * string option 
 include Protobuf_capable.S with type t:=t 
end
val put          : string -> (([ `Maybe_siblings ] Robj.Make (Key) (Value).t * string option) t, [> error ]) Result.t


module  Index_search : sig
  type t = Keys.ud * pair list * string option * bool option 
end

val index_search :
  string ->
  (Index_search.t t, [> error ]) Result.t

val index_search_stream :
  string ->
  (Index_search.t t, [> error ]) Result.t

end

