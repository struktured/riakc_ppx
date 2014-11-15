open Core.Std
open Async.Std

module Default_index = Cache.Default_index

module CompositeKey = 
  struct
    type t = { name: string [@key 1]; id: int [@key 2]} [@@deriving protobuf, show]
  end

module VariantValue =
  struct
    type t = GOOD [@key 1] | BAD [@key 2] [@@deriving protobuf, show]
  end

module Cache = Cache.Make(CompositeKey)(VariantValue)


let option_to_string =
  Option.value ~default:"<none>"

let hex_of_string =
  String.concat_map ~f:(fun c -> sprintf "%X" (Char.to_int c))

let print_usermeta content =
  let module U = Cache.Robj.Usermeta in
  List.iter
    ~f:(fun u ->
      printf "USERMETA: %s = %s\n" (CompositeKey.show (U.key u)) (option_to_string (U.value u)))
    (Cache.Robj.Content.usermeta content)

let print_indices content = 
  let module I = Default_index in
  let index_value_to_string = function
    | I.String s  -> "String " ^ s
    | I.Integer i -> "Integer " ^ (Int.to_string i)
    | I.Bad_int s -> "Bad_int " ^ s
    | I.Unknown s -> "Unknown " ^ s
  in
  List.iter
    ~f:(fun i ->
      printf "INDEX: %s = %s\n" (CompositeKey.show (Cache.Robj.Index.key i)) (option_to_string 
      (Option.map (Cache.Robj.Index.value i) index_value_to_string)))
    (Cache.Robj.Content.indices content)


let print_value content =
  let value = Cache.Robj.Content.value content in
  List.iter
    ~f:(printf "CONTENT: %s\n")
    (String.split ~on:'\n' (VariantValue.show value))

let print_contents =
  List.iter
    ~f:(fun content ->
      let module C = Cache.Robj.Content in
      printf "CONTENT_TYPE: %s\n" (option_to_string (C.content_type content));
      printf "CHARSET: %s\n" (option_to_string (C.charset content));
      printf "CONTENT_ENCODING: %s\n" (option_to_string (C.content_encoding content));
      print_usermeta content;
      print_indices content;
      print_value content)

let fail s =
  printf "%s\n" s;
  shutdown 1

let parse_index s =
  match String.lsplit2 ~on:':' s with
    | Some ("bin", kv) -> begin
      match String.lsplit2 ~on:':' kv with
	| Some (k, v) ->
            Cache.Robj.Index.create ~k:{CompositeKey.name=k;id=0} ~v:(Some (Default_index.String v))
	| None ->
	  failwith ("Bad index: " ^ s)
    end
    | Some ("int", kv) -> begin
      match String.lsplit2 ~on:':' kv with
	| Some (k, v) ->
            Cache.Robj.Index.create ~k:{CompositeKey.name=k;id=0} ~v:(Some (Default_index.Integer (Int.of_string v)))
	| None ->
	  failwith ("Bad index: " ^ s)
    end
    | _ ->
      failwith ("Bad index: " ^ s)


let rec add_2i r idx = 
  if idx < Array.length Sys.argv then
    let module R = Cache.Robj in
    let content = List.hd_exn (R.contents r) in
    let indices = R.Content.indices content in
    add_2i
      (R.set_content
	 (R.Content.set_indices
	    ((parse_index Sys.argv.(idx))::indices)
	    content)
	 r)
      (idx + 1)
  else
    r


let argv_or index default_val = if index < (Array.length Sys.argv) then Sys.argv.(index) else default_val

let exec () =
  let _ = Random.self_init () in
  let host = argv_or 1 "localhost" in
  let port = Int.of_string (argv_or 2 "8087") in
  let b    = argv_or 3 "complex-bucket" in
  let k    = let open CompositeKey in {name = argv_or 4 "foobar"; id = Random.int 1000} in
  print_endline ("CompositeKey is: " ^ (CompositeKey.show k));
  let v    = match (String.uppercase (argv_or 5 (if (Random.bool()) then "GOOD" else "BAD"))) with 
    | "GOOD" -> VariantValue.GOOD 
    | "BAD" -> VariantValue.BAD
    | _ -> failwith "Bad value. Must be GOOD or BAD" in
  Cache.with_cache
    ~host
    ~port
    ~bucket:b
    (fun c ->
      let module R = Cache.Robj in
      let robj = R.of_value v in
      let robj = add_2i robj 6 in 
      Cache.put c ~k ~opts:[Cache.Put.Return_body] robj)

let eval () =
  exec () >>| function
    | Ok (robj, _) -> begin
      let module R = Cache.Robj in
      let vclock =
	match R.vclock robj with
	  | Some v ->
	    hex_of_string v
	  | None ->
	    "<none>"
      in
      printf "VCLOCK: %s\n" vclock;
      print_contents (R.contents robj);
      shutdown 0
    end
    | Error `Bad_conn           -> fail "Bad_conn"
    | Error `Bad_payload        -> fail "Bad_payload"
    | Error `Incomplete_payload -> fail "Incomplete_payload"
    | Error `Notfound           -> fail "Notfound"
    | Error `Incomplete         -> fail "Incomplete"
    | Error `Overflow           -> fail "Overflow"
    | Error `Unknown_type       -> fail "Unknown_type"
    | Error `Wrong_type         -> fail "Wrong_type"
    | Error `Protobuf_encoder_error         -> fail "Protobuf_encoder_error"


let () =
  ignore (eval ());
  never_returns (Scheduler.go ())
