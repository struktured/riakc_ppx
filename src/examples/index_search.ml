open Core.Std
open Async.Std

module Default_index = Cache.Default_index

module Cache = Cache.Make_with_index(Cache.String)(Cache.String)(Cache.String)

let hex_of_string =
  String.concat_map ~f:(fun c -> sprintf "%X" (Char.to_int c))

let fail s =
  printf "%s\n" s;
  shutdown 1

let parse_qt idx =
  let module Is = Riakc.Opts.Index_search in
  match Sys.argv.(idx) with
    | "eq:int" ->
      Is.Query.eq_int (Int.of_string (Sys.argv.(idx + 1)))
    | "eq:bin" ->
      Is.Query.eq_string Sys.argv.(idx + 1)
    | "range:int" ->
      Is.Query.range_int
	~min:(Int.of_string (Sys.argv.(idx + 1)))
	~max:(Int.of_string (Sys.argv.(idx + 2)))
	~return_terms:false
    | "range:bin" ->
      Is.Query.range_string
	~min:Sys.argv.(idx + 1)
	~max:Sys.argv.(idx + 2)
	~return_terms:false
    | search ->
      failwith ("Unknown search: " ^ search)

let exec () = 

  let host  = Sys.argv.(1) in
  let port  = Int.of_string Sys.argv.(2) in
  let b     = Sys.argv.(3) in
  let index = Sys.argv.(4) in
  let qt    = parse_qt 5   in
  Cache.with_cache
    ~host
    ~port
    ~bucket:b
    (fun c ->
      Cache.index_search
	c
	~index
	qt)

let print_keys (results:Response.Index_search.t) = 
  let open Response.Index_search in results.keys

let eval () =
  exec () >>| function
    | Ok results -> begin
      let _ = print_keys results in
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
