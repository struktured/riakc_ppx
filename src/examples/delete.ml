open Core.Std
open Async.Std

module StringCache = Caches.StringCache

let fail s =
  printf "%s\n" s;
  shutdown 1

let exec () =
  let host = Sys.argv.(1) in
  let port = Int.of_string Sys.argv.(2) in
  let b    = Sys.argv.(3) in
  let k    = Sys.argv.(4) in
  Riakc.Conn.with_conn
    ~host
    ~port
    (fun c -> StringCache.delete (StringCache.create ~conn:c ~bucket:b) k)

let eval () =
  exec () >>| function
    | Ok () -> begin
      printf "Deleted\n";
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
    | Error `Protobuf_encoder_error -> fail "Protobuf_encoder_error"


let () =
  ignore (eval ());
  never_returns (Scheduler.go ())
