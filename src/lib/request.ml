open Core.Std

module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end


module B = Protobuf.Encoder

let wrap_request (mc:'a) s =
  (* Add 1 for the mc *)
  let l = String.length s + 1 in
  let preamble_mc = String.create 5 in
  preamble_mc.[0] <- Char.of_int_exn ((l lsr 24) land 0xff);
  preamble_mc.[1] <- Char.of_int_exn ((l lsr 16) land 0xff);
  preamble_mc.[2] <- Char.of_int_exn ((l lsr 8) land 0xff);
  preamble_mc.[3] <- Char.of_int_exn (l land 0xff);
  preamble_mc.[4] <- mc;
  preamble_mc ^ s

module Make(Key:Key) (Value:Value) = 
  struct
type error = [`Unknown]
let ping () =
  Ok (wrap_request '\x01' "")

let client_id () =
  Ok (wrap_request '\x03' "")

let server_info () =
  Ok (wrap_request '\x07' "")

let bucket_props bucket () =
  let open Result.Monad_infix in
  let b = B.create () in
  B.bytes bucket b;
  Ok (wrap_request '\x13' (B.to_string b))

let list_buckets () =
  Ok (wrap_request '\x0F' "")

let list_keys bucket () =
  let open Result.Monad_infix in
  let b = B.create () in
  B.bytes bucket b; 
  Ok (wrap_request '\x11' (B.to_string b))

let get g () =
  let module Get = Opts.Get(Key) in
  let open Get in 
  let open Result.Monad_infix in
  let b = B.create () in 
  Get.get_to_protobuf g b;
  Ok (wrap_request '\x09' (B.to_string b))

let put p () =
  let module Put = Opts.Put (Key) (Value) in
  let module Content = Robj.Content(Key) (Value) in
  let open Put in
  let b = B.create () in
  Put.put_to_protobuf p b;
  Ok (wrap_request '\x0B' (B.to_string b))

let delete d () =
  let module Delete = Opts.Delete (Key) in
  let open Delete in
  let open Result.Monad_infix in
  let b = B.create () in
  Delete.delete_to_protobuf d b; 
(*  B.bytes     b 1 d.bucket >>= fun () ->
          let msg = B.embd_msg b 2 d.key in 
          msg
          >>= fun () ->
  B.int32_opt b 3 d.rw     >>= fun () ->
  B.bytes_opt b 4 d.vclock >>= fun () ->
  B.int32_opt b 5 d.r      >>= fun () ->
  B.int32_opt b 6 d.w      >>= fun () ->
  B.int32_opt b 7 d.pr     >>= fun () ->
  B.int32_opt b 8 d.pw     >>= fun () ->
  B.int32_opt b 9 d.dw     >>= fun () ->
  *) 
  Ok (wrap_request '\x0D' (B.to_string b))

let index_search ~stream idx_s () =
  let open Opts.Index_search in
  let determine_index idx_s =
    match idx_s.query_type with
      | Query.Eq_int _
      | Query.Range_int _ ->
	idx_s.index ^ "_int"
      | Query.Eq_string _
      | Query.Range_string _ ->
	idx_s.index ^ "_bin"
  in
  let determine_key idx_s =
    match idx_s.query_type with
      | Query.Eq_int i ->
	Some (Int.to_string i)
      | Query.Eq_string s ->
	Some s
      | Query.Range_int _
      | Query.Range_string _ ->
	None
  in
  let determine_min idx_s =
    match idx_s.query_type with
      | Query.Range_int { Query.min = min_i } ->
	Some (Int.to_string min_i)
      | Query.Range_string { Query.min = min_s } ->
	Some min_s
      | Query.Eq_int _
      | Query.Eq_string _ ->
	None
  in
  let determine_max idx_s =
    match idx_s.query_type with
      | Query.Range_int { Query.max = max_i } ->
	Some (Int.to_string max_i)
      | Query.Range_string { Query.max = max_s } ->
	Some max_s
      | Query.Eq_int _
      | Query.Eq_string _ ->
	None
  in
  let determine_rt idx_s =
    match idx_s.query_type with
      | Query.Range_string r ->
	Some r.Query.return_terms
      | Query.Range_int r ->
	Some r.Query.return_terms
      | Query.Eq_string _
      | Query.Eq_int _ ->
	None
  in
  let determine_cont idx_s =
    Option.map ~f:Kontinuation.to_string idx_s.continuation
  in
  let query_type_conv = function
    | Query.Eq_string _
    | Query.Eq_int _ ->
      Ok 0
    | Query.Range_string _
    | Query.Range_int _ ->
      Ok 1
  in
  let idx  = determine_index idx_s in
  let key  = determine_key   idx_s in
  let min  = determine_min   idx_s in
  let max  = determine_max   idx_s in
  let rt   = determine_rt    idx_s in
  let cont = determine_cont  idx_s in
  let b    = B.create () in
  let open Result.Monad_infix in
  
  (*B.bytes     b  1 idx_s.bucket                     >>= fun () ->
  B.bytes     b  2 idx                              >>= fun () ->
  B.enum      b  3 idx_s.query_type query_type_conv >>= fun () ->
  B.bytes_opt b  4 key                              >>= fun () ->
  B.bytes_opt b  5 min                              >>= fun () ->
  B.bytes_opt b  6 max                              >>= fun () ->
  B.bool_opt  b  7 rt                               >>= fun () ->
  B.bool      b  8 stream                           >>= fun () ->
  B.int32_opt b  9 idx_s.max_results                >>= fun () ->
  B.bytes_opt b 10 cont                             >>= fun () ->
  *)Ok (wrap_request '\x19' (B.to_string b))
end

