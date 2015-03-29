open Core.Std
open Async.Std

module Rconn = Riakc.Conn
(*module Robj  = Riakc.Robj*)

open Deferred.Result.Monad_infix

module String = Cache.String
module StringCache = Caches.StringCache
module State = struct
  type t = unit

  let create () = ()
end
		       
(*Only need to be able to resolve any conflicts b/c right now cannot even do that.
  Conflict resolution here is meant to be done in client code, not by riak. The
  context includes use of a bucket type in which siblings are allowed, not the default
  namespace.
*)
module Conflicted : sig
  type closedtype = Red | Yellow | Green
  (*compilationg would fail if next line is moved down 7 lines*)
  type recordt = { a: string; b: int; c:closedtype} 
  val closedtype_from_protobuf : Protobuf.Decoder.t -> closedtype
  val closedtype_from_protobuf_bare : Protobuf.Decoder.t -> closedtype
  val closedtype_to_protobuf : closedtype -> Protobuf.Encoder.t -> unit
  val closedtype_to_protobuf_bare : closedtype -> Protobuf.Encoder.t -> unit
  val pp_closedtype : Format.formatter -> closedtype -> unit
  val show_closedtype : closedtype -> string
  val resolveColor : closedtype * closedtype -> closedtype
  val recordt_from_protobuf : Protobuf.Decoder.t -> recordt
  val recordt_to_protobuf : recordt -> Protobuf.Encoder.t -> unit
  val pp_recordt : Format.formatter -> recordt -> unit
  val show_recordt : recordt -> string
  val resolve_conflicts : recordt list -> recordt
end = struct
  type closedtype =
      Red [@key 1]
    | Yellow [@key 2]
    | Green [@key 3]
	    [@@deriving protobuf, show];;

  type recordt = { a : string [@key 1];
		   b : int [@key 2];
		   c : closedtype [@key 3] }
		   [@@deriving protobuf, show];;

  let resolveColor (c1, c2) =
    match c1, c2 with
    | Green, Green -> Green
    | Red, _ -> Red
    | _, Red -> Red
    | Yellow, _ -> Yellow
    | _, Yellow -> Yellow;;
  (*Concatenate all strings seen in field a; summate all ints seen in field b;
    if we ever see a Red then stick with red else just output last color we saw.*)
  let resolve_conflicts bunchofts =
    let init = { a = ""; b = 0; c = Green } in
    let rec helper alist resolved = 
      match alist with
      | [] -> resolved
      | h::t -> let s = String.concat "" [resolved.a;h.a] in
		let updated = { resolved with a = s } in
		let updated2 = { updated with b = (updated.b + h.b) } in
	        { updated2 with c = Red } in
    helper bunchofts init;;
end 
		       
let assert_cond msg = function
  | true  -> Deferred.return (Ok ())
  | false -> begin
	     printf "Error: %s\n" msg;
	     Deferred.return (Error `Assert_failed)
	   end
	       
let string_of_pb_encoded_r (r : Conflicted.recordt) =
  let open Conflicted in 
  let e = Protobuf.Encoder.create () in
  (recordt_to_protobuf r e;
   Protobuf.Encoder.to_string e;);;

let recordt_of_pb pb =
  Protobuf.Decoder.of_string pb |> Conflicted.recordt_from_protobuf;;
  
let exec_update c updated_robj =
  let module R = StringCache.Robj in
  let open Opts.Put in 
  let opts = [Bucket_type "siblings_allowed"] in
  StringCache.put c ~opts ~k:"testkey" updated_robj >>|
    (fun x -> match x with
		(robj, (Some key)) -> ()
	      | (robj, None) -> ()
	      | _ -> printf "\nPut error." ; ())
      
let exec_get c k =
  let open Opts.Get in
  let opts = [BucketType "siblings_allowed"] in
  StringCache.get c ~opts k;;

let rec rlist_from_content_list accum contentlist =
  let module R = StringCache.Robj in
  match contentlist with
    [] -> accum
  | h::t -> rlist_from_content_list ((recordt_of_pb (R.Content.value h))::accum) t
  
let get_and_resolve_conflicts c =
  let module R = StringCache.Robj in
  let module C = StringCache.Robj.Content in
  let open Opts.Get in
  let open Response in
  let open Conflicted in
  exec_get c "testkey" >>= function
			 | robj -> (let rcrdtlist = rlist_from_content_list [] (R.contents robj) in
				    let resolved = resolve_conflicts rcrdtlist in
				    let e = Protobuf.Encoder.create () in
				    let _ = recordt_to_protobuf resolved e in
				    let pb = Protobuf.Encoder.to_string e in
				    let old_content_hd = R.content robj in
				    let new_content = R.Content.set_value pb old_content_hd in
				    let updated_robj = R.set_content new_content robj in
exec_update updated_robj);;
(*
			 | Core.Std.Result.Error `Notfound           -> printf "Not found"; ()
			 | Core.Std.Result.Error `Bad_conn           -> printf "Bad_conn"; ()
			 | Core.Std.Result.Error `Bad_payload        -> printf "Bad_payload"; ()
			 | Core.Std.Result.Error `Incomplete_payload -> printf "Incomplete_payload"; ()
			 | Core.Std.Result.Error `Unknown_type       -> printf "Unknown_type"; ()
			 | Core.Std.Result.Error `Wrong_type         -> printf "Wrong_type"; ()
			 | Core.Std.Result.Error `Overflow           -> printf "Overflow"; ()
			 | Core.Std.Result.Error `Protobuf_encoder_error -> printf "Protobuf_encoder_error"; ()
*)
  
    
let resolve_test c =
  let open Opts.Put in
  let module Robj = StringCache.Robj in
  let open Conflicted in
  let opts = [BucketType "siblings_allowed"] in 
  let r1 = { a="first"; b=1; c=Green } in
  let r2 = { a="second"; b=20; c=Red } in
  let r1aspb = string_of_pb_encoded_r r1 in
  let r2aspb = string_of_pb_encoded_r r2 in
  let robj =
    Robj.create
      (Robj.Content.create r1aspb)
  in
  let robj2 = Robj.create (Robj.Content.create r2aspb) in
  let key = "testkey" in
  StringCache.put c opts ~k:key robj >>=
    fun _ -> (Core.Std.Unix.sleep 6; Deferred.return(Ok ())) >>=
    fun _ -> StringCache.put c opts ~k:key robj2 >>=
    fun _ -> (Unix.sleep 6; Deferred.return(Ok ())) >>=
    fun _ -> get_and_resolve_conflicts c >>=
    fun _ -> exec_get c "testkey" >>=
    fun robj -> (let rlist = R.contents robj in
		 assert_cond "Failed to resolve conflict; siblings exist" (List.length rlist = 1)) >>=
    fun result -> match result with
		    Ok () -> (let cont = R.content robj in
			      let pb = Robj.value cont in
			      let r = recordt_of_pb pb in
			      assert_cond
				"Value is wrong. Must have failed to resolve conflict or to put with vclock. Check db with curl."
				((r.b = 21) && (r.c=Red)))
		  | Error _ -> Deferred.return (Ok())
		  
  
let tests = [ ("todo"           , todo_test)]
	      
let execute_test t =
  let with_cache () =
    StringCache.with_cache
      ~host:"127.0.0.1"
      ~port:8087
      ~bucket:(Sys.argv.(3))
      (fun cache -> t cache)
  in
  with_cache ()
	     
let rec execute_tests s = function
  | [] -> Deferred.return (shutdown 0)
  | (name, t)::ts -> begin
		     let open Deferred.Monad_infix in
		     execute_test t >>= function
				      | Ok () -> begin
						 printf "%s...PASSED\n" name;
						 execute_tests s ts
					       end
				      | Error _ -> begin
						   printf "%s...FAILED\n" name;
						   execute_tests s ts
						 end
		   end
		       
let run_tests () =
  execute_tests (State.create ()) tests
		
let () =
  ignore (run_tests ());
  never_returns (Scheduler.go ())
		
