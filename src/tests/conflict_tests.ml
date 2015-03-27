open Core.Std
open Async.Std

module Rconn = Riakc.Conn
module Robj  = Riakc.Robj

open Deferred.Result.Monad_infix

module String = Cache.String
module StringCache = Caches.StringCache
(*Only need to be able to resolve any conflicts b/c right now cannot even do that.
  Conflict resolution here is meant to be done in client code, not by riak. The
  context includes use of a bucket type in which siblings are allowed, not the default
  namespace.
*)
module Conflicted : sig
  type closedtype = Red | Yellow | Green;
  val closedtype_from_protobuf : Protobuf.Decoder.t -> closed_type
  val closedtype_from_protobuf_bare : Protobuf.Decoder.t -> closed_type
  val closedtype_to_protobuf : closedtype -> Protobuf.Encoder.t -> unit
  val closedtype_to_protobuf_bare : closedtype -> Protobuf.Encoder.t -> unit
  val pp_closedtype : Format.formatter -> closedtype -> unit
  val show_closedtype : closedtype -> string
  val closedtype_to_string : closedtype -> string
  type t = { a: string; b: int; c:closedtype} 
  val t_from_protobuf : Protobuf.Decoder.t -> t
  val t_to_protobuf : t -> Protobuf.Encoder.t -> unit
  val pp_t : Format.formatter -> t -> unit
  val show_t : t -> string
end = struct
  type closedtype =
      Red [@key 1]
    | Yellow [@key 2]
    | Green [@key 3]
	    [@@deriving protobuf, show];;
  type t = { a : string [@key 1];
	     b : int [@key 2];
	     c : closedtype [@key 3] }
	     [@@deriving protobuf, show];;
end 
		       
let assert_cond msg = function
  | true  -> Deferred.return (Ok ())
  | false -> begin
	     printf "Error: %s\n" msg;
	     Deferred.return (Error `Assert_failed)
	   end

let tests = [ ("todo"           , todo_test)]
	      
let execute_test t =
  let with_cache () =
    StringCache.with_cache
      ~host:Sys.argv.(1)
		       ~port:(Int.of_string Sys.argv.(2))
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
		
