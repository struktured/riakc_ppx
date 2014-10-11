
module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end


module Quorum = struct
  type t =
    | One [@key 1]
    | All [@key 2]
    | Default [@key 3]
    | Quorum [@key 4]
    | N [@key 5] of int [@@deriving Protobuf] 
(*
  let one     = Core.Std.Int32.of_int_exn (-2)
  let quorum  = Core.Std.Int32.of_int_exn (-3)
  let all     = Core.Std.Int32.of_int_exn (-4)
  let default = Core.Std.Int32.of_int_exn (-5)
 
  let to_int32 =  
    let conv x = 
      match Core.Std.Int32.to_int x with Some n -> n | None -> raise (Invalid_argument (Core.Std.Int32.to_string x)) in
   function
    | N n when n > 1000 ->
      (* Arbitrary and cheap, but n should always be small *)
      failwith "to_int32 - n too large"
    | N n ->
      (*Core.Std.Int32.of_int_exn *) n
    | One ->
      conv one
    | All ->
      conv all
    | Default ->
      conv default
    | Quorum ->
      conv quorum

  let of_int32 = function
    | n when Core.Std.Int32.equal n one ->
      One
    | n when Core.Std.Int32.equal n all ->
      All
    | n when Core.Std.Int32.equal n default ->
      Default
    | n when Core.Std.Int32.equal n quorum ->
      Quorum
    | n -> begin
      match Core.Std.Int32.to_int n with
	| Some n ->
	  N n
	| None ->
	  failwith "of_int32 - n too large"
    end
*)
end [@@deriving Protobuf]

module Get(Key:Key) = struct
  type error = [ `Bad_conn | `Notfound | Response.error ]

  type t =
    | Timeout     of int
    | R           of Quorum.t
    | Pr          of Quorum.t
    | If_modified of string
    | Basic_quorum 
    | Notfound_ok 
    | Head
    | Deletedvclock

  type get = { bucket        : string [@key 1]
             ; key           : Key.t [@key 2]
             ; r             : int option [@key 3]
             ; pr            : int option [@key 4]
             ; basic_quorum  : bool option [@key 5]
             ; notfound_ok   : bool option [@key 6]
             ; if_modified   : string option [@key 7]
             ; head          : bool option [@key 8]
             ; deletedvclock : bool option [@key 9]
  } [@@deriving Protobuf]

  let get_of_opts (opts:t list) ~b ~k =
    let g = { bucket        = b
	    ; key           = k
	    ; r             = None
	    ; pr            = None
	    ; basic_quorum  = None
	    ; notfound_ok   = None
	    ; if_modified   = None
	    ; head          = None
	    ; deletedvclock = None
	    }
    in
    Core.Std.List.fold_left
      ~f:(fun g -> function _ -> g) (*function
	| Timeout _ ->
	  g
	| R n ->
	  { g with r = Some (Quorum.to_int32 n) }
	| Pr n ->
	  { g with pr = Some (Quorum.to_int32 n) }
	| If_modified s ->
	  { g with if_modified = Some s }
	| Basic_quorum ->
	  { g with basic_quorum = Some true }
	| Notfound_ok ->
	  { g with notfound_ok = Some true }
	| Head ->
	  { g with head = Some true }
	| Deletedvclock ->
	  { g with deletedvclock = Some true }) *)
      ~init:g
      opts
end

module Put(Key:Key) (Value:Value) = struct
  type error = [ `Bad_conn | Response.error ]

  type t =
    | Timeout  of int
    | W      of Quorum.t
    | Dw     of Quorum.t
    | Pw     of Quorum.t
    | Return_body 
    | If_not_modified 
    | If_none_match  
    | Return_head 

  type put = { bucket          : string [@key 1]
             ; key             : Key.t option [@key 2]
             ; vclock          : string option [@key 3]
             ; content         : Robj.Content(Key)(Value).t [@key 4]
             ; w               : int option [@key 5]
             ; dw              : int option [@key 6]
             ; pw              : int option [@key 7]
             ; return_body     : bool option [@key 8]
             ; if_not_modified : bool option [@key 9]
             ; if_none_match   : bool option [@key 10]
             ; return_head     : bool option [@key 11]
  } [@@deriving Protobuf]

  module Robj = Robj.Make(Key)(Value)
  let put_of_opts opts ~b ~k robj =
    let p = { bucket          = b
	    ; key             = k
	    ; vclock          = Robj.vclock robj
	    ; content         = Robj.content robj
	    ; w               = None
	    ; dw              = None
	    ; pw              = None
	    ; return_body     = None
	    ; if_not_modified = None
	    ; if_none_match   = None
	    ; return_head     = None
	    }
    in
    Core.Std.List.fold_left
      ~f:(fun p -> function _ -> p) (*
	| Timeout _ ->
	  p
	| W n ->
	  { p with w = Some (Quorum.to_int32 n) }
	| Dw n ->
	  { p with dw = Some (Quorum.to_int32 n) }
	| Pw n ->
	  { p with pw = Some (Quorum.to_int32 n) }
	| Return_body ->
	  { p with return_body = Some true }
	| If_not_modified ->
	  { p with if_not_modified = Some true }
	| If_none_match ->
	  { p with if_none_match = Some true }
	| Return_head ->
	  { p with return_head = Some true }) *)
      ~init:p
      opts
end

module Delete(Key:Key) = struct
  type error = [ `Bad_conn | Response.error ]

  type t =
    | Timeout  of int
    | Rw      of Quorum.t
    | R       of Quorum.t
    | W       of Quorum.t
    | Pr      of Quorum.t
    | Pw      of Quorum.t
    | Dw      of Quorum.t 

  type delete = { bucket : string [@key 1]
                ; key    : Key.t [@key 2]
                ; rw     : int option [@key 3]
                ; vclock : string option [@key 4]
                ; r      : int option [@key 5]
                ; w      : int option [@key 6]
                ; pr     : int option [@key 7]
                ; pw     : int option [@key 8]
                ; dw     : int option [@key 9]
  } [@@deriving Protobuf]

  let delete_of_opts opts ~b ~k =
    let d = { bucket = b
	    ; key    = k
	    ; rw     = None
	    ; vclock = None
	    ; r      = None
	    ; w      = None
	    ; pr     = None
	    ; pw     = None
	    ; dw     = None
	    }
    in
    Core.Std.List.fold_left
      ~f:(fun d -> function _ -> d) (*
	| Timeout _ ->
	  d
	| Rw n ->
	  { d with rw = Some (Quorum.to_int32 n) }
	| R n ->
	  { d with w = Some (Quorum.to_int32 n) }
	| W n ->
	  { d with dw = Some (Quorum.to_int32 n) }
	| Pr n ->
	  { d with pr = Some (Quorum.to_int32 n) }
	| Pw n ->
	  { d with pw = Some (Quorum.to_int32 n) }
	| Dw n ->
	  { d with dw = Some (Quorum.to_int32 n) }) *)
      ~init:d
      opts
end

module Index_search = struct
  type error = [ `Bad_conn | Response.error ]

  module Query = struct
    type 'a range = { min          : 'a 
                    ; max          : 'a 
                    ; return_terms : bool 
    } 

    type string_range = { min          : string [@key 1]
                    ; max          : string [@key 2]
                    ; return_terms : bool [@key 3]
    } [@@deriving Protobuf]

    type int_range = { min          : int [@key 1]
                    ; max          : int [@key 2]
                    ; return_terms : bool [@key 3]
    } [@@deriving Protobuf]



(*
    let string_from_protobuf = Protobuf.Decoder.bytes
    let string_to_protobuf = Protobuf.Encoder.bytes
    let int_from_protobuf d = 0
    let int_to_protobuf i e = ()
*)

    type t =
      | Eq_string    [@key 1] of string
      | Eq_int       [@key 2] of int 
      | Range_string [@key 3] of string_range 
      | Range_int    [@key 4] of int_range [@@deriving Protobuf]

    let eq_string key =
      Eq_string key

    let eq_int key =
      Eq_int key

    let range_string ~min ~max ~return_terms =
      Range_string { min; max; return_terms }

    let range_int ~min ~max ~return_terms =
      Range_int { min; max; return_terms }
  end


  module Kontinuation = struct
    type t = string [@@deriving Protobuf]

    let of_string s = s
    let to_string t = t
  end

  type t =
    | Timeout       of int 
    | Max_results   of int
    | Continuation  of Kontinuation.t 

  type index_search = { bucket       : string [@key 1]
                      ; index        : string [@key 2]
                      ; query_type   : Query.t [@key 3]
                      ; max_results  : int option [@key 4]
                      ; continuation : Kontinuation.t option [@key 5]
                      ; timeout      : int option [@key 6]
  } [@@deriving Protobuf]

  let index_search_of_opts opts ~b ~index ~query_type =
    let idx_s = { bucket       = b
		; index        = index
		; query_type   = query_type
		; max_results  = None
		; continuation = None
		; timeout      = None
		}
    in
    Core.Std.List.fold_left
      ~f:(fun idx_s -> function
	| Timeout _ ->
	  idx_s
	| Max_results n ->
	  { idx_s with max_results = Some n }
	| Continuation k ->
	  { idx_s with continuation = Some k })
      ~init:idx_s
      opts

end
