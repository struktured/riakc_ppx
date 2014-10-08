

module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end


module Quorum = struct
  type t =
    | One [@key 1]
    | All [@key 2]
    | Default [@key 3]
    | Quorum [@key 4]
    | N [@key 5] of int [@@deriving Protobuf] 

  let one     = Core.Std.Int32.of_int_exn (-2)
  let quorum  = Core.Std.Int32.of_int_exn (-3)
  let all     = Core.Std.Int32.of_int_exn (-4)
  let default = Core.Std.Int32.of_int_exn (-5)
  
  let to_int32 = let conv x = 
      match Core.Std.Int32.to_int x with Some n -> n | None -> raise (Invalid_argument "x") in
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

  let from_protobuf = t_from_protobuf
  let to_protobuf = t_to_protobuf
end

module Get(Key:Key) = struct
  type error = [ `Bad_conn | `Notfound | Response.error ]

  type t =
    | Timeout     [@key 1] of int
    | R           [@key 2] of Quorum.t
    | Pr          [@key 3] of Quorum.t
    | If_modified [@key 4] of string
    | Basic_quorum [@key 5]
    | Notfound_ok [@key 6]
    | Head [@key 7]
    | Deletedvclock [@key 8] [@@deriving Protobuf]

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

  let get_of_opts opts ~b ~k =
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
    List.fold_left
      ~f:(fun g -> function
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
	  { g with deletedvclock = Some true })
      ~init:g
      opts
end

module Put(Key:Key) (Value:Value) = struct
  type error = [ `Bad_conn | Response.error ]

  type t =
    | Timeout of int
    | W       of Quorum.t
    | Dw      of Quorum.t
    | Pw      of Quorum.t
    | Return_body
    | If_not_modified
    | If_none_match
    | Return_head

  type put = { bucket          : string
	     ; key             : Key.t option
	     ; vclock          : string option
	     ; content         : Robj.Content(Key)(Value).t
	     ; w               : int option
	     ; dw              : int option
	     ; pw              : int option
	     ; return_body     : bool
	     ; if_not_modified : bool
	     ; if_none_match   : bool
	     ; return_head     : bool
	     }

  module Robj = Robj.Make(Key)(Value)
  let put_of_opts opts ~b ~k robj =
    let p = { bucket          = b
	    ; key             = k
	    ; vclock          = Robj.vclock robj
	    ; content         = Robj.content robj
	    ; w               = None
	    ; dw              = None
	    ; pw              = None
	    ; return_body     = false
	    ; if_not_modified = false
	    ; if_none_match   = false
	    ; return_head     = false
	    }
    in
    List.fold_left
      ~f:(fun p -> function
	| Timeout _ ->
	  p
	| W n ->
	  { p with w = Some (Quorum.to_int32 n) }
	| Dw n ->
	  { p with dw = Some (Quorum.to_int32 n) }
	| Pw n ->
	  { p with pw = Some (Quorum.to_int32 n) }
	| Return_body ->
	  { p with return_body = true }
	| If_not_modified ->
	  { p with if_not_modified = true }
	| If_none_match ->
	  { p with if_none_match = true }
	| Return_head ->
	  { p with return_head = true })
      ~init:p
      opts
end

module Delete = struct
  type error = [ `Bad_conn | Response.error ]

  type t =
    | Timeout of int
    | Rw      of Quorum.t
    | R       of Quorum.t
    | W       of Quorum.t
    | Pr      of Quorum.t
    | Pw      of Quorum.t
    | Dw      of Quorum.t

  type delete = { bucket : string
		; key    : string
		; rw     : int option
		; vclock : string option
		; r      : int option
		; w      : int option
		; pr     : int option
		; pw     : int option
		; dw     : int option
		}

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
    List.fold_left
      ~f:(fun d -> function
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
	  { d with dw = Some (Quorum.to_int32 n) })
      ~init:d
      opts
end

module Index_search = struct
  type error = [ `Bad_conn | Response.error ]

  module Make_query = struct
    type 'a range = { min          : 'a [@key 1]
                    ; max          : 'a [@key 2]
                    ; return_terms : bool [@key 3]
    } [@@deriving Protobuf]


    let string_from_protobuf = Protobuf.Decoder.bytes
    let string_to_protobuf = Protobuf.Encoder.bytes
    let int_from_protobuf d = 0
    let int_to_protobuf i e = ()


    type t =
      | Eq_string    [@key 1] of string
      | Eq_int       [@key 2] of int 
      | Range_string [@key 3] of string range 
      | Range_int    [@key 4] of int range [@@deriving Protobuf]

    let eq_string key =
      Eq_string key

    let eq_int key =
      Eq_int key

    let range_string ~min ~max ~return_terms =
      Range_string { min; max; return_terms }

    let range_int ~min ~max ~return_terms =
      Range_int { min; max; return_terms }
  end

  module Query = Protobuf_capable.Make(Make_query)

  module Kontinuation = struct
    type t = string [@@deriving Protobuf]

    let of_string s = s
    let to_string t = t
    let from_protobuf = t_from_protobuf
    let to_protobuf = t_to_protobuf
  end

  type t =
    | Timeout      [@key 1] of int 
    | Max_results  [@key 2] of int
    | Continuation [@key 3] of Kontinuation.t [@@deriving Protobuf]

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
    List.fold_left
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
