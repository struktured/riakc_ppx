
module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end


module Quorum : sig
  type t =
    | One [@key 1]
    | All [@key 2]
    | Default [@key 3]
    | Quorum [@key 4]
    | N [@key 5] of int [@key 6] [@deriving Protobouf]

  val to_int32 : t -> int
  val of_int32 : Core.Std.Int32.t -> t
end

module Get : functor(Key:Key) ->
sig
  type error =[ `Bad_conn | `Notfound | Response.error ]

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
             ; basic_quorum : bool option [@key 5]
             ; notfound_ok : bool option [@key 6]
             ; if_modified   : string option [@key 7]
             ; head : bool option [@key 8]
             ; deletedvclock : bool option [@key 9]
  } [@@deriving Protobuf]

  val get_of_opts : t list -> b:string -> k:Key.t -> get

end

module Put : functor (Key:Key) (Value:Value) ->
sig
  type error = [ `Bad_conn | Response.error ] 
  (*cmodule type Content = Robj.Content(Key) (Value) *)
 (* module type Robj = Robj.Make (Key) (Value) *)
  type t =
    | Timeout [@key 1] of int 
    | W       [@key 2] of Quorum.t 
    | Dw      [@key 3] of Quorum.t 
    | Pw      [@key 4] of Quorum.t 
    | Return_body [@key 5]
    | If_not_modified [@key 6]
    | If_none_match [@key 7]
    | Return_head [@key 8] [@@deriving Protobuf]

  type put = { bucket          : string [@key 1]
             ; key             : Key.t option [@key 2]
             ; vclock          : string option [@key 3]
             ; content         : Robj.Content(Key)(Value).t [@key 4]
             ; w               : int option [@key 5]
             ; dw              : int option [@key 7]
             ; pw              : int option [@key  8]
             ; return_body     : bool option [@key 9]
             ; if_not_modified : bool option [@key 10]
             ; if_none_match   : bool option [@key 11]
             ; return_head     : bool option [@key 12]
  } [@@deriving Protobuf]

  val put_of_opts : t list -> b:string -> k:Key.t option -> [ `No_siblings ] Robj.Make(Key)(Value).t -> put
end

module Delete : functor (Key:Key) ->
sig
  type error = [ `Bad_conn | Response.error ]

  type t =
    | Timeout [@key 1] of int
    | Rw      [@key 2] of Quorum.t
    | R       [@key 3]  of Quorum.t
    | W       [@key 4] of Quorum.t
    | Pr      [@key 5] of Quorum.t
    | Pw      [@key 6] of Quorum.t
    | Dw      [@key 7] of Quorum.t [@@deriving Protobuf]

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

  val delete_of_opts : t list -> b:string -> k:Key.t  -> delete
end

module Index_search : sig
  type error = [ `Bad_conn | Response.error ]

  module Query : sig
    type 'a range = { min          : 'a [@key 1]
                    ; max          : 'a [@key 2]
                    ; return_terms : bool [@key 3]
    } [@@deriving Protobuf]

    type t =
      | Eq_string    [@key 1] of string
      | Eq_int       [@ley 2] of int
      | Range_string [@key 3] of string range
      | Range_int    [@key 4] of int range [@@deriving Protobuf]

    val eq_string    : string -> t
    val eq_int       : int -> t
    val range_string : min:string -> max:string -> return_terms:bool -> t
    val range_int    : min:int    -> max:int    -> return_terms:bool -> t
    include Protobuf_capable.S with type t := t
  end

  module Kontinuation : sig
    type t [@@deriving Protobuf]

    val of_string : string -> t
    val to_string : t -> string
    include Protobuf_capable.S with type t := t
  end

  type t =
    | Timeout      of int
    | Max_results  of int
    | Continuation of Kontinuation.t

  type index_search = { bucket       : string [@key 1]
                      ; index        : string [@key 2]
                      ; query_type   : Query.t [@key 3]
                      ; max_results  : int option [@key 4]
                      ; continuation : Kontinuation.t option [@key 5]
                      ; timeout      : int option [@key 6]
  } [@@deriving Protobuf]

  val index_search_of_opts :
    t list ->
    b:string ->
    index:string ->
    query_type:Query.t ->
    index_search
end
