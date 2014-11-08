
module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end


module Quorum : sig
  type t =
    | One 
    | All 
    | Default 
    | Quorum 
    | N of int 
  include Protobuf_capable.S with type t := t
  val to_int32 : t -> int
  val of_int32 : Core.Std.Int32.t -> t 
end

module Get : functor(Key:Key) ->
sig
  type error =[ `Bad_conn | `Notfound | Response.error ] 

  type t =
    | Timeout      of int 
    | R           of Quorum.t 
    | Pr          of Quorum.t 
    | If_modified of string 
    | Basic_quorum 
    | Notfound_ok 
    | Head
    | Deletedvclock 

  type get = { bucket        : string 
             ; key           : Key.t 
             ; r             : int option 
             ; pr            : int option
             ; basic_quorum : bool option
             ; notfound_ok : bool option
             ; if_modified   : string option 
             ; head : bool option 
             ; deletedvclock : bool option 
  } 

  val get_from_protobuf : Protobuf.Decoder.t -> get
  val get_to_protobuf : get -> Protobuf.Encoder.t -> unit


  val get_of_opts : t list -> b:string -> k:Key.t -> get

end

module Put : functor (Key:Key) (Value:Value) ->
sig
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
             ; return_body     : bool option 
             ; if_not_modified : bool option 
             ; if_none_match   : bool option 
             ; return_head     : bool option 
  } 

  val put_from_protobuf : Protobuf.Decoder.t -> put
  val put_to_protobuf : put -> Protobuf.Encoder.t -> unit

  val put_of_opts : t list -> b:string -> k:Key.t option -> [ `No_siblings ] Robj.Make(Key)(Value).t -> put
end

module Delete : functor (Key:Key) ->
sig
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
                ; key    : Key.t 
                ; rw     : int option 
                ; vclock : string option 
                ; r      : int option 
                ; w      : int option 
                ; pr     : int option 
                ; pw     : int option 
                ; dw     : int option 
  } 

  val delete_from_protobuf : Protobuf.Decoder.t -> delete
  val delete_to_protobuf : delete -> Protobuf.Encoder.t -> unit

  val delete_of_opts : t list -> b:string -> k:Key.t  -> delete
end

module Index_search : sig
  type error = [ `Bad_conn | Response.error ]

  module Query : sig
    type 'a range = { min          : 'a 
                    ; max          : 'a 
                    ; return_terms : bool 
    }

    type string_range = { min          : string 
                    ; max          : string 
                    ; return_terms : bool 
    } 

   type int_range = { min          : int 
                    ; max          : int 
                   ; return_terms : bool 
    } 

  val string_range_from_protobuf : Protobuf.Decoder.t -> string_range
  val string_range_to_protobuf : string_range -> Protobuf.Encoder.t -> unit

  val int_range_from_protobuf : Protobuf.Decoder.t -> int_range
  val int_range_to_protobuf : int_range -> Protobuf.Encoder.t -> unit




    type t =
      | Eq_string    of string
      | Eq_int       of int
      | Range_string of string_range
      | Range_int    of int_range 

    val eq_string    : string -> t
    val eq_int       : int -> t
    val range_string : min:string -> max:string -> return_terms:bool -> t
    val range_int    : min:int    -> max:int    -> return_terms:bool -> t
    include Protobuf_capable.S with type t := t
  end

  module Kontinuation : sig
    type t 

    val of_string : string -> t
    val to_string : t -> string
    include Protobuf_capable.S with type t := t
  end

  type t =
    | Timeout      of int
    | Max_results  of int
    | Continuation of Kontinuation.t

  type index_search = { bucket       : string 
                      ; index        : string 
                      ; query_type   : Query.t 
                      ; max_results  : int option 
                      ; continuation : Kontinuation.t option 
                      ; timeout      : int option 
  }
 
 val index_search_from_protobuf : Protobuf.Decoder.t -> index_search
 val index_search_to_protobuf : index_search -> Protobuf.Encoder.t -> unit


  val index_search_of_opts :
    t list ->
    b:string ->
    index:string ->
    query_type:Query.t ->
    index_search
end
