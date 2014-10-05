open Core.Std

type 'a t

module Usermeta : sig
  type ('k, 'v) t

  val create    : k:'k -> v:'v option -> t
  val key       : t -> 'k
  val value     : t -> 'v option
  val set_key   : 'k -> t -> t
  val set_value : 'v option -> t -> t
end

module Index : sig
  type idx = | String  of string
	     | Integer of int
	     | Bad_int of string
	     | Unknown of string

  type 'a t

  val create    : k:'a -> v:idx -> t
  val key       : t -> 'a
  val value     : t -> idx
  val set_key   : 'a -> t -> t
  val set_value : idx -> t -> t
end

module Link : sig
  type 'a t

  val bucket : t -> string option
  val key    : t -> 'a option
  val tag    : t -> string option

  val set_bucket : string option -> t -> t
  val set_key    : 'a option -> t -> t
  val set_tag    : string option -> t -> t
end

module Content : sig
  type 'v t = { 
          value                : 'v [@key 1];
          content_type         : string option  [@key 2];
          charset              : string option  [@key 3];
          content_encoding     : string option  [@key 4];
          vtag                 : string option  [@key 5];
          links                : Link.t list    [@key 6];
          last_mod             : Int32.t option [@key 7];
          last_mod_usec        : Int32.t option [@key 8];
          usermeta             : Usermeta.t list [@key 9];
          indices              : Index.t list [@key 10];
          deleted              : bool [@key 11]
  }

  val create               : string -> 'v t
  val set_value            : 'v -> 'v t -> 'v t
  val set_content_type     : string option -> 'v t -> 'v t
  val set_charset          : string option -> 'v t -> 'v t
  val set_content_encoding : string option -> 'v t -> 'v t
  val set_vtag             : string option -> 'v t -> 'v t
  val set_links            : Link.t list -> 'v t -> 'v t
  val set_last_mod         : Int32.t option -> 'v t -> 'v t
  val set_last_mod_usec    : Int32.t option -> 'v t -> 'v t
  val set_usermeta         : Usermeta.t list -> 'v t -> 'v t
  val set_indices          : Index.t list -> 'v t -> 'v t
  val to_pb : 'v t -> Pb_robj.Content.t
  val of_pb : Pb_robj.Content.t -> 'v t
end

val of_pb :
  Pb_robj.Content.t list ->
  string option ->
  bool option ->
  [ `Maybe_siblings ] 'v t

val to_pb : 'a t -> (Pb_robj.Content.t list * string option)

val create       : Content.t -> [ `No_siblings ] t
val contents     : 'a t -> Content.t list
val content      : [ `No_siblings ] t -> Content.t
val vclock       : 'a t -> string option
val unchanged    : 'a t -> bool

val set_contents : Content.t list -> 'a t -> [ `Maybe_siblings ] t
val set_content  : Content.t -> 'a t -> [ `No_siblings ] t
val set_vclock   : string option -> 'a t -> 'a t
