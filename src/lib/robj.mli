
module Link : 
sig 
  type t = { bucket : bytes option 
           ; key    : bytes option 
           ; tag    : bytes option 
           }

  val bucket : t -> bytes option
  val key : t -> bytes option
  val tag : t -> bytes option
  
  include Protobuf_capable.S with type t := t
end

module Pair : 
sig
  type t = { key : bytes
           ; value : bytes option 
  }

  val create : k:bytes -> v:bytes option -> t

  val key : t ->  bytes
  val value : t -> bytes option

  val set_key : bytes -> t -> t
  val set_value: bytes option -> t -> t

  include Protobuf_capable.S with type t := t
end

module Usermeta : module type of Pair
module Index : module type of Pair

module Content : 
sig

  type t = { value            : bytes 
  ; content_type     : bytes option 
  ; charset          : bytes option 
  ; content_encoding : bytes option 
  ; vtag             : bytes option 
  ; links            : Link.t list 
  ; last_mod         : Int32.t option 
  ; last_mod_usec    : Int32.t option 
  ; usermeta         : Usermeta.t list 
  ; indices          : Index.t list 
  ; deleted          : bool option
  } 
  include Protobuf_capable.S with type t := t
  val create               : bytes -> t

  val value                : t -> bytes
  val content_type         : t -> bytes option
  val charset              : t -> bytes option
  val content_encoding     : t -> bytes option
  val vtag                 : t -> bytes option
  val links                : t -> Link.t list
  val last_mod             : t -> Int32.t option
  val last_mod_usec        : t -> Int32.t option
  val usermeta             : t -> Usermeta.t list
  val indices              : t -> Index.t list 
  val deleted              : t -> bool

  val set_value            : bytes -> t -> t
  val set_content_type     : bytes option -> t -> t
  val set_charset          : bytes option -> t -> t
  val set_content_encoding : bytes option -> t -> t
  val set_vtag             : bytes option -> t -> t
  val set_links            : Link.t list -> t -> t
  val set_last_mod         : Int32.t option -> t -> t
  val set_last_mod_usec    : Int32.t option -> t -> t
  val set_usermeta         : Usermeta.t list -> t -> t
  val set_indices          : Index.t list -> t -> t
  include Protobuf_capable.S with type t := t
end

  type 'a t
  val of_pb :
    Content.t list ->
    bytes option ->
    bool option ->
    [ `Maybe_siblings ] t
  
  val create       : Content.t -> [ `No_siblings ] t
  val contents     : 'a t -> Content.t list
  val content      : [ `No_siblings ] t -> Content.t
  val vclock       : 'a t -> bytes option
  val unchanged    : 'a t -> bool

  val set_contents : Content.t list -> 'a t -> [ `Maybe_siblings ] t
  val set_content  : Content.t -> 'a t -> [ `No_siblings ] t
  val set_vclock   : bytes option -> 'a t -> 'a t 
