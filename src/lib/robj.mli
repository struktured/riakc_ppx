
module Link : 
sig 
  type t = { bucket : string option 
           ; key    : string option 
           ; tag    : string option 
           }

  val bucket : t -> string option
  val key : t -> string option
  val tag : t -> string option
  
  include Protobuf_capable.S with type t := t
end

module Pair : 
sig
  type t = { key : string
           ; value : string option 
  }

  val create : k:string -> v:string option -> t

  val key : t ->  string
  val value : t -> string option

  val set_key : string -> t -> t
  val set_value: string option -> t -> t

  include Protobuf_capable.S with type t := t
end

module Usermeta : module type of Pair
module Index : module type of Pair

module Content : 
sig

  type t = { value            : string 
  ; content_type     : string option 
  ; charset          : string option 
  ; content_encoding : string option 
  ; vtag             : string option 
  ; links            : Link.t list 
  ; last_mod         : Int32.t option 
  ; last_mod_usec    : Int32.t option 
  ; usermeta         : Usermeta.t list 
  ; indices          : Index.t list 
  ; deleted          : bool option
  } 
  include Protobuf_capable.S with type t := t
  val create               : string -> t

  val value                : t -> string
  val content_type         : t -> string option
  val charset              : t -> string option
  val content_encoding     : t -> string option
  val vtag                 : t -> string option
  val links                : t -> Link.t list
  val last_mod             : t -> Int32.t option
  val last_mod_usec        : t -> Int32.t option
  val usermeta             : t -> Usermeta.t list
  val indices              : t -> Index.t list 
  val deleted              : t -> bool

  val set_value            : string -> t -> t
  val set_content_type     : string option -> t -> t
  val set_charset          : string option -> t -> t
  val set_content_encoding : string option -> t -> t
  val set_vtag             : string option -> t -> t
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
    string option ->
    bool option ->
    [ `Maybe_siblings ] t
  
  val create       : Content.t -> [ `No_siblings ] t
  val contents     : 'a t -> Content.t list
  val content      : [ `No_siblings ] t -> Content.t
  val vclock       : 'a t -> string option
  val unchanged    : 'a t -> bool

  val set_contents : Content.t list -> 'a t -> [ `Maybe_siblings ] t
  val set_content  : Content.t -> 'a t -> [ `No_siblings ] t
  val set_vclock   : string option -> 'a t -> 'a t 
