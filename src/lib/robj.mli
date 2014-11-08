module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end


module Link : functor(Key:Key) -> 
sig 
  type t = { bucket : string option 
           ; key    : Key.t option 
           ; tag    : string option 
           }
include Protobuf_capable.S with type t := t
end

module Pair : functor(Key:Key) (Value:Value) ->
sig
  type t = { key: Key.t  
  ; value : Value.t option 
  } 
  include Protobuf_capable.S with type t := t
end


module Usermeta : functor(Key:Key) (Value:Value) ->
sig
  type t = { key : Key.t 
           ; value : Value.t option 
  } [@@deriving protobuf]

  val create : k:Key.t -> v:Value.t option -> t

  val key : t ->  Key.t
  val value : t -> Value.t option

  val set_key : Key.t -> t -> t 
  val set_value: Value.t option -> t -> t

  include Protobuf_capable.S with type t := t
end


module Content : functor (Key:Key) (Value:Value) ->
sig
  module Link : module type of Link(Key)
  module Usermeta : module type of Usermeta(Key)(Value)
  module Pair : module type of Pair(Key)(Value)

  type t = { value            : Value.t 
  ; content_type     : string option 
  ; charset          : string option 
  ; content_encoding : string option 
  ; vtag             : string option 
  ; links            : Link.t list 
  ; last_mod         : Int32.t option 
  ; last_mod_usec    : Int32.t option 
  ; usermeta         : Usermeta.t list 
  ; indices          : Pair.t list 
  ; deleted          : bool option
  } 
  include Protobuf_capable.S with type t := t
  val create               : Value.t -> t

  val value                : t -> Value.t
  val content_type         : t -> string option
  val charset              : t -> string option
  val content_encoding     : t -> string option
  val vtag                 : t -> string option
  val links                : t -> Link.t list
  val last_mod             : t -> Int32.t option
  val last_mod_usec        : t -> Int32.t option
  val usermeta             : t -> Usermeta.t list
  val indices              : t -> Pair.t list (* TODO no idea if this is right *)
  val deleted              : t -> bool

  val set_value            : Value.t -> t -> t
  val set_content_type     : string option -> t -> t
  val set_charset          : string option -> t -> t
  val set_content_encoding : string option -> t -> t
  val set_vtag             : string option -> t -> t
  val set_links            : Link.t list -> t -> t
  val set_last_mod         : Int32.t option -> t -> t
  val set_last_mod_usec    : Int32.t option -> t -> t
  val set_usermeta         : Usermeta.t list -> t -> t
  val set_indices          : Pair.t list -> t -> t
  include Protobuf_capable.S with type t := t
end

module Make : functor (Key:Key) (Value:Value) ->
sig
  module Content : module type of Content(Key)(Value)
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
end
