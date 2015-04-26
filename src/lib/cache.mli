module Result = Core.Std.Result
module Option = Core.Std.Option
module Deferred = Async.Std.Deferred
module String = Protobuf_capables.String
module Int = Protobuf_capables.Int
module Default_usermeta = String
module Default_index : sig
  type t =
      String of string
    | Integer of int
    | Bad_int of string
    | Unknown of string
  val from_protobuf : Protobuf.Decoder.t -> t
  val to_protobuf : t -> Protobuf.Encoder.t -> unit
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module type S =
  sig
    module Key : Protobuf_capable.S
    module Value : Protobuf_capable.S
    module Usermeta_value : Protobuf_capable.S
    module Index_value : Protobuf_capable.S

    type conn = Conn.t
    type t = { conn : conn; bucket : string; }
    val get_conn : t -> conn
    val get_bucket : t -> string
    module Unsafe_Robj : module type of Robj
    module Robj :
    sig
      module Link :
      sig
        type t = {
          bucket : string option;
          key : Key.t option;
          tag : string option;
        }
        val bucket : t -> string option
        val key : t -> Key.t option
        val tag : t -> string option
        val set_bucket : string option -> t -> t
        val set_key : Key.t option -> t -> t
        val set_tag : string option -> t -> t
        val to_unsafe : t -> Unsafe_Robj.Link.t
        val from_unsafe : Unsafe_Robj.Link.t -> t
      end
      module type Unsafe_Pair =
        sig
          type t = { key : string; value : string option; }
          val value : t -> string option
          val key : t -> string
        end
      module Pair :
      functor (Unsafe : Unsafe_Pair) (V : Protobuf_capable.S) ->
              sig
                type t = { key : Key.t; value : V.t option; }
                val create : k:Key.t -> v:V.t option -> t
                val key : t -> Key.t
                val value : t -> V.t option
                val set_key : Key.t -> t -> t
                val set_value : V.t option -> t -> t
                val to_unsafe : t -> Unsafe.t
                val from_unsafe : Unsafe.t -> t
              end
      module Usermeta :
      sig
        type t =
            Pair(Unsafe_Robj.Usermeta)(Usermeta_value).t = {
            key : Key.t;
            value : Usermeta_value.t option;
        }
        val create : k:Key.t -> v:Usermeta_value.t option -> t
        val key : t -> Key.t
        val value : t -> Usermeta_value.t option
        val set_key : Key.t -> t -> t
        val set_value : Usermeta_value.t option -> t -> t
        val to_unsafe : t -> Unsafe_Robj.Usermeta.t
        val from_unsafe : Unsafe_Robj.Usermeta.t -> t
      end
      module Index :
      sig
        type t =
            Pair(Unsafe_Robj.Index)(Index_value).t = {
            key : Key.t;
            value : Index_value.t option;
          }
        val create : k:Key.t -> v:Index_value.t option -> t
        val key : t -> Key.t
        val value : t -> Index_value.t option
        val set_key : Key.t -> t -> t
        val set_value : Index_value.t option -> t -> t
        val to_unsafe : t -> Unsafe_Robj.Index.t
        val from_unsafe : Unsafe_Robj.Index.t -> t
      end
      module Content :
      sig
        type t = {
          value : Value.t;
          content_type : string option;
          charset : string option;
          content_encoding : string option;
          vtag : string option;
          links : Link.t list;
          last_mod : Int32.t option;
          last_mod_usec : Int32.t option;
          usermeta : Usermeta.t list;
          indices : Index.t list;
          deleted : bool option;
        }
        val value : t -> Value.t
        val content_type : t -> string option
        val charset : t -> string option
        val content_encoding : t -> string option
        val vtag : t -> string option
        val links : t -> Link.t list
        val last_mod : t -> Int32.t option
        val last_mod_usec : t -> Int32.t option
        val usermeta : t -> Usermeta.t list
        val indices : t -> Index.t list
        val deleted : t -> bool
        val create : Value.t -> t
        val to_unsafe : t -> Unsafe_Robj.Content.t
        val from_unsafe : Unsafe_Robj.Content.t -> t
        val set_value : Value.t -> t -> t
        val set_content_type : string option -> t -> t
        val set_charset : string option -> t -> t
        val set_content_encoding : string option -> t -> t
        val set_vtag : string option -> t -> t
        val set_links : Link.t list -> t -> t
        val set_last_mod : Int32.t option -> t -> t
        val set_last_mod_usec : Int32.t option -> t -> t
        val set_usermeta : Usermeta.t list -> t -> t
        val set_indices : Index.t list -> t -> t
      end
      type 'a t = {
        contents : Content.t list;
        vclock : string option;
        unchanged : bool;
      }
      val create : Content.t -> 'a t
      val of_value : Value.t -> 'a t
      val create_siblings : Content.t list -> 'a t
      val contents : 'a t -> Content.t list
      val content : 'a t -> Content.t
      val vclock : 'a t -> string option
      val unchanged : 'a t -> bool
      val set_contents : Content.t list -> 'a t -> 'b t
      val set_content : Content.t -> 'a t -> 'b t
      val set_vclock : string option -> 'a t -> 'b t
      val to_unsafe : 'a t -> [ `No_siblings ] Unsafe_Robj.t
      val from_unsafe : 'a Unsafe_Robj.t -> 'b t
    end
    val create : conn:conn -> bucket:string -> t
    val list_keys_stream :
      t ->
      (Key.t list -> unit Deferred.t) ->
      (unit, [> Conn.error | Response.error]) Deferred.Result.t
    val with_cache :
      host:string ->
      port:int ->
      bucket:string ->
      (t -> ('a, [> Conn.error ] as 'e) Deferred.Result.t) ->
      ('a, 'e) Deferred.Result.t
    val list_keys :
      t ->
      (Key.t list,
       [> Conn.error | Response.error ])
        Result.t Async_kernel.Deferred.t
    val get :
      t ->
      ?opts:Opts.Get.t list ->
      Key.t ->
      ('a Robj.t, [> Opts.Get.error ]) Result.t Deferred.t
    val put :
      t ->
      ?opts:Opts.Put.t list ->
      ?k:Key.t ->
      'a Robj.t ->
      ('b Robj.t * Key.t Option.t, [> Opts.Put.error ]) Result.t
        Deferred.t
    val delete :
      t ->
      ?opts:Opts.Delete.t list ->
      Key.t ->
      (unit, [> Opts.Delete.error ]) Result.t Deferred.t
    val index_search :
      t ->
      ?opts:Opts.Index_search.t list ->
      index:Index_value.t ->
      Opts.Index_search.Query.t ->
      (Response.Index_search.t, [> Opts.Index_search.error ]) Result.t
        Deferred.t
    val bucket_props : t -> (Response.Props.t, [> Conn.error | Response.error ]) Deferred.Result.t
  end

module type S2 =
  sig
    module Key : Protobuf_capable.PBS2
    module Value : Protobuf_capable.S
    module Usermeta_value : Protobuf_capable.S
    module Index_value : Protobuf_capable.S

    type conn = Conn.t
    type t = { conn : conn; bucket : string; }
    val get_conn : t -> conn
    val get_bucket : t -> string
    module Unsafe_Robj : module type of Robj
    module Robj :
    sig
      module Link :
      sig
        type t = {
          bucket : string option;
          key : Key.t option;
          tag : string option;
        }
        val bucket : t -> string option
        val key : t -> Key.t option
        val tag : t -> string option
        val set_bucket : string option -> t -> t
        val set_key : Key.t option -> t -> t
        val set_tag : string option -> t -> t
        val to_unsafe : t -> Unsafe_Robj.Link.t
        val from_unsafe : Unsafe_Robj.Link.t -> t
      end
      module type Unsafe_Pair =
        sig
          type t = { key : string; value : string option; }
          val value : t -> string option
          val key : t -> string
        end
      module Pair :
      functor (Unsafe : Unsafe_Pair) (V : Protobuf_capable.S) ->
              sig
                type t = { key : Key.t; value : V.t option; }
                val create : k:Key.t -> v:V.t option -> t
                val key : t -> Key.t
                val value : t -> V.t option
                val set_key : Key.t -> t -> t
                val set_value : V.t option -> t -> t
                val to_unsafe : t -> Unsafe.t
                val from_unsafe : Unsafe.t -> t
              end
      module Usermeta :
      sig
        type t =
            Pair(Unsafe_Robj.Usermeta)(Usermeta_value).t = {
            key : Key.t;
            value : Usermeta_value.t option;
        }
        val create : k:Key.t -> v:Usermeta_value.t option -> t
        val key : t -> Key.t
        val value : t -> Usermeta_value.t option
        val set_key : Key.t -> t -> t
        val set_value : Usermeta_value.t option -> t -> t
        val to_unsafe : t -> Unsafe_Robj.Usermeta.t
        val from_unsafe : Unsafe_Robj.Usermeta.t -> t
      end
      module Index :
      sig
        type t =
            Pair(Unsafe_Robj.Index)(Index_value).t = {
            key : Key.t;
            value : Index_value.t option;
          }
        val create : k:Key.t -> v:Index_value.t option -> t
        val key : t -> Key.t
        val value : t -> Index_value.t option
        val set_key : Key.t -> t -> t
        val set_value : Index_value.t option -> t -> t
        val to_unsafe : t -> Unsafe_Robj.Index.t
        val from_unsafe : Unsafe_Robj.Index.t -> t
      end
      module Content :
      sig
        type t = {
          value : Value.t;
          content_type : string option;
          charset : string option;
          content_encoding : string option;
          vtag : string option;
          links : Link.t list;
          last_mod : Int32.t option;
          last_mod_usec : Int32.t option;
          usermeta : Usermeta.t list;
          indices : Index.t list;
          deleted : bool option;
        }
        val value : t -> Value.t
        val content_type : t -> string option
        val charset : t -> string option
        val content_encoding : t -> string option
        val vtag : t -> string option
        val links : t -> Link.t list
        val last_mod : t -> Int32.t option
        val last_mod_usec : t -> Int32.t option
        val usermeta : t -> Usermeta.t list
        val indices : t -> Index.t list
        val deleted : t -> bool
        val create : Value.t -> t
        val to_unsafe : t -> Unsafe_Robj.Content.t
        val from_unsafe : Unsafe_Robj.Content.t -> t
        val set_value : Value.t -> t -> t
        val set_content_type : string option -> t -> t
        val set_charset : string option -> t -> t
        val set_content_encoding : string option -> t -> t
        val set_vtag : string option -> t -> t
        val set_links : Link.t list -> t -> t
        val set_last_mod : Int32.t option -> t -> t
        val set_last_mod_usec : Int32.t option -> t -> t
        val set_usermeta : Usermeta.t list -> t -> t
        val set_indices : Index.t list -> t -> t
      end
      type 'a t = {
        contents : Content.t list;
        vclock : string option;
        unchanged : bool;
      }
      val create : Content.t -> 'a t
      val of_value : Value.t -> 'a t
      val create_siblings : Content.t list -> 'a t
      val contents : 'a t -> Content.t list
      val content : 'a t -> Content.t
      val vclock : 'a t -> string option
      val unchanged : 'a t -> bool
      val set_contents : Content.t list -> 'a t -> 'b t
      val set_content : Content.t -> 'a t -> 'b t
      val set_vclock : string option -> 'a t -> 'b t
      val to_unsafe : 'a t -> [ `No_siblings ] Unsafe_Robj.t
      val from_unsafe : 'a Unsafe_Robj.t -> 'b t
    end
    val create : conn:conn -> bucket:string -> t
    val list_keys_stream :
      t ->
      (Key.t list -> unit Deferred.t) ->
      (unit, [> Conn.error | Response.error]) Deferred.Result.t
    val with_cache :
      host:string ->
      port:int ->
      bucket:string ->
      (t -> ('a, [> Conn.error ] as 'e) Deferred.Result.t) ->
      ('a, 'e) Deferred.Result.t
    val list_keys :
      t ->
      (Key.t list,
       [> Conn.error | Response.error ])
        Result.t Async_kernel.Deferred.t
    val get :
      t ->
      ?opts:Opts.Get.t list ->
      Key.t ->
      ('a Robj.t, [> Opts.Get.error ]) Result.t Deferred.t
    val put :
      t ->
      ?opts:Opts.Put.t list ->
      ?k:Key.t ->
      'a Robj.t ->
      ('b Robj.t * Key.t Option.t, [> Opts.Put.error ]) Result.t
      Deferred.t
    val delete :
      t ->
      ?opts:Opts.Delete.t list ->
      Key.t ->
      (unit, [> Opts.Delete.error ]) Result.t Deferred.t

    val purge :
      t ->
      (unit, [> Opts.Delete.error]) Result.t Deferred.t

    val purge2 :
      conn ->
      string ->
      (unit, [> Opts.Delete.error]) Result.t Deferred.t

    val index_search :
      t ->
      ?opts:Opts.Index_search.t list ->
      index:Index_value.t ->
      Opts.Index_search.Query.t ->
      (Response.Index_search.t, [> Opts.Index_search.error ]) Result.t
          Deferred.t
    val bucket_props : t -> (Response.Props.t, [> Conn.error | Response.error ]) Deferred.Result.t
  end

module Make_with_usermeta_index :
functor
  (Key : Protobuf_capable.S) (Value : Protobuf_capable.S) (Usermeta_value : Protobuf_capable.S) (Index_value : Protobuf_capable.S) -> S
  with
    type Key.t = Key.t and 
    type Value.t = Value.t and 
    type Usermeta_value.t = Usermeta_value.t and
    type Index_value.t = Index_value.t

module Make_with_usermeta_index_primitive_key :
functor
  (Value : Protobuf_capable.S) (Usermeta_value : Protobuf_capable.S) (Index_value : Protobuf_capable.S) -> S2
  with
    type Key.t = Core.Std.String.t and
    type Value.t = Value.t and
    type Usermeta_value.t = Usermeta_value.t and
    type Index_value.t = Index_value.t

module Make_with_usermeta :
functor
  (Key : Protobuf_capable.S) (Value : Protobuf_capable.S) (Usermeta_value : Protobuf_capable.S) ->
    module type of Make_with_usermeta_index(Key)(Value)(Usermeta_value)(Default_index)

module Make_with_index :
functor
  (Key : Protobuf_capable.S) (Value : Protobuf_capable.S) (Index_value : Protobuf_capable.S) ->
    module type of Make_with_usermeta_index(Key)(Value)(Default_usermeta)(Index_value)

module Make :
functor (Key : Protobuf_capable.S) (Value : Protobuf_capable.S) -> 
  module type of Make_with_usermeta(Key)(Value)(Default_usermeta)

module Make_with_primitive_keys :
functor (Value : Protobuf_capable.S) ->
  module type of Make_with_usermeta_index_primitive_key(Value)(Default_usermeta)(Default_index)

module Make_with_value :
  functor (Value : Protobuf_capable.S) ->
    module type of Make_with_usermeta(String)(Value)(Default_usermeta)

