open Async.Std

module Result = Core.Std.Result
module Option = Core.Std.Option
module Deferred = Async.Std.Deferred

module Bytes = Protobuf_capables.Bytes
module String = Protobuf_capables.String
module Bool = Protobuf_capables.Bool
module Int = Protobuf_capables.Int
module Default_usermeta = String

module Default_index =
  struct
    type t =
      | String of (string [@key 2]) [@key 1]
      | Integer of (int [@key 4]) [@key 3]
      | Bad_int of (string [@key 6]) [@key 5]
      | Unknown of (string [@key 8]) [@key 7] [@@deriving protobuf, show]
  end

module type S =
  sig
    module Key : Protobuf_capable.Raw_S
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
      (Key.t list -> unit Async.Std.Deferred.t) ->
      (unit,
       [> `Bad_conn
       | `Bad_payload
       | `Incomplete_payload
       | `Overflow
       | `Protobuf_encoder_error
       | `Unknown_type
       | `Wrong_type ])
        Async.Std.Deferred.Result.t
    val with_cache :
      host:string ->
      port:int ->
      bucket:string ->
      (t -> ('a, [> Conn.error ] as 'e) Async.Std.Deferred.Result.t) ->
      ('a, 'e) Async.Std.Deferred.Result.t
    val list_keys :
      t ->
      (Key.t list,
       [> `Bad_conn
       | `Bad_payload
       | `Incomplete_payload
       | `Overflow
       | `Protobuf_encoder_error
       | `Unknown_type
       | `Wrong_type ])
        Result.t Async_kernel.Deferred.t
    val get :
      t ->
      ?opts:Opts.Get.t list ->
      Key.t ->
      ('a Robj.t, [> Opts.Get.error ]) Result.t Async_kernel.Deferred.t
    val put :
      t ->
      ?opts:Opts.Put.t list ->
      ?k:Key.t ->
      'a Robj.t ->
      ('b Robj.t * Key.t Option.t, [> Opts.Put.error ]) Result.t
	Async_kernel.Deferred.t

    val delete :
      t ->
      ?opts:Opts.Delete.t list ->
      Key.t ->
      (unit, [> Opts.Delete.error ]) Result.t Async_kernel.Deferred.t

    val purge :
      t ->
      (unit, [> Opts.Delete.error ]) Result.t Async_kernel.Deferred.t

    val purge2 :
      conn ->
      string ->
      (unit, [> Opts.Delete.error ]) Result.t Async_kernel.Deferred.t

    val index_search :
      t ->
      ?opts:Opts.Index_search.t list ->
      index:Index_value.t ->
      Opts.Index_search.Query.t ->
      (Response.Index_search.t, [> Opts.Index_search.error ]) Result.t
       Async_kernel.Deferred.t

    val bucket_props :
      t ->
      (Response.Props.t,
       [> `Bad_conn
       | `Bad_payload
       | `Incomplete_payload
       | `Overflow
       | `Protobuf_encoder_error
       | `Unknown_type
       | `Wrong_type ])
        Async.Std.Deferred.Result.t
  end

module Make_with_usermeta_index_raw_key
	 (Key:Protobuf_capable.Raw_S)
	 (Value:Protobuf_capable.S) 
	 (Usermeta_value: Protobuf_capable.S) 
	 (Index_value:Protobuf_capable.S) = struct 

  module Key = Key
  module Value = Value
  module Usermeta_value = Usermeta_value
  module Index_value = Index_value
			 
  let serialize_key = Key.to_string 
  let serialize_value = Protobuf_capable.serialize_proto Value.to_protobuf 
					
  let deserialize_key = Key.of_string
  let deserialize_value = Protobuf_capable.deserialize_proto Value.from_protobuf
					    
  type conn = Conn.t 
  type t = {conn:conn;bucket:string} 
	     
  let get_conn t = t.conn
  let get_bucket t = t.bucket
		       
  module Put = Opts.Put
  module Get = Opts.Get
  module Delete = Opts.Delete
		    
  module Unsafe_Robj = Robj
			 
  module Robj = struct
    
    module Link = 
      struct
	type t = { bucket : string option 
		 ; key    : Key.t option 
		 ; tag    : string option 
		 } 

	let bucket t = t.bucket
	let key t    = t.key
	let tag t    = t.tag
			 
	let set_bucket b t = { t with bucket = b }
	let set_key k t    = { t with key = k }
	let set_tag tag t  = { t with tag = tag }
			       
	let to_unsafe (t:t) : Unsafe_Robj.Link.t = 
	  let key = Option.map (key t) serialize_key in
	  let bucket = bucket t in
	  let tag = tag t in
	  {Unsafe_Robj.Link.bucket; key; tag}
	    
	let from_unsafe (t:Unsafe_Robj.Link.t) : t =
	  let key = Option.map (Unsafe_Robj.Link.key t) deserialize_key in
	  let bucket = Unsafe_Robj.Link.bucket t in
	  let tag = Unsafe_Robj.Link.tag t in {bucket;key;tag}
						
      end
	
    module type Unsafe_Pair = sig
	type t = {key: string ; value : string option}
	val value : t -> string option
	val key : t -> string
      end
				
    module Pair(Unsafe: Unsafe_Pair) (V:Protobuf_capable.S) = struct
      type t = { key : Key.t 
               ; value : V.t option 
	       }  
		 
      let create ~k ~v = { key = k; value = v }
			   
      let key t   = t.key
      let value t = t.value
		      
      let set_key s t = {t with key = s}
      let set_value so t = {t with value = so}
			     
      let to_unsafe (t:t) : Unsafe.t = 
	let key = serialize_key (key t) in

	let value = Option.map (value t) (Protobuf_capable.serialize_proto V.to_protobuf) in
	{ Unsafe.key; Unsafe.value}
	  
      let from_unsafe (t:Unsafe.t) : t =
	let value = Option.map (Unsafe.value t) (Protobuf_capable.deserialize_proto V.from_protobuf) in
	let key = deserialize_key (Unsafe.key t) in {key;value}
    end
								
    module Usermeta = Pair(Unsafe_Robj.Usermeta)(Usermeta_value)
    module Index = Pair(Unsafe_Robj.Index)(Index_value)
		       
    module Content = struct
      type t = { value            : Value.t 
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
		 
      let value t            = t.value
      let content_type t     = t.content_type
      let charset t          = t.charset
      let content_encoding t = t.content_encoding
      let vtag t             = t.vtag
      let links t            = t.links
      let last_mod t         = t.last_mod
      let last_mod_usec t    = t.last_mod_usec
      let usermeta t         = t.usermeta
      let indices t          = t.indices
      let deleted t          = match t.deleted with Some x -> x | None -> false

	
      let create v = {value = v; 
		      content_type=None;
		      charset=None;
		      content_encoding=None;
		      vtag=None;links=[];
		      last_mod=None;
		      last_mod_usec=None;
		      usermeta=[];
		      indices=[];
		      deleted=None}
		       
      let to_unsafe (t:t) : Unsafe_Robj.Content.t = 

        let module C = Unsafe_Robj.Content in 
        let open C in 
        create (serialize_value t.value) |>
        set_content_type t.content_type |>
        set_charset t.charset |>
        set_content_encoding t.content_encoding |> 
        set_vtag t.vtag  |>
        set_links (List.map Link.to_unsafe t.links) |> 
        set_last_mod t.last_mod |>
        set_last_mod_usec t.last_mod_usec |>
        set_usermeta (List.map Usermeta.to_unsafe t.usermeta) |> 
        set_indices (List.map Index.to_unsafe t.indices)

      let from_unsafe (content:Unsafe_Robj.Content.t) =
	let module C = Unsafe_Robj.Content in
	let v = deserialize_value (C.value content) in
	{(create v) with 
          content_type = C.content_type content; 
          charset = C.charset content;
          content_encoding = C.content_encoding content;
          vtag = C.vtag content;
          links = List.map Link.from_unsafe (C.links content);
          last_mod = C.last_mod content;
          last_mod_usec = C.last_mod_usec content;
          usermeta = List.map Usermeta.from_unsafe (C.usermeta content);
          indices = List.map Index.from_unsafe (C.indices content);
          deleted = if (C.deleted content) then Some true else None}
	  
	  
      let set_value v t             = { t with value = v }
      let set_content_type ct t     = { t with content_type = ct }
      let set_charset cs t          = { t with charset = cs }
      let set_content_encoding ce t = { t with content_encoding = ce }
      let set_vtag vt t             = { t with vtag = vt }
      let set_links ls t            = { t with links = ls }
      let set_last_mod lm t         = { t with last_mod = lm }
      let set_last_mod_usec lmu t   = { t with last_mod_usec = lmu }
      let set_usermeta u t          = { t with usermeta = u }
      let set_indices i t           = { t with indices = i }
					
    end
		       
    type 'a t = { contents  : Content.t list
		; vclock    : string option
		; unchanged : bool
		}
		  
    let create c =
      { contents  = [c]
      ; vclock    = None
      ; unchanged = false
      }
	
    let of_value v = create (Content.create v)
			    
    let create_siblings contents =
      { contents  = contents
      ; vclock    = None
      ; unchanged = false
      }
	
	
    let contents t        = t.contents
    let content t         = Core.Std.List.hd_exn (t.contents)
    let vclock t          = t.vclock
    let unchanged t       = t.unchanged
			      
    let set_contents cs t = { t with contents = cs }
    let set_content c t   = { t with contents = [c] }
    let set_vclock v t    = { t with vclock = v }
			      
    let to_unsafe t = 
      let content = Content.to_unsafe (content t) in
      Unsafe_Robj.set_vclock t.vclock (Unsafe_Robj.create content)
			     
    let from_unsafe t = 
      let contents = List.map Content.from_unsafe (Unsafe_Robj.contents t) in
      set_vclock (Unsafe_Robj.vclock t) (create_siblings contents)
  end
		  
  let create ~conn ~bucket = {conn;bucket}
  let list_keys_stream cache consumer = Conn.list_keys_stream cache.conn cache.bucket
							      (fun string -> let keys = List.map (fun b -> deserialize_key b) string in consumer keys) 
							      
  let with_cache ~host ~port ~bucket f =
    Conn.with_conn host port (fun conn -> (f (create ~conn ~bucket)))

  let list_keys cache = Conn.list_keys cache.conn cache.bucket >>| function
    | Result.Ok keys -> (** TODO - why do I need encode_decode here ? *)
        Result.Ok (List.map (fun (b:string) -> Key.of_string (Protobuf_capable.encode_decode b)) keys)
    | Result.Error err ->
        Result.Error err

  let get cache ?(opts = []) (k:Key.t) = Conn.get cache.conn ~opts ~b:cache.bucket (serialize_key k) 
					 >>| function
					   | Result.Ok robj_unsafe -> begin
								      let robj = Robj.from_unsafe robj_unsafe in
								      if Robj.contents robj = [] && Robj.vclock robj = None then
									Result.Error `Notfound
								      else
									Result.Ok robj 
								    end
					   | Result.Error err ->
					      Result.Error err
							   
  let put cache ?(opts = []) ?(k:Key.t option) (robj:'a Robj.t) =
    let unsafe_robj = Robj.to_unsafe robj in
    let serialized_key = Option.map k serialize_key in
    Conn.put
      cache.conn
      ~opts
      ~b:cache.bucket
      ?k:serialized_key 
      unsafe_robj
    >>| function
      | Result.Ok (unsafe_robj, key) ->
	 Result.Ok (Robj.from_unsafe unsafe_robj, Option.map key deserialize_key)
      | Result.Error err ->
	 Result.Error err
		      
  let delete cache ?(opts = []) (k:Key.t) =
    let serialized_key = serialize_key k in
    Conn.delete
      cache.conn
      ~opts
      ~b:cache.bucket
      serialized_key
    >>| function
      | Result.Ok () ->
 Result.Ok ()
      | Result.Error err ->
   Result.Error err
  let purge cache = Conn.purge cache.conn cache.bucket
   >>| function
     | Result.Ok () -> Result.Ok ()
     | Result.Error err -> Result.Error err

  let purge2 conn bucket = Conn.purge conn bucket
     >>| function
       | Result.Ok () -> Result.Ok ()
       | Result.Error err -> Result.Error err


  let index_search t ?(opts = []) ~(index:Index_value.t) query_type =
    Conn.index_search
      t.conn
      ~opts
      ~b:t.bucket
      ~index:(Protobuf_capable.serialize_proto Index_value.to_protobuf index)
      query_type
    >>| function
      | Result.Ok results ->
        Result.Ok results
      | Result.Error err ->
        Result.Error err
  let bucket_props t = Conn.bucket_props (get_conn t) (get_bucket t)
end

module Conv = Protobuf_capable.Conversion.Make

module Make_with_usermeta_index
 (Key:Protobuf_capable.S)
 (Value:Protobuf_capable.S) 
 (Usermeta_value: Protobuf_capable.S) 
 (Index_value:Protobuf_capable.S) = 
   Make_with_usermeta_index_raw_key(Conv(Key))(Value)(Usermeta_value)(Index_value)

module Make_with_usermeta(Key:Protobuf_capable.S) (Value:Protobuf_capable.S) (Usermeta_value:Protobuf_capable.S) =
  Make_with_usermeta_index(Key) (Value) (Usermeta_value) (Default_index)

module Make_with_index(Key:Protobuf_capable.S)(Value:Protobuf_capable.S)(Index_value:Protobuf_capable.S) =
  Make_with_usermeta_index(Key)(Value) (Default_usermeta) (Index_value)

module Make_with_string_key(Value:Protobuf_capable.S) =
  Make_with_usermeta_index_raw_key(Core.Std.String)
  (Value) (Default_usermeta) (Default_index)

module Make(Key:Protobuf_capable.S) (Value:Protobuf_capable.S) =
  Make_with_usermeta_index_raw_key(Conv(Key)) (Value) (Default_usermeta) (Default_index)

