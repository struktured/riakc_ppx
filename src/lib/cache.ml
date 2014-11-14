open Async.Std

module Result = Core.Std.Result
module Option = Core.Std.Option

let encode_decode (b:string) =
    let e = Protobuf.Encoder.create () in
    Protobuf.Encoder.bytes (Bytes.of_string b) e; Protobuf.Encoder.to_string e

let serialize_proto to_protobuf v = 
 let e = Protobuf.Encoder.create () in to_protobuf v e; Protobuf.Encoder.to_string e

let deserialize_proto from_protobuf (b:string) = 
  let d = Protobuf.Decoder.of_string b in from_protobuf d

module Bytes = Protobuf_capables.Bytes
module String = Protobuf_capables.String
module Bool = Protobuf_capables.Bool
module Int = Protobuf_capables.Int

module Default_usermeta = String

module Default_index = struct 
  type t =    String [@key 1] of string [@key 2]
             | Integer [@key 3] of int [@key 4]
             | Bad_int [@key 5] of string [@key 6]
             | Unknown [@key 7] of string [@key 8] [@@deriving protobuf, show]
end


module Make_with_usermeta_index
  (Key:Protobuf_capable.S) 
  (Value:Protobuf_capable.S) 
  (Usermeta_value: Protobuf_capable.S) 
  (Index_value:Protobuf_capable.S) = struct 

    module Key = Key
    module Value = Value
    module Usermeta_value = Usermeta_value
    module Index_value = Index_value

let serialize_key = serialize_proto Key.to_protobuf 
let serialize_value = serialize_proto Value.to_protobuf 

let deserialize_key = deserialize_proto Key.from_protobuf 
let deserialize_value = deserialize_proto Value.from_protobuf

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
    let module L = Unsafe_Robj.Link in 
    { L.bucket; L.key; L.tag}

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
    let value = Option.map (value t) (serialize_proto V.to_protobuf) in
    { Unsafe.key; Unsafe.value}

  let from_unsafe (t:Unsafe.t) : t =
    let value = Option.map (Unsafe.value t) (deserialize_proto V.from_protobuf) in
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
      let content = C.create (serialize_value t.value) in
      (C.set_content_type t.content_type 
        (C.set_charset t.charset 
          (C.set_content_encoding t.content_encoding 
            (C.set_vtag t.vtag 
              (C.set_links (List.map Link.to_unsafe t.links) 
                (C.set_last_mod t.last_mod 
                  (C.set_last_mod_usec t.last_mod_usec 
                    (C.set_usermeta (List.map Usermeta.to_unsafe t.usermeta) 
                      (C.set_indices (List.map Index.to_unsafe t.indices) content)))))))))
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
    | Result.Ok keys ->
        Result.Ok (List.map (fun (b:string) -> Key.from_protobuf (Protobuf.Decoder.of_string (encode_decode b))) keys)
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

let index_search t ?(opts = []) ~(index:Index_value.t) query_type =
  Conn.index_search
    t.conn
    ~opts
    ~b:t.bucket
    ~index:(serialize_proto Index_value.to_protobuf index)
    query_type
  >>| function
    | Result.Ok results ->
      Result.Ok results
    | Result.Error err ->
      Result.Error err


let bucket_props t = Conn.bucket_props (get_conn t) (get_bucket t)
end
module Make_with_usermeta(Key:Protobuf_capable.S) (Value:Protobuf_capable.S) (Usermeta_value:Protobuf_capable.S) =
    Make_with_usermeta_index(Key) (Value) (Usermeta_value) (Default_index)

module Make_with_index(Key:Protobuf_capable.S)(Value:Protobuf_capable.S)(Index_value:Protobuf_capable.S) =
    Make_with_usermeta_index(Key) (Value) (Default_usermeta) (Index_value)

module Make(Key:Protobuf_capable.S) (Value:Protobuf_capable.S) =
    Make_with_usermeta_index(Key) (Value) (Default_usermeta) (Default_index)



