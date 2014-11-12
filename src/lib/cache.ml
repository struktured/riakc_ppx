open Async.Std

module Result = Core.Std.Result
module String = Core.Std.String
module Option = Core.Std.Option

module type Key = sig include Protobuf_capable.S end
module type Value = sig include Protobuf_capable.S end

let encode_decode b =
    let e = Protobuf.Encoder.create () in
    Protobuf.Encoder.bytes b e; Protobuf.Encoder.to_bytes e

module Make(Key:Key) (Value:Value) =
struct 
     
let serialize_key (k:Key.t) = 
  let e = Protobuf.Encoder.create () in 
  Key.to_protobuf k e; Protobuf.Encoder.to_bytes e

let deserialize_key (b:bytes) = 
  let d = Protobuf.Decoder.of_bytes b in Key.from_protobuf d

let serialize_value (v:Value.t) = 
  let e = Protobuf.Encoder.create () in Value.to_protobuf v e; Protobuf.Encoder.to_bytes e

let deserialize_value (b:bytes) = 
  let d = Protobuf.Decoder.of_bytes b in Value.from_protobuf d

 type conn = Conn.t 
  type t = {conn:conn;bucket:string} 

  let get_conn t = t.conn
  let get_bucket t = t.bucket

  module Put = Opts.Put
  module Get = Opts.Get
  module Delete = Opts.Delete
  module Link = Robj.Link
  module Usermeta = Robj.Usermeta
  module Pair = Robj.Pair

module Unsafe_Robj = Robj

module Robj = struct
  module Usermeta = Robj.Usermeta
  module Pair = Robj.Pair
  module Link = Robj.Link
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
  ; indices          : Pair.t list 
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
              (C.set_links [] (*t.links*) 
                (C.set_last_mod t.last_mod 
                  (C.set_last_mod_usec t.last_mod_usec 
                    (C.set_usermeta [] (*t.usermeta*) 
                      (C.set_indices [] (*t.indices*) content)))))))))
   let from_unsafe (content:Unsafe_Robj.Content.t) =
      let module C = Unsafe_Robj.Content in
      let v = deserialize_value (C.value content) in
      {(create v) with 
        content_type = C.content_type content; 
        charset = C.charset content;
        content_encoding = C.content_encoding content;
        vtag = C.vtag content;
        links = [] (*C.links content*);
        last_mod = C.last_mod content;
        last_mod_usec = C.last_mod_usec content;
        usermeta = [] (*C.usermeta content*);
        indices = [] (*C.indices contenet*);
        deleted = if (C.deleted content) then Some true else None}
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
  let list_keys_stream cache consumer =
  Conn.do_request_stream
    cache.conn 
    (fun bytes -> let keys = List.map (fun b -> deserialize_key b) bytes in consumer keys) 
    (Request.list_keys cache.bucket)
    Response.list_keys 


  let list_keys cache =
  Conn.do_request
    cache.conn
    (Request.list_keys cache.bucket)
    Response.list_keys
  >>| function
    | Result.Ok keys ->
        Result.Ok (List.map (fun b -> Key.from_protobuf (Protobuf.Decoder.of_bytes (encode_decode b))) (List.concat keys))
    | Result.Error err ->
      Result.Error err

let get cache ?(opts = []) (k:Key.t) =
 Conn.do_request
    cache.conn
    (Request.get (Get.get_of_opts opts ~b:cache.bucket ~k:(serialize_key k)))
    Response.get
  >>| function
    | Result.Ok [robj_unsafe] -> begin
      let robj = Robj.from_unsafe robj_unsafe in
      if Robj.contents robj = [] && Robj.vclock robj = None then
	Result.Error `Notfound
      else
	Result.Ok robj 
    end
    | Result.Ok _ ->
      Result.Error `Wrong_type
    | Result.Error err ->
      Result.Error err

let put cache ?(opts = []) ?(k:Key.t option) (robj:'a Robj.t) =
  let unsafe_robj = Robj.to_unsafe robj in
  let serialized_key = Option.map k serialize_key in
  Conn.do_request
    cache.conn
    (Request.put (Put.put_of_opts opts ~b:cache.bucket ~k:serialized_key unsafe_robj))
    Response.put
  >>| function
    | Result.Ok [(unsafe_robj, key)] ->
      Result.Ok (Robj.from_unsafe unsafe_robj, Option.map key deserialize_key)
    | Result.Ok _ ->
      Result.Error `Wrong_type
    | Result.Error err ->
      Result.Error err

let delete cache ?(opts = []) (k:Key.t) =
  Conn.do_request
    cache.conn
    (Request.delete (Delete.delete_of_opts opts ~b:cache.bucket ~k:(serialize_key k)))
    Response.delete
  >>| function
    | Result.Ok [()] ->
      Result.Ok ()
    | Result.Ok _ ->
      Result.Error `Wrong_type
    | Result.Error err ->
      Result.Error err
(*
let index_search t ?(opts = []) ~b ~index query_type =
  let idx_s =
    Opts.Index_search.index_search_of_opts
      opts
      ~b
      ~index
      ~query_type
  in
  Conn.do_request
    t
    (Request.index_search ~stream:false idx_s)
    Response.index_search
  >>| function
    | Ok [results] ->
      Ok results
    | Ok _ ->
      Error `Wrong_type
    | Error err ->
      Error err
*)

let bucket_props t = Conn.bucket_props (get_conn t) (get_bucket t)
end

