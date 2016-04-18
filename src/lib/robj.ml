let option_of_bool = function
  | Some true -> Some true
  | _         -> None


module Link = struct
  type t = { bucket : (string option [@key 1])
           ; key    : (string option [@key 2])
           ; tag    : (string option [@key 3])
  } [@@deriving protobuf]

  let bucket t = t.bucket
  let key t    = t.key
  let tag t    = t.tag

  let set_bucket b t = { t with bucket = b }
  let set_key k t    = { t with key = k }
  let set_tag tag t  = { t with tag = tag }

end



module Pair = struct
  type t = { key : (string [@key 1])
           ; value : (string option [@key 2])
  } [@@deriving protobuf]

  let create ~k ~v = { key = k; value = v }

  let key t   = t.key
  let value t = t.value

  let set_key s t = {t with key = s}
  let set_value so t = {t with value = so}
end

module Usermeta = Pair
module Index = Pair

module Content = struct
  module Link = Link
  module Pair = Pair 
  module Usermeta = Usermeta 
  module Index = Index

  type t = { value            : (string [@key 1])
	   ; content_type     : (string option [@key 2])
	   ; charset          : (string option [@key 3])
	   ; content_encoding : (string option [@key 4])
           ; vtag             : (string option [@key 5])
           ; links            : (Link.t list   [@key 6])
           ; last_mod         : (Int32.t option [@key 7] [@encoding `varint])
           ; last_mod_usec    : (Int32.t option [@key 8] [@encoding `varint])
           ; usermeta         : (Usermeta.t list [@key 9])
           ; indices          : (Index.t list [@key 10])
           ; deleted          : (bool option [@key 11])
  } [@@deriving protobuf]

  let create v =
    { value = v
    ; content_type     = None
    ; charset          = None
    ; content_encoding = None
    ; vtag             = None
    ; links            = []
    ; last_mod         = None
    ; last_mod_usec    = None
    ; usermeta         = []
    ; indices          = []
    ; deleted          = Some false
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

let of_pb contents vclock unchanged =
  { contents  = contents
  ; vclock    = vclock
  ; unchanged = Core.Std.Option.value ~default:false unchanged
  }

let to_pb t = (t.contents, t.vclock)

let create c =
  { contents  = [c]
  ; vclock    = None
  ; unchanged = false
  }

let of_value v = create (Content.create v)

let contents t        = t.contents
let content t         = Core.Std.List.hd_exn (t.contents)
let vclock t          = t.vclock
let unchanged t       = t.unchanged

let set_contents cs t = { t with contents = cs }
let set_content c t   = { t with contents = [c] }
let set_vclock v t    = { t with vclock = v }
