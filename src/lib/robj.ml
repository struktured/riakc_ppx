open Core.Std

let option_of_bool = function
  | Some true -> Some true
  | _         -> None


module type Key = Protobuf_capable.S
module type Value = Protobuf_capable.S

module Link(Key:Key) = struct
  type t = { bucket : string option [@key 1]
           ; key    : Key.t option [@key 2]
           ; tag    : string option [@key 3]
  } [@@deriving Protobuf]
end

module Link(Key:Key) = Protobuf_capable.Make (Link (Key))

module Link(Key:Key) = struct
  type t = { bucket : string option [@key 1]
           ; key    : Key.t option [@key 2]
           ; tag    : string option [@key 3]
  } [@@deriving Protobuf]
end

module Link(Key:Key) = Protobuf_capable.Make (Link (Key))



module Link(Key:Key) = struct
  type t = { bucket : string option [@key 1]
           ; key    : Key.t option [@key 2]
           ; tag    : string option [@key 3]
  } [@@deriving Protobuf]
end

module Link(Key:Key) = Protobuf_capable.Make (Link (Key))


module Pair (Key:Key) (Value:Value) = struct
        type t = { key   : Key.t [@key 1]
           ; value : Value.t option [@key 2]
	   } [@@@deriving Protobuf]
end
module Pair(Key:Key) (Value:Value) = Protobuf_capable.Make(Pair(Key) (Value))

module Usermeta (Key:Key) (Value:Value) =
struct
  type t = { key : Key.t [@key 1]
           ; value : Value.t option [@key 2]
  } [@@deriving Protobuf]

  let create ~k ~v = { key = k; value = v }

  let key t   = t.key
  let value t = t.value

  let set_key s t = {t with key = s}
  let set_value so t = {t with value = so}
end

module Usermeta(Key:Key) (Value:Value) = Protobuf_capable.Make(Pair(Key) (Value))



module Content(Key:Key) (Value:Value) = struct
  module Link = Link(Key)
  module Pair = Pair (Key) (Value)
  module Usermeta = Usermeta (Key) (Value)
  type t = { value            : Value.t [@key 1]
	   ; content_type     : string option [@key 2]
	   ; charset          : string option [@key 3]
	   ; content_encoding : string option [@key 4]
           ; vtag             : string option [@key 5]
           ; links            : Link.t list   [@key 6]
           ; last_mod         : Int32.t option [@key 7]
           ; last_mod_usec    : Int32.t option [@key 8]
           ; usermeta         : Usermeta.t list [@key 9]
           ; indices          : Pair.t list [@key 10]
           ; deleted          : bool option [@key 11]
  } [@@deriving Protobuf]
end

module Content(Key:Key) (Value:Value) = Protobuf_capable.Make(Content (Key) (Value))

module Make (Key:Key) (Value:Value) = struct
module Content = Content (Key) (Value)
type 'a t = { contents  : Content.t list
	    ; vclock    : string option
	    ; unchanged : bool
	    }

let of_pb contents vclock unchanged =
  { contents  = contents
  ; vclock    = vclock
  ; unchanged = Option.value ~default:false unchanged
  }

let to_pb t = (t.contents, t.vclock)

let create c =
  { contents  = [c]
  ; vclock    = None
  ; unchanged = false
  }

let contents t        = t.contents
let content t         = List.hd_exn (t.contents)
let vclock t          = t.vclock
let unchanged t       = t.unchanged

let set_contents cs t = { t with contents = cs }
let set_content c t   = { t with contents = [c] }
let set_vclock v t    = { t with vclock = v }
end 
