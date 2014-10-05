open Core.Std

module D = Protobuf.Decoder

module type Key = Protobuf_capable.S

type pair = (string * string option)
type keys = Key.t list

let client_id =
  D.bytes

let server_info =
  D.bytes_opt 1 >>= fun node ->
  D.bytes_opt 2 >>= fun server ->
  D.return (node, server)

let list_buckets =
  D.bytes_rep 1 >>= D.return

let list_keys =
  D.bytes_rep 1 >>= fun keys ->
  D.bool_opt  2 >>= function
    | Some true ->
      D.return (keys, true)
    | _ ->
      D.return (keys, false)

let bucket_props =
  let props =
    D.int32_opt 1 >>= fun n_val ->
    D.bool_opt  2 >>= fun allow_mult ->
    D.return (n_val, allow_mult)
  in
  D.embd_msg 1 props >>= D.return

let get =
  D.embd_msg_rep 1 Pb_robj.Content.parse >>= fun contents ->
  D.bytes_opt    2                       >>= fun vclock ->
  D.bool_opt     3                       >>= fun unchanged ->
  D.return (contents, vclock, unchanged)

let put =
  D.embd_msg_rep 1 Pb_robj.Content.parse >>= fun contents ->
  D.bytes_opt    2                       >>= fun vclock ->
  D.bytes_opt    3                       >>= fun key ->
  D.return (contents, vclock, key)

let pair =
  D.bytes     1 >>= fun key ->
  D.bytes_opt 2 >>= fun value ->
  D.return (key, value)

let index_search =
  D.bytes_rep    1      >>= fun keys ->
  D.embd_msg_rep 2 pair >>= fun results ->
  D.bytes_opt    3      >>= fun cont ->
  D.bool_opt     4      >>= fun d ->
  D.return (keys, results, cont, d)
