# About

riakc_ppx is a type safe cache implementation of [ocaml-riakc](https://github.com/orbitz/ocaml-riakc) using ppx extensions. Keys, values, indices, and user meta data are all strongly typed for a particular riak bucket instance. It is currently a fork of ocaml-riakc, but could become dependent on it instead in the future.

# Requirements

As this project requires ppx extensions, you need a recent OCaml release.

 - OCaml >= 4.02.1
 - [bitstring](https://code.google.com/p/bitstring/)
 - [async](https://github.com/janestree/async)
 - [ppx\_deriving\_protobuf](https://github.com/whitequark/ppx_deriving_protobuf)

All of the above packages can be installed via opam.

# Installation

##Using opam

If you want to run this package in utop, do the following first:

> **opam pin add -k git riakc\_ppx git@github.com:/struktured/pa\_test.git**
> 

To install riakc_ppx itself simply run
> **opam install riakc_ppx**
>

or you can pin this repository:

> **opam pin add -k git riakc_ppx git@github.com:/struktured/riakc\_ppx.git**

##Building locally
The following will clone the repository, build locally, and then pin to opam:

> **git clone git@github.com:/struktured/riakc\_ppx.git**
> 
> **cd riakc\_ppx**
> 
> **omake**
> 
> **opam pin add riakc\_ppx .**
> 

# Examples

There are examples of using each API command in the example directory.  There is also a minor test suite in the tests directory. The primary API entry points are Cache, Robj, and Conn.

Here's a simple run through of creating a cache using a record key type and a variant value type. All that's needed to run this is utop, a running riak server, and a valid installation of riakc_ppx (with pa_test pinned as shown above!):

`utop # #require "riakc_ppx";;`
<hr>
`utop # module RecordKey = struct type t = {name:string [@key 1];id:int [@key 2]} [@@deriving protobuf] end;;`<BR><BR>
*`module RecordKey : sig`                                                                                         
 `type t = { name : bytes; id : int; }`<BR> 
`val from_protobuf : Protobuf.Decoder.t -> t` <BR> 
`val to_protobuf : t -> Protobuf.Encoder.t -> unit`<BR> 
`end`*<BR>  
<hr>
`utop # module VariantValue = struct type t = RED [@key 1] | GREEN [@key 2] [@@deriving protobuf] end;;`<BR><BR>
*`module VariantValue :`                                                                                                                                 
  `sig`                                                                                                                        
   `type t = RED | GREEN`                                                                                                                             
    `val from_protobuf : Protobuf.Decoder.t -> t`<BR>
    `val from_protobuf_bare : Protobuf.Decoder.t -> t`<BR>
    `val to_protobuf : t -> Protobuf.Encoder.t -> unit`<BR>
    `val to_protobuf_bare : t -> Protobuf.Encoder.t -> unit`<BR>
  `end`*<BR>
<hr>
`utop # module C = Cache.Make(RecordKey)(VariantValue);;`<BR><BR>
*`module C : sig ... end`*
<hr>
`utop # C.with_cache ~host:"localhost" ~port:8087 ~bucket:"demo-bucket1" (fun c -> C.put c ~k:{RecordKey.name="key 1";id=123} (C.Robj.of_value (VariantValue.GREEN)));;`<BR><br>
*`- : ('a C.Robj.t * RecordKey.t option, [> Opts.Put.error ]) Cache.Result.t =                                         
                                Core.Std.Result.Ok ({C.Robj.contents = []; vclock = None; unchanged = false}, None)`*<BR>
<hr>
`utop # C.with_cache ~host:"localhost" ~port:8087 ~bucket:"demo-bucket1" (fun c -> C.get c {RecordKey.name="key 1";id=123});;`<BR><BR>
*`- : ('a C.Robj.t, [> Opts.Get.error ]) Cache.Result.t = Core.Std.Result.Ok {C.Robj.contents =
   [{C.Robj.Content.value = VariantValue.GREEN; ... }]}`*
<hr>


# Contributing / Future work

## Future work

- Map reduce api
- Better test suite

## Contributing

- Open an issue on github 
- Contact struktured on \#ocaml freenode irc.
