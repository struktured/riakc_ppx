# About

**riakc_ppx** is a type safe cache implementation of [ocaml-riakc](https://github.com/orbitz/ocaml-riakc) using ppx extensions. Keys, values, indices, and user meta data are all strongly typed for a particular riak bucket instance. It is currently a fork of ocaml-riakc, but could become dependent on it instead in the future.

# Requirements

As this project requires ppx extensions, you need a recent OCaml release.

 - OCaml >= 4.02.1
 - [bitstring](https://code.google.com/p/bitstring/)
 - [async](https://github.com/janestree/async)
 - [ppx_deriving_protobuf](https://github.com/whitequark/ppx_deriving_protobuf)

All of the above packages can be installed via opam.

# Installation

## Using opam

If you want to run this package in utop, do the following first:

```
opam pin add -k git riakc_ppx git@github.com:/struktured/pa_test.git
``` 

To install riakc_ppx itself simply run
```
opam install riakc_ppx
```

or you can pin this repository:
```
opam pin add riakc_ppx --dev-repo
```

## Building locally

The following will clone the repository, build locally, and then pin to opam:

```
git clone git@github.com:/struktured/riakc_ppx.git
cd riakc_ppx
omake
opam pin add riakc_ppx .
```

# Examples

There are examples of using each API command in the example directory.  There is also a minor test suite in the tests directory. The primary API entry points are Cache, Robj, and Conn.

Here's a simple run through of creating a cache using a record key type and a variant value type. All that's needed to run this is utop, a running riak server, and a valid installation of riakc_ppx (with pa_test pinned as shown above!):

```ocaml
utop # #require "riakc_ppx";;
utop # module RecordKey = struct type t = {name:string [@key 1];id:int [@key 2]} [@@deriving protobuf] end;;
module RecordKey : sig
type t = { name : bytes; id : int; }
val from_protobuf : Protobuf.Decoder.t -> t
val to_protobuf : t -> Protobuf.Encoder.t -> unit
end
utop # module VariantValue = struct type t = RED [@key 1] | GREEN [@key 2] [@@deriving protobuf] end;;
module VariantValue :
  sig
   type t = RED | GREEN
    val from_protobuf : Protobuf.Decoder.t -> t
    val from_protobuf_bare : Protobuf.Decoder.t -> t
    val to_protobuf : t -> Protobuf.Encoder.t -> unit
    val to_protobuf_bare : t -> Protobuf.Encoder.t -> unit
  end
utop # module C = Cache.Make(RecordKey)(VariantValue);;
module C : sig .. end
utop # C.with_cache ~host:"localhost" ~port:8087 ~bucket:"demo-bucket1" (fun c -> C.put c ~k:{RecordKey.name="key 1";id=123} (C.Robj.of_value (VariantValue.GREEN)));;
- : ('a C.Robj.t  RecordKey.t option, [> Opts.Put.error ]) Cache.Result.t =
    Core.Std.Result.Ok ({C.Robj.contents = []; vclock = None; unchanged = false}, None)
utop # C.with_cache ~host:"localhost" ~port:8087 ~bucket:"demo-bucket1" (fun c -> C.get c {RecordKey.name="key 1";id=123});;
- : ('a C.Robj.t, [> Opts.Get.error ]) Cache.Result.t = Core.Std.Result.Ok {C.Robj.contents =
    [{C.Robj.Content.value = VariantValue.GREEN; ... }]}
```

# Contributing / Future work

## Future work

 * Map reduce api
 * Better test suite

## Contributing

 * Open an issue on github 
 * Contact struktured on \#ocaml freenode irc.
