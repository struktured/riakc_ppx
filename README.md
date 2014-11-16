# About

riakc_ppx is a type safe cache extension to [ocaml-riakc](https://github.com/orbitz/ocaml-riakc). Keys, values,
indices, and user meta data are all strongly typed for a particular riak bucket instance.

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

To install riakc-ppx itself you can pin this repository:

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

There are examples of using each API command in the example directory.  There is also a minor test suite in the tests directory.

The two API entry points are Cache, Robj, and Con.
