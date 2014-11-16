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

The two API entry points are Cache, Robj, and Con..













https://github.com/orbitz/ocaml-riakc/issues/
* Releases
This represents only the features that are available in tagged releases.
** DONE 3.0.0
   - [X] Streaming support for list_keys
   - [X] Non-streaming support for 2i searches
   - [X] Misc API changes
** DONE 2.0.0
   - [X] Add support for setting 2i in an object.  Indices and Usermeta data types have been renamed.  Querying by 2i is not yet supporrted.
** DONE 1.0.0
   - [X] Requires Core >= 109.12.00 due to Async change and Ocaml >= 4.00
   - [X] Links are now supported in GET/PUT
   - [X] A note on the version number - this only represents that this release is not backwards compatible with 0.0.0, not some new level of stability.
** DONE 0.0.0
*** Methods
    - [X] ping
    - [X] client_id
    - [X] server_info
    - [X] list_buckets
    - [X] list_keys
    - [X] bucket_props
    - [X] get
    - [X] put
    - [X] delete
* Future Features
  - Timeouts on operations
  - 2i streaming support

