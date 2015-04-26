all:	conflict_tests.ml
	ocamlfind ocamlc -g -principal conflict_tests.ml -linkpkg -thread -I ./ -package async,core,ppx_tools,ppx_deriving_protobuf,riakc_ppx
#.cmo:	.ml .cmi 
#	ocamlfind ocamlc -g -c .ml timePB.ml -thread -package core,riakc_ppx
#.cmi:	.mli
#	ocamlfind ocamlc -g -c .mli -thread -package core,riakc_ppx
