# Swap the definitions of OCAMLBUILD to execute on cs department machines
# such as cycles.cs.princeton.edu
#
# OCAMLBUILD=~cos326/ocaml/bin/ocamlbuild

OCAMLBUILD=ocamlbuild

all: seq seqapps qpop example 

seq: 
	$(OCAMLBUILD) -use-ocamlfind test_seq.native

seqapps:
	$(OCAMLBUILD) -use-ocamlfind -I apps/ apps/main_apps.native

qpop:
	$(OCAMLBUILD) -use-ocamlfind -I population/ population/population.native

example:
	$(OCAMLBUILD) -use-ocamlfind examples.native

clean:
	rm -r _build
	rm *.native
