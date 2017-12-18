LIB = check.ml memo.ml priority.ml \
	  LTS.ml POR.ml \
	  SAList.ml channel.ml tm.ml frame.ml formula.ml process.ml \
	  sem_utils.ml trace_equiv.ml
CHECK = $(LIB) check_main.ml

ML=$(wildcard *ml)
MLI=$(wildcard *mli)
CMO=$(ML:.ml=.cmo)

OCAMLCFLAGS=-g
OCAMLOPTFLAGS=-g

# The P flag is meant to be either empty or just "p"
# to switch from ocamlc/ocamlopt to ocamlcp/ocamloptp.
# Note that "make lib" won't work with these profiling
# compilers.
P=

OCAMLC=ocamlfind ocamlc$(P) $(OCAMLCFLAGS) -package alcotest
OCAMLOPT=ocamlfind ocamlopt$(P) $(OCAMLOPTFLAGS) -package alcotest

all: run_tests test.byte test

lib: porridge.cma porridge.cmxa

clean:
	rm -f *.o *.a *.cmi *.cmo *.cma *.cmx *.cmxa check test

%.cmo: %.ml
	$(OCAMLC) -c $<
%.cmi: %.mli
	$(OCAMLC) -c $<
%.cmx: %.ml
	$(OCAMLOPT) -for-pack Porridge -c $<

CHECK_CMO = $(CHECK:.ml=.cmo)

check: $(CHECK_CMO)
	$(OCAMLC) -linkpkg $(CHECK_CMO) -o check
run_tests: check
	./check

porridge.cmo: $(LIB:.ml=.cmo)
	$(OCAMLC) $+ -pack -o $@
porridge.cma: porridge.cmo
	$(OCAMLC) $+ -a -o $@

porridge.cmx: $(LIB:.ml=.cmx)
	$(OCAMLOPT) $+ -opaque -pack -o $@
porridge.cmxa: porridge.cmx
	$(OCAMLOPT) $+ -opaque -a -o $@

test.byte: $(LIB:.ml=.cmo) test.cmo
	$(OCAMLC) -linkpkg $+ -o $@

test: $(LIB:.ml=.cmx) test.cmx
	$(OCAMLOPT) -linkpkg $+ -o $@

# Profiling information

OCAMLPROF_FILES=$(LIB) test.ml
profiles:
	mkdir -p prof
	for i in $(OCAMLPROF_FILES) ; do OCAMLPROF_DUMP=/dev/null ocamlprof $$i > prof/$$i ; done

# Dependencies

-include .depend
.depend: $(ML) $(MLI)
	ocamldep *ml *mli > .depend
