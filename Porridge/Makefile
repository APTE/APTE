LIB = check.ml debug.ml memo.ml \
	  LTS.ml POR.ml \
	  channel.ml term.ml process.ml \
	  sem_utils.ml trace_equiv.ml
CHECK = $(LIB) check_main.ml

ML=$(wildcard *ml)
MLI=$(wildcard *mli)
CMO=$(ML:.ml=.cmo)

OCAMLC=ocamlfind ocamlc -g -package alcotest
OCAMLOPT=ocamlfind ocamlopt -package alcotest

all: run_tests \
  porridge.cma porridge.cmxa \
  test.byte test

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
	$(OCAMLOPT) $+ -pack -o $@
porridge.cmxa: porridge.cmx
	$(OCAMLOPT) $+ -a -o $@

test.byte: porridge.cma test.cmo
	$(OCAMLC) -linkpkg $+ -o $@

test: porridge.cmxa test.cmx
	$(OCAMLOPT) -linkpkg $+ -o $@

# stats: $(CMO)
# 	$(OCAMLC) \
# 	  debug.cmo memo.cmo \
# 	  LTS.cmo POR.cmo \
# 	  term.cmo process.cmo trace_equiv.cmo \
# 	  stats.cmo -o stats

-include .depend
.depend: $(ML) $(MLI)
	ocamldep *ml *mli > .depend

clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.cmxa check test

cleanPorridge:
	rm -f check.cmi  check_main.cmi  LTS.cmi  memo.cmi  sem_utils.cmi  test.cmi  trace_equiv.cmi term.cmi process.cmi  debug.cmi check test *.cmt *.cmo

plot.txt: stats
	./stats > $@
plot.pdf: plot.plot plot.txt
	gnuplot plot.plot
