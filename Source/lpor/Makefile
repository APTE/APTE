ML=$(wildcard *ml)
MLI=$(wildcard *mli)
CMO=$(ML:.ml=.cmo)

OCAMLC=ocamlfind ocamlc -package alcotest

all: run_tests

%.cmo: %.ml
	$(OCAMLC) -g -c $<
%.cmi: %.mli
	$(OCAMLC) -g -c $<

CHECK_CMO =  \
			 check.cmo \
			 debug_.cmo memo.cmo \
			 LTS.cmo POR.cmo \
			 channel.cmo term_.cmo process_.cmo \
			 sem_utils.cmo semantics.cmo \
			 check_main.cmo
check: $(CHECK_CMO)
	$(OCAMLC) -linkpkg -g $(CHECK_CMO) -o check
run_tests: check
	./check

test: $(CMO)
	$(OCAMLC) -g \
	  debug_.cmo memo.cmo \
	  LTS.cmo POR.cmo \
	  term_.cmo process_.cmo semantics.cmo \
	  test.cmo -o test

stats: $(CMO)
	$(OCAMLC) -g \
	  debug_.cmo memo.cmo \
	  LTS.cmo POR.cmo \
	  term_.cmo process_.cmo semantics.cmo \
	  stats.cmo -o stats

-include .depend
.depend: $(ML) $(MLI)
	ocamldep *ml *mli > .depend

clean:
	rm -f *.cmi *.cmo test

plot.txt: stats
	./stats > $@
plot.pdf: plot.plot plot.txt
	gnuplot plot.plot
