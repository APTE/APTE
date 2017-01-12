EXECUTABLE = ../apte
NAME_PROGRAMME = APTE
VERSION = 0.4beta

### Compiler

# For bytecode compilation, unset NATIVECODE below or run:
#  make NATIVECODE="" <target>
NATIVECODE=yes

OCAMLC=$(if $(NATIVECODE),ocamlopt,ocamlc)
OCAMLC_UC=$(shell echo $(OCAMLC) | tr a-z A-Z)
CMO=$(if $(NATIVECODE),cmx,cmo)
OCAMLDEP=ocamldep $(if $(NATIVECODE),-native)
OCAMLDOC=ocamldoc


### Compiler options

INCLUDES = -I standard_library -I parser -I trace_equivalence -I length_equivalence -I secrecy
# Compiler options specific to OCaml version >= 4
V4OPTIONS=$(if $(shell $(OCAMLC) -version | grep '^4'),-bin-annot)
OCAMLFLAGS = $(INCLUDES) $(INCLUDES_MOD) $(V4OPTIONS) -w Ae -inline 0 -g -p


### Makefile verbosity

# When V=@, full commands are not shown, e.g.,
#  ocamlc <plenty of options> -c <file>"
# is replaced by the summary
#  OCAMLC -c <file>
# To disable this feature (e.g. to debug compiler flags) use
#  make V="" <target>
# Variable V should only be set to "@" or "".
V=@
# Variable NV is used to disable summaries when V is not set.
NV=$(if $(V),@,@\#)


### Sources

GENERATED_SOURCES = parser/grammar.ml parser/lexer.ml parser/grammar.mli
STD_ML = standard_library/term.ml standard_library/recipe.ml standard_library/constraint.ml \
		 standard_library/constraint_system.ml standard_library/process.ml \
		 standard_library/standard_library.ml
TRACE_ML = trace_equivalence/statistic.ml trace_equivalence/rules.ml trace_equivalence/invariant.ml \
		   trace_equivalence/strategy.ml trace_equivalence/algorithm.ml \
		   trace_equivalence/trace_equivalence.ml
LENGTH_ML = length_equivalence/length.ml length_equivalence/length_algorithm.ml \
			length_equivalence/length_equivalence.ml
SECRECY_ML = secrecy/secrecy_algorithm.ml secrecy/secrecy.ml
PARSER_ML = parser/parser_function.ml parser/grammar.ml parser/lexer.ml \
			parser/parser.ml
ML = debug.ml $(STD_ML) $(TRACE_ML) $(LENGTH_ML) $(SECRECY_ML) $(PARSER_ML) main.ml
OBJ = $(ML:.ml=.$(CMO))


### Targets

../apte: .depend .display $(OBJ)
	@echo
	@echo The executable :
	@echo
	$(NV)echo $(OCAMLC_UC) -o $(EXECUTABLE)
	$(V)$(OCAMLC) -o $(EXECUTABLE) $(OCAMLFLAGS) $(OBJ)
	@echo
	@echo ----- Some Statistics -----
	@echo
	@echo Number of line of the program :
	@find . -name "*.ml" -or -name "*.mli" -or -name "*.mly" -or -name "*.mll" | xargs cat | wc -l

clean:
	@echo ----- Clean $(NAME_PROGRAMME) -----
	rm -f $(EXECUTABLE) $(EXECUTABLE)_debug
	rm -f *~ *.cm[ioxt] *.o
	rm -f */*~ */*.cm[ioxt] */*.o
	rm -f $(GENERATED_SOURCES)
	rm -f .depend
	rm -rf ../log

.display: $(ML)
	@echo ----------------------------------------------
	@echo          Compilation of $(NAME_PROGRAMME) $(VERSION)
	@echo ----------------------------------------------
	@touch .display


### Common rules

.SUFFIXES: .ml .mli .$(CMO) .cmi .mll .mly

.ml.$(CMO):
	$(NV)echo $(OCAMLC_UC) -c $<
	$(V)$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(NV)echo $(OCAMLC_UC) -c $<
	$(V)$(OCAMLC) $(OCAMLFLAGS) -c $<

.mll.ml:
	ocamllex $<

.mly.ml:
	ocamlyacc -v $<


### Dependencies

SOURCES = $(wildcard *.ml) $(wildcard *.mli) \
          $(wildcard */*.ml) $(wildcard */*.mli)
.depend: $(SOURCES) $(GENERATED_SOURCES) Makefile
	$(NV)echo OCAMLDEP
	$(V)$(OCAMLDEP) $(INCLUDES) $(SOURCES) $(GENERATED_SOURCES) > .depend

-include .depend
