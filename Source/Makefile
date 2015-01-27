EXECUTABLE = ../apte
NAME_PROGRAMME = APTE
VERSION = 0.4beta

OCAMLC= ocamlc
OCAMLOPT= ocamlopt
OCAMLDEP= ocamldep
OCAMLDOC= ocamldoc

INCLUDES = -I standard_library -I parser -I trace_equivalence -I length_equivalence

OCAMLFLAGS = $(INCLUDES) $(INCLUDES_MOD) -bin-annot -w Ae # add other options for ocamlc here
OCAMLOPTFLAGS = $(INCLUDES) $(INCLUDES_MOD) -bin-annot -w Ae # add other options for ocamlopt here


GENERATED_SOURCES = parser/grammar.ml parser/lexer.ml parser/grammar.mli
STD_ML = standard_library/term.ml standard_library/recipe.ml standard_library/constraint.ml \
		 standard_library/constraint_system.ml standard_library/process.ml \
		 standard_library/standard_library.ml
TRACE_ML = trace_equivalence/statistic.ml trace_equivalence/rules.ml trace_equivalence/invariant.ml \
		   trace_equivalence/strategy.ml trace_equivalence/algorithm.ml \
		   trace_equivalence/trace_equivalence.ml
LENGTH_ML = length_equivalence/length.ml length_equivalence/length_algorithm.ml \
			length_equivalence/length_equivalence.ml
PARSER_ML = parser/parser_function.ml parser/grammar.ml parser/lexer.ml \
			parser/parser.ml
ML = debug.ml $(STD_ML) $(TRACE_ML) $(LENGTH_ML) $(PARSER_ML) main.ml
OBJ = $(ML:.ml=.cmx)


# Starting Point

../apte: .depend .display $(OBJ)
	@echo
	@echo The executable :
	@echo
	$(OCAMLOPT) -o $(EXECUTABLE) $(OCAMLFLAGS) $(OBJ)
	@echo
	@echo ----- Some Statistics -----
	@echo
	@echo Number of line of the program :
	@find . -name "*.ml" -or -name "*.mli" -or -name "*.mly" -or -name "*.mll" | xargs cat | wc -l

# Clean up

clean:
	@echo ----- Clean $(NAME_PROGRAMME) -----
	rm -f $(EXECUTABLE) $(EXECUTABLE)_debug
	rm -f *~ *.cm[ioxt] *.o
	rm -f */*~ */*.cm[ioxt] */*.o
	rm -f $(GENERATED_SOURCES)
	rm -f .depend
	rm -rf ../log

# Display

.display: $(ML)
	@echo ----------------------------------------------
	@echo          Compilation of $(NAME_PROGRAMME) $(VERSION)
	@echo ----------------------------------------------
	@touch .display

# Common rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	@echo OCAMLC -c $<
	@$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	@echo OCAMLOPT -c $<
	@$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

.ml.cmx:
	@echo OCAMLOPT -c $<
	@$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

.mll.ml:
	ocamllex $<

.mly.ml:
	ocamlyacc -v $<

# Dependencies

SOURCES = $(wildcard *.ml) $(wildcard *.mli) \
          $(wildcard */*.ml) $(wildcard */*.mli)
.depend: $(SOURCES) $(GENERATED_SOURCES)
	$(OCAMLDEP) $(INCLUDES) $(SOURCES) $(GENERATED_SOURCES) > .depend

-include .depend
