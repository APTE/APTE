OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
VERSION=0.3.2beta

EXECUTABLE = ../apte
NAME_PROGRAMME = APTE

INCLUDES = 

OCAMLFLAGS = $(INCLUDES) -w Ae # add other options for ocamlc here 
OCAMLOPTFLAGS = $(INCLUDES) -w Ae # add other options for ocamlopt here

MODULES = standard_library.mod trace_equivalence.mod length_equivalence.mod parser.mod
MODULES_COMPILE = $(MODULES:%.mod=%.cmx)
MODULES_DEBUG = $(MODULES:%.mod=%.debug)
MODULES_COMPILE_DEBUG = $(MODULES:%.mod=%.cmo)
MODULES_CLEAN = $(MODULES:%.mod=%.clean)

OBJ = debug.cmx
OBJ_DEBUG = $(OBJ:%.cmx=%.cmo)

COMPILE = $(OBJ:%.cmx=%.mli) $(OBJ:%.cmx=%.ml) $(OBJ:%.cmx=%.cmi) $(OBJ)
COMPILE_DEBUG = $(OBJ:%.cmx=%.cmi) $(OBJ:%.cmx=%.cmo)


# Starting Point

main: display .depend $(COMPILE) $(MODULES)
	@echo
	@echo The executable :
	@echo
	$(OCAMLOPT) -o $(EXECUTABLE) $(OCAMLFLAGS) $(OBJ)  $(MODULES_COMPILE) main.ml
	@echo
	@echo ----- Some Statistics -----
	@echo 
	@echo Number of line of the program :
	@find . -name "*.ml" -or -name "*.mli" -or -name "*.mly" -or -name "*.mll" | xargs cat | wc -l
	
debug: display .depend $(COMPILE_DEBUG) $(MODULES_DEBUG)
	$(OCAMLC) -g -o $(EXECUTABLE)_debug $(OCAMLFLAGS) $(OBJ_DEBUG) $(MODULES_COMPILE_DEBUG) main.ml
	
# Module

.SUFFIXES: .ml .mli .cmx .cmo .clean  .mod .cmi .debug

.ml.cmo: 
	$(OCAMLC) -g $(OCAMLFLAGS) -c $<

.mli.cmi: 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

.ml.cmx: 
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

.PHONY:%.mod
#	 standard_library.mod trace_equivalence.mod length_equivalence.mod parser.mod

%.mod :
	@echo
	@cd $* && $(MAKE)

%.debug :
	@echo
	@cd $* && $(MAKE) debug
	
%.clean :
	@cd $* && $(MAKE) clean

# Clean up

clean: $(MODULES_CLEAN)
	@echo ----- Clean $(NAME_PROGRAMME) -----
	rm -f $(EXECUTABLE) $(EXECUTABLE)_debug
	rm -f *~ *.cm[iox] *.o
	
# Dependencies 

.depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend 
	
include .depend

# Display

display: 
	@echo ----------------------------------------------
	@echo          Compilation of $(NAME_PROGRAMME) $(VERSION)
	@echo ----------------------------------------------

