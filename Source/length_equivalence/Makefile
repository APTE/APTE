OCAMLC= ocamlc
OCAMLOPT= ocamlopt
OCAMLDEP= ocamldep
OCAMLDOC= ocamldoc

NAME_MODULE = Trace equivalence w.r.t. length
NAME_MODULE_OBJ = length_equivalence
NAME_MODULE_MAJ = Length_equivalence

INCLUDES= 
INDLUDES_MOD = -I ../

OCAMLFLAGS = $(INCLUDES) $(INDLUDES_MOD) -w Ae # add other options for ocamlc here 
OCAMLOPTFLAGS = $(INCLUDES) $(INDLUDES_MOD) -w Ae # add other options for ocamlopt here

# STANDARD LIBRARY

OBJ= length.cmx algorithm.cmx
OBJ_DEBUG = $(OBJ:%.cmx=%.cmo)

COMPILE = $(OBJ:%.cmx=%.ml) $(OBJ:%.cmx=%.mli) $(OBJ:%.cmx=%.cmi) $(OBJ)
COMPILE_DEBUG = $(OBJ:%.cmx=%.cmi) $(OBJ:%.cmx=%.cmo)


# Starting Point

main: .depend display $(COMPILE)
	@echo
	@echo Packing of all module files :
	@echo
	$(OCAMLOPT) -pack -o ../$(NAME_MODULE_OBJ).cmx $(OCAMLOPTFLAGS) $(OBJ)
	
debug: .depend display $(COMPILE_DEBUG)
	@echo
	@echo Packing of all module files :
	@echo
	$(OCAMLC) -g -pack -o ../$(NAME_MODULE_OBJ).cmo $(OCAMLFLAGS) $(OBJ_DEBUG)


# Common rules 

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo: 
	$(OCAMLC) -g $(OCAMLFLAGS) -c $<

.mli.cmi: 
	$(OCAMLOPT) -for-pack $(NAME_MODULE_MAJ) $(OCAMLOPTFLAGS) -c $<

.ml.cmx: 
	$(OCAMLOPT) -for-pack $(NAME_MODULE_MAJ) $(OCAMLOPTFLAGS) -c $<

# Clean up

clean:
	@echo ----- Clean module : $(NAME_MODULE) -----
	rm -f ../$(NAME_MODULE_OBJ).cm[iox]
	rm -f ../$(NAME_MODULE_OBJ).o
	rm -f *.cm[iox] *~ *.o
	
# Dependencies 

.depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend 
	
include .depend

# Display

display: 
	@echo ------ Compilation of the module : $(NAME_MODULE) ------
	@echo
