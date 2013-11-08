OCAMLC= ocamlc
OCAMLOPT= ocamlopt
OCAMLDEP= ocamldep

NAME_MODULE = Parser
NAME_MODULE_OBJ = parser
NAME_MODULE_MAJ = Parser

INCLUDES = 
INDLUDES_MOD = -I ../ 

OCAMLFLAGS = $(INCLUDES) $(INDLUDES_MOD) -w Ae # add other options for ocamlc here 
OCAMLOPTFLAGS = $(INCLUDES) $(INDLUDES_MOD) -w Ae # add other options for ocamlopt here

# STANDARD LIBRARY

OBJ= parser_function.cmx
OBJ_DEBUG = $(OBJ:%.cmx=%.cmo)

COMPILE = $(OBJ:%.cmx=%.cmi) $(OBJ)
COMPILE_DEBUG = $(OBJ:%.cmx=%.cmi) $(OBJ:%.cmx=%.cmo)


# Starting Point

main: .depend display $(COMPILE) create_parser
	@echo
	@echo Packing of all module files :
	@echo
	$(OCAMLOPT) -pack -o ../$(NAME_MODULE_OBJ).cmx $(OCAMLOPTFLAGS) $(OBJ) grammar.cmx lexer.cmx
	
debug: .depend display $(COMPILE_DEBUG) create_parser_DEBUG
	@echo
	@echo Packing of all module files :
	@echo
	$(OCAMLC) -g -pack -o ../$(NAME_MODULE_OBJ).cmo $(OCAMLFLAGS) $(OBJ_DEBUG) grammar.cmo lexer.cmo
	
#Parser	
	
create_parser:
	ocamllex lexer.mll
	ocamlyacc -v grammar.mly
	$(OCAMLOPT) -for-pack $(NAME_MODULE) $(OCAMLOPTFLAGS) -c grammar.mli
	$(OCAMLOPT) -for-pack $(NAME_MODULE) $(OCAMLOPTFLAGS) -c grammar.ml
	$(OCAMLOPT) -for-pack $(NAME_MODULE) -i $(OCAMLOPTFLAGS) lexer.ml > lexer.mli
	$(OCAMLOPT) -for-pack $(NAME_MODULE) $(OCAMLOPTFLAGS) -c lexer.mli
	$(OCAMLOPT) -for-pack $(NAME_MODULE) $(OCAMLOPTFLAGS) -c lexer.ml
	
create_parser_DEBUG:
	ocamllex lexer.mll
	ocamlyacc grammar.mly
	$(OCAMLC) -for-pack $(NAME_MODULE) $(OCAMLFLAGS) -c grammar.mli
	$(OCAMLC) -for-pack $(NAME_MODULE) $(OCAMLFLAGS) -c grammar.ml
	$(OCAMLC) -i $(OCAMLFLAGS) lexer.ml > lexer.mli
	$(OCAMLC) -for-pack $(NAME_MODULE) $(OCAMLFLAGS) -c lexer.mli
	$(OCAMLC) -for-pack $(NAME_MODULE) $(OCAMLFLAGS) -c lexer.ml	
	
	
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
	@echo ----- Clean module : Parser -----
	rm -f ../$(NAME_MODULE_OBJ).cm[iox]
	rm -f ../$(NAME_MODULE_OBJ).o
	rm -f lexer.mli lexer.ml grammar.ml grammar.mli grammar.output
	rm -f *.cm[iox] *~ *.o
	
# Dependencies 

.depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend 
	
include .depend

# Display

display: 
	@echo ------ Compilation of the module : $(NAME_MODULE) ------
	@echo
