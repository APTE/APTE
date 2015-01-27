VERSION=0.4beta
NAME_PROGRAMME = APTE

PATH_SOURCE = ../Source/

NAME_STANDARD = standard_library
FILE_STANDARD = term.mli recipe.mli constraint.mli constraint_system.mli process.mli
PATH_STANDARD = $(FILE_STANDARD:%.mli= $(PATH_SOURCE)standard_library/%.mli)

NAME_TRACE = trace_equivalence
FILE_TRACE = rules.mli strategy.mli algorithm.mli
PATH_TRACE = $(FILE_TRACE:%.mli= $(PATH_SOURCE)trace_equivalence/%.mli)

# Starting Point

main: tool display standard trace
	@echo
	@echo Compilation of the .pdf file :
	@echo
	latex main
	bibtex main || true
	latex main
	dvipdf -dPDFSETTINGS=/prepress -dEmbedAllFonts=true main
		
# Module Stan<dard Library

standard:
	@echo
	@echo Compilation of the $(NAME_STANDARD).tex file: 
	@echo
	ocamldoc.opt -latextitle 4,subparagraph -noheader -notrailer -o $(NAME_STANDARD).tex -latex -I $(PATH_SOURCE) -I $(PATH_SOURCE)$(NAME_STANDARD)/ $(PATH_STANDARD)
	
trace:
	@echo
	@echo Compilation of the $(NAME_TRACE).tex file: 
	@echo
	ocamldoc.opt -latextitle 4,subparagraph -noheader -notrailer -o $(NAME_TRACE).tex -latex -I $(PATH_SOURCE) -I $(PATH_SOURCE)$(NAME_TRACE)/ $(PATH_TRACE)
		
# Display

display: 
	@echo ----------------------------------------------
	@echo         Documentation of $(NAME_PROGRAMME) $(VERSION)
	@echo ----------------------------------------------

# Cleaning
clean: clean_tool
	@echo ----- Clean documentation of $(NAME_PROGRAMME) $(VERSION) -----
	rm -f main.aux main.bbl main.blg main.dvi main.log main.out main.pdf main.toc *~

# Tool

tool: 
	@cd ../Source/;make
	
clean_tool:
	@cd ../Source/;make clean
