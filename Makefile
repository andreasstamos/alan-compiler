# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

# Compiler settings
OCAMLC=ocamlc
OCAMLOPT=ocamlfind ocamlopt
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

# Compiler flags
OCAMLC_FLAGS=-g
OCAMLOPT_FLAGS=-g -cc clang-15 \
  -package llvm.shared \
  -package llvm.target.shared \
  -package llvm.scalar_opts.shared \
  -package llvm.analysis.shared \
  -package llvm.passmgr_builder.shared \
  -package llvm.ipo.shared \
  -package llvm.vectorize.shared \
  -linkpkg

# Source files
SOURCES=Ast.ml Lexer.ml Parser.ml ShowAst.ml Semantic.ml Codegen.ml Optimize.ml Main.ml

# Object files
OBJECTS=$(SOURCES:.ml=.cmx)

# Target executable
TARGET=alanc$(EXE)

ALLDEPS=depend_restart
ifdef NODEPEND
	ALLDEPS=$(TARGET)
endif

# Default target
all: $(ALLDEPS)

depend_restart: depend
	NODEPEND=1 make 

# Rule to build the target executable
$(TARGET): $(OBJECTS)
	$(OCAMLOPT) $(OCAMLOPT_FLAGS) -o $@ $^

# Rules for compiling .ml and .mli files
%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPT_FLAGS) -c $<

%.cmi: %.mli
	$(OCAMLOPT) $(OCAMLOPT_FLAGS) -c $<

# Lexer and parser generation
Lexer.ml: Lexer.mll
	$(OCAMLLEX) -o $@ $<

Parser.ml Parser.mli: Parser.mly
	$(OCAMLYACC) -v $<

# Dependencies
.PHONY: depend clean distclean

depend: $(SOURCES) Parser.mli
	$(OCAMLDEP) $^ > .depend

-include .depend

clean:
	$(RM) $(SOURCES:.ml=.cmx) $(SOURCES:.ml=.cmi) $(SOURCES:.ml=.o) Lexer.ml Parser.ml Parser.mli Parser.output .depend $(TARGET)

