include Makefile.config

#---------------------------------------
# Directories
#---------------------------------------

SRCDIR = $(shell pwd)
#
# Installation directory prefix
#
PREFIX = $(BDDAPRON_PREFIX)

#---------------------------------------
# CAML part
#---------------------------------------

DOCLATEX =\
-latextitle 1,chapter\
-latextitle 2,section\
-latextitle 3,subsection\
-latextitle 4,subsubsection\
-latextitle 5,paragraph\
-latextitle 6,subparagraph\
-noheader -notrailer

OCAMLINC = -I $(MLCUDDIDL_PREFIX)/lib -I $(CAMLLIB_PREFIX)/lib -I $(MLGMPIDL_PREFIX)/lib -I $(APRON_PREFIX)/lib

BDDMOD = \
	bdd/output \
	bdd/reg \
	bdd/int \
	bdd/enum \
	bdd/env \
	bdd/cond \
	bdd/expr0 bdd/expr1 \
	bdd/domain0 bdd/domain1


MTBDDAPRONMOD = \
	mtbddapron/apronexpr \
	mtbddapron/apronexprDD \
	mtbddapron/apronDD \
	mtbddapron/env \
	mtbddapron/cond \
	mtbddapron/expr0 mtbddapron/expr1 mtbddapron/expr2 \
	mtbddapron/domain0 mtbddapron/domain1 \
	mtbddapron/syntax mtbddapron/yacc mtbddapron/lex mtbddapron/parser

MLMOD = $(BDDMOD) $(MTBDDAPRONMOD)

MLSRC = $(MLMOD:%=%.mli) $(MLMOD:%=%.ml)
MLINT = $(MLMOD:%=%.cmi)
MLOBJ = $(MLMOD:%=%.cmo)
MLOBJx = $(MLMOD:%=%.cmx)

MLLIB_TOINSTALL = bdd.cmi bdd.cma mtbddapron.cmi mtbddapron.cma
MLLIB_TOINSTALLx = bdd.cmx bdd.cmxa bdd.a mtbddapron.cmx mtbddapron.cmxa mtbddapron.a

#---------------------------------------
# Rules
#---------------------------------------

# Global rules
all: $(MLLIB_TOINSTALL) $(MLLIB_TOINSTALLx)

byte: $(MLLIB_TOINSTALL)
opt: $(MLLIB_TOINSTALLx)

install:
	$(INSTALLd) $(PREFIX)/lib
	for i in $(MLLIB_TOINSTALL) $(MLLIB_TOINSTALLx); do \
		if test -f $$i; then $(INSTALL) $$i $(PREFIX)/lib; fi; \
	done

distclean:
	(cd $(PREFIX)/lib; /bin/rm -f $(MLLIB_TOINSTALL) $(MLLIB_TOINSTALLx))

clean:
	for i in . bdd mtbddapron; do \
		cd $(SRCDIR)/$$i; \
		/bin/rm -f *.[aoc] *.cm[ioxa] *.cmxa *.annot; \
		/bin/rm -f *.log *.aux *.bbl *.blg *.toc **.idx *.ilg *.ind ocamldoc*.tex ocamldoc.sty *.dvi *.pdf *.out; \
		/bin/rm -fr html; \
		/bin/rm -f bddtop mtbddaprontop example?.byte example?.opt; \
	done
	(cd mtbddapron; /bin/rm -f yacc.ml yacc.mli lex.ml)

mostlyclean: clean
	for i in . bdd mtbddapron; do \
		/bin/rm -f Makefile.depend *.annot; \
	done

# CAML rules
mtbddapron.cma: bdd.cmo mtbddapron.cmo
	$(OCAMLC) -a $(OCAMLFLAGS) $(OCAMLINC) -o $@ $^
mtbddapron.cmxa: bdd.cmx mtbddapron.cmx
	$(OCAMLOPT) -a $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ $^
	$(RANLIB) mtbddapron.a
bdd.cma: bdd.cmo
	$(OCAMLC) -a $(OCAMLFLAGS) $(OCAMLINC) -o $@ $^
bdd.cmxa: bdd.cmx
	$(OCAMLOPT) -a $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ $^
	$(RANLIB) bdd.a

mtbddapron.cmo: $(MTBDDAPRONMOD:%=%.cmo)
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -pack -o $@ $^

mtbddapron.cmx: $(MTBDDAPRONMOD:%=%.cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -pack -o $@ $^

bdd.cmo: $(BDDMOD:%=%.cmo)
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -pack -o $@ $^

bdd.cmx: $(BDDMOD:%=%.cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -pack -o $@ $^


# TEX rules
.PHONY: mtbddapron.dvi mtbddapron.pdf bdd.dvi bdd.pdf html depend

mtbddapron.pdf: mtbddapron.dvi
	$(DVIPDF) mtbddapron.dvi mtbddapron.pdf

mtbddapron.dvi: $(MTBDDAPRONMOD:%=%.cmi) $(MTBDDAPRONMOD:%=%.mli)
	$(OCAMLDOC) -latextitle 1,chapter -latextitle 2,section -latextitle 3,subsection -latextitle 4,subsubsection -latextitle 5,paragraph -noheader -notrailer -latex -o ocamldoc.tex $(OCAMLINC) -I mtbddapron $(MTBDDAPRONMOD:%=%.mli)
	$(LATEX) mtbddapron/mtbddapron
	$(MAKEINDEX) mtbddapron
	$(LATEX) mtbddapron/mtbddapron
	$(LATEX) mtbddapron/mtbddapron

bdd.pdf: bdd.dvi
	$(DVIPDF) bdd.dvi bdd.pdf

bdd.dvi: $(BDDMOD:%=%.cmi) $(BDDMOD:%=%.mli)
	$(OCAMLDOC) -latextitle 1,chapter -latextitle 2,section -latextitle 3,subsection -latextitle 4,subsubsection -latextitle 5,paragraph -noheader -notrailer -latex -o ocamldoc.tex $(OCAMLINC) -I bdd $(BDDMOD:%=%.mli)
	$(LATEX) bdd/bdd
	$(MAKEINDEX) bdd
	$(LATEX) bdd/bdd
	$(LATEX) bdd/bdd

html: $(MLINT) $(MLMOD:%=%.mli)
	mkdir -p html
	$(OCAMLDOC) -html -d html -colorize-code $(OCAMLINC) $(MLMOD:%=%.mli)

#---------------------------------------
# Test
#---------------------------------------

bddtop: bdd.cma
	$(OCAMLMKTOP) -g -verbose $(OCAMLFLAGS) $(OCAMLINC) -o $@ \
	-custom -cc "$(CC)" cudd.cma camllib.cma bdd.cma

mtbddaprontop: mtbddapron.cma
	$(OCAMLMKTOP) -g -verbose $(OCAMLFLAGS) $(OCAMLINC) -o $@ \
	-custom -cc "$(CC)" cudd.cma camllib.cma \
	bigarray.cma gmp.cma apron.cma box.cma polka.cma mtbddapron.cma -cclib "-lboxMPQ -lpolkaMPQ"

example1.byte: bdd/example1.ml bdd.cma
	$(OCAMLC) -g $(OCAMLFLAGS) $(OCAMLINC) -o $@ -custom cudd.cma camllib.cma bdd.cma $<

example2.byte: mtbddapron/example2.ml mtbddapron.cma
	$(OCAMLC) -g $(OCAMLFLAGS) $(OCAMLINC) -o $@ -custom cudd.cma camllib.cma bigarray.cma gmp.cma apron.cma box.cma polka.cma mtbddapron.cma  -cclib "-lboxMPQ -lpolkaMPQ" $<

example1.opt: mtbddapron/example2.ml mtbddapron.cmxa
	$(OCAMLOPT) -verbose -g $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ cudd.cmxa camllib.cmxa bdd.cmxa $<

example2.opt: mtbddapron/example2.ml mtbddapron.cmxa
	$(OCAMLOPT) -verbose -g $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ cudd.cmxa camllib.cmxa bigarray.cmxa gmp.cmxa apron.cmxa box.cmxa polka.cmxa mtbddapron.cmxa -cclib "-lboxMPQ -lpolkaMPQ" $<

#--------------------------------------------------------------
# IMPLICIT RULES AND DEPENDENCIES
#--------------------------------------------------------------

.SUFFIXES: .ml .mli .cmi .cmo .cmx .tex
.PRECIOUS: $(BDDMOD:%=%.cmi) $(BDDMOD:%=%.cmo) $(BDDMOD:%=%.cmx) $(MTBDDAPRONMOD:%=%.cmi) $(MTBDDAPRONMOD:%=%.cmo) $(MTBDDAPRONMOD:%=%.cmx) 

#-----------------------------------
# CAML
#-----------------------------------

bdd/%.cmi: bdd/%.mli
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -I bdd -c $<

bdd/%.cmo: bdd/%.ml
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -I bdd -c $<

bdd/%.cmx: bdd/%.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -I bdd -for-pack Bdd -c $<

mtbddapron/%.cmi: mtbddapron/%.mli bdd.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -I mtbddapron -c $<

mtbddapron/%.cmo: mtbddapron/%.ml bdd.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -I mtbddapron -c $<

mtbddapron/%.cmx: mtbddapron/%.ml bdd.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -I mtbddapron -for-pack Mtbddapron -c $<

mtbddapron/%.ml: mtbddapron/%.mll
	$(OCAMLLEX) $^

mtbddapron/%.ml mtbddapron/%.mli: mtbddapron/%.mly
	$(OCAMLYACC) $^

depend: mtbddapron/yacc.ml mtbddapron/yacc.mli mtbddapron/lex.ml
	$(OCAMLDEP) $(OCAMLINC) -I bdd $(BDDMOD:%=%.mli) $(BDDMOD:%=%.ml) >Makefile.depend
	$(OCAMLDEP) $(OCAMLINC) -I mtbddapron $(MTBDDAPRONMOD:%=%.mli) $(MTBDDAPRONMOD:%=%.ml) >>Makefile.depend

Makefile.depend: mtbddapron/yacc.ml mtbddapron/yacc.mli mtbddapron/lex.ml
	$(OCAMLDEP) $(OCAMLINC) -I bdd $(BDDMOD:%=%.mli) $(BDDMOD:%=%.ml) >Makefile.depend
	$(OCAMLDEP) $(OCAMLINC) -I mtbddapron $(MTBDDAPRONMOD:%=%.mli) $(MTBDDAPRONMOD:%=%.ml) >>Makefile.depend

-include Makefile.depend
