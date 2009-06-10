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

BDDAPRONMOD = \
	bddapron/apronexpr \
	bddapron/apronexprDD \
	bddapron/apronDD \
	bddapron/env \
	bddapron/cond \
	bddapron/expr0 bddapron/expr1 bddapron/expr2 \
        bddapron/descend \
	bddapron/domain0 bddapron/domain1 \
	bddapron/syntax bddapron/yacc bddapron/lex bddapron/parser

MLMOD = $(BDDMOD) $(BDDAPRONMOD)

MLSRC = $(MLMOD:%=%.mli) $(MLMOD:%=%.ml)
MLINT = $(MLMOD:%=%.cmi)
MLOBJ = $(MLMOD:%=%.cmo)
MLOBJx = $(MLMOD:%=%.cmx)

MLLIB_TOINSTALL = bdd.cmi bdd.cma bddapron.cmi bddapron.cma
MLLIB_TOINSTALLx = bdd.cmx bdd.cmxa bdd.a bddapron.cmx bddapron.cmxa bddapron.a

#---------------------------------------
# Rules
#---------------------------------------

# Global rules
all: byte opt

byte: bdd.cma bddapron.cma
opt: bdd.cmxa bddapron.cmxa

install:
	$(INSTALLd) $(PREFIX)/lib
	for i in $(MLLIB_TOINSTALL) $(MLLIB_TOINSTALLx); do \
		if test -f $$i; then $(INSTALL) $$i $(PREFIX)/lib; fi; \
	done

distclean:
	(cd $(PREFIX)/lib; /bin/rm -f $(MLLIB_TOINSTALL) $(MLLIB_TOINSTALLx))

clean:
	for i in . bdd bddapron; do \
		cd $(SRCDIR)/$$i; \
		/bin/rm -f *.[aoc] *.cm[ioxa] *.cmxa *.annot; \
		/bin/rm -f *.log *.aux *.bbl *.blg *.toc **.idx *.ilg *.ind ocamldoc*.tex ocamldoc.sty *.dvi *.pdf *.out; \
		/bin/rm -fr html; \
		/bin/rm -f bddtop bddaprontop example?.byte example?.opt; \
	done
	(cd bddapron; /bin/rm -f yacc.ml yacc.mli lex.ml)

mostlyclean: clean
	for i in . bdd bddapron; do \
		/bin/rm -f Makefile.depend *.annot; \
	done

# CAML rules
bddapron.cma: bdd.cmo bddapron.cmo
	$(OCAMLC) -a $(OCAMLFLAGS) $(OCAMLINC) -o $@ $^
bddapron.cmxa: bdd.cmx bddapron.cmx
	$(OCAMLOPT) -a $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ $^
	$(RANLIB) bddapron.a
bdd.cma: bdd.cmo
	$(OCAMLC) -a $(OCAMLFLAGS) $(OCAMLINC) -o $@ $^
bdd.cmxa: bdd.cmx
	$(OCAMLOPT) -a $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ $^
	$(RANLIB) bdd.a

bddapron.cmo: bddapron.cmi
bddapron.cmi: $(BDDAPRONMOD:%=%.cmo)
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -pack -o bddapron.cmo $^

bddapron.cmx: $(BDDAPRONMOD:%=%.cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -pack -o $@ $^

bdd.cmo: bdd.cmi
bdd.cmi: $(BDDMOD:%=%.cmo)
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -pack -o bdd.cmo $^

bdd.cmx: $(BDDMOD:%=%.cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -pack -o $@ $^


# TEX rules
.PHONY: bddapron.dvi bddapron.pdf bdd.dvi bdd.pdf html depend

bddapron.pdf: bddapron.dvi
	$(DVIPDF) bddapron.dvi bddapron.pdf

bddapron.dvi: $(BDDAPRONMOD:%=%.cmi) $(BDDAPRONMOD:%=%.mli)
	$(OCAMLDOC) -latextitle 1,chapter -latextitle 2,section -latextitle 3,subsection -latextitle 4,subsubsection -latextitle 5,paragraph -noheader -notrailer -latex -o ocamldoc.tex $(OCAMLINC) -I bddapron $(BDDAPRONMOD:%=%.mli)
	$(LATEX) bddapron/bddapron
	$(MAKEINDEX) bddapron
	$(LATEX) bddapron/bddapron
	$(LATEX) bddapron/bddapron

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

bddaprontop: bddapron.cma
	$(OCAMLMKTOP) -g -verbose $(OCAMLFLAGS) $(OCAMLINC) -o $@ \
	-custom -cc "$(CC)" cudd.cma camllib.cma \
	bigarray.cma gmp.cma apron.cma box.cma polka.cma bddapron.cma -cclib "-lboxMPQ -lpolkaMPQ"

example1.byte: bdd/example1.ml bdd.cma
	$(OCAMLC) -g $(OCAMLFLAGS) $(OCAMLINC) -o $@ -custom cudd.cma camllib.cma bdd.cma $<

example2.byte: bddapron/example2.ml bddapron.cma
	$(OCAMLC) -g $(OCAMLFLAGS) $(OCAMLINC) -o $@ -custom cudd.cma camllib.cma bigarray.cma gmp.cma apron.cma box.cma polka.cma bddapron.cma  -cclib "-lboxMPQ -lpolkaMPQ" $<

example1.opt: bddapron/example2.ml bddapron.cmxa
	$(OCAMLOPT) -verbose -g $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ cudd.cmxa camllib.cmxa bdd.cmxa $<

example2.opt: bddapron/example2.ml bddapron.cmxa
	$(OCAMLOPT) -verbose -g $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ cudd.cmxa camllib.cmxa bigarray.cmxa gmp.cmxa apron.cmxa box.cmxa polka.cmxa bddapron.cmxa -cclib "-lboxMPQ -lpolkaMPQ" $<

#--------------------------------------------------------------
# IMPLICIT RULES AND DEPENDENCIES
#--------------------------------------------------------------

.SUFFIXES: .ml .mli .cmi .cmo .cmx .tex
.PRECIOUS: $(BDDMOD:%=%.cmi) $(BDDMOD:%=%.cmo) $(BDDMOD:%=%.cmx) $(BDDAPRONMOD:%=%.cmi) $(BDDAPRONMOD:%=%.cmo) $(BDDAPRONMOD:%=%.cmx)

#-----------------------------------
# CAML
#-----------------------------------

bdd/%.cmi: bdd/%.mli
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -I bdd -c $<

bdd/%.cmo: bdd/%.ml
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -I bdd -c $<

bdd/%.cmx: bdd/%.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -I bdd -for-pack Bdd -c $<

bddapron/%.cmi: bddapron/%.mli bdd.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -I bddapron -c $<

bddapron/%.cmo: bddapron/%.ml bdd.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -I bddapron -c $<

bddapron/%.cmx: bddapron/%.ml bdd.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -I bddapron -for-pack Bddapron -c $<

bddapron/%.ml: bddapron/%.mll
	$(OCAMLLEX) $^

bddapron/%.ml bddapron/%.mli: bddapron/%.mly
	$(OCAMLYACC) $^

depend: bddapron/yacc.ml bddapron/yacc.mli bddapron/lex.ml
	$(OCAMLDEP) $(OCAMLINC) -I bdd $(BDDMOD:%=%.mli) $(BDDMOD:%=%.ml) >Makefile.depend
	$(OCAMLDEP) $(OCAMLINC) -I bddapron $(BDDAPRONMOD:%=%.mli) $(BDDAPRONMOD:%=%.ml) >>Makefile.depend

Makefile.depend: bddapron/yacc.ml bddapron/yacc.mli bddapron/lex.ml
	$(OCAMLDEP) $(OCAMLINC) -I bdd $(BDDMOD:%=%.mli) $(BDDMOD:%=%.ml) >Makefile.depend
	$(OCAMLDEP) $(OCAMLINC) -I bddapron $(BDDAPRONMOD:%=%.mli) $(BDDAPRONMOD:%=%.ml) >>Makefile.depend

-include Makefile.depend
