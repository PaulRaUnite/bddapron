include Makefile.config

#---------------------------------------
# Directories
#---------------------------------------

SRCDIR = $(shell pwd)
#
# Installation directory prefix
#
PREFIX = $(FORMULA_PREFIX)

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

OCAMLINC = -I $(MLCUDDIDL_PREFIX)/lib -I $(MLGMPIDL_PREFIX)/lib -I $(CAMLLIB_PREFIX)/lib -I $(FORMULA_PREFIX)/lib -I $(APRON_PREFIX)/lib

MLMOD = \
apronexpr apronexprDD apronDD \
bddapronexpr bddaprondomain bddapronexprE bddaprondomainE

MLSRC = $(MLMOD:%=%.mli) $(MLMOD:%=%.ml)
MLINT = $(MLMOD:%=%.cmi)
MLOBJ = $(MLMOD:%=%.cmo)
MLOBJx = $(MLMOD:%=%.cmx)


MLLIB_TOINSTALL = $(MLSRC) $(MLINT) bddapron.cma
MLLIB_TOINSTALLx =  $(MLSRC) $(MLINT) bddapron.cmxa bddapron.a

#---------------------------------------
# Rules
#---------------------------------------

# Global rules
all: bddapron

bddapron: $(MLINT) bddapron.cma bddapron.cmxa

install:
	$(INSTALLd) $(PREFIX)/lib
	for i in $(MLLIB_TOINSTALL) $(MLLIB_TOINSTALLx); do \
		if test -f $$i; then $(INSTALL) $$i $(PREFIX)/lib; fi; \
	done

distclean:
	(cd $(PREFIX)/lib; /bin/rm -f $(MLLIB_TOINSTALL) $(MLLIB_TOINSTALLx))

clean:
	/bin/rm -f *.[aoc] *.cm[ioxa] *.cmxa *.annot
	/bin/rm -f *.log *.aux *.bbl *.blg *.toc **.idx *.ilg *.ind ocamldoc*.tex ocamldoc.sty *.dvi *.pdf
	/bin/rm -fr html
	/bin/rm -f bddapronrun bddaprontop

mostlyclean: clean
	/bin/rm -f Makefile.depend *.annot

tar: $(MLSRC) Makefile Makefile.config.model README bddapron.pdf html
	(cd ..; tar zcvf $(HOME)/bddapron.tgz $(^:%=bddapron/%))

# CAML rules
bddapron.cma: $(MLOBJ)
	$(OCAMLC) -a $(OCAMLFLAGS) $(OCAMLINC) -o $@ $^
bddapron.cmxa: $(MLOBJx)
	$(OCAMLOPT) -a $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ $^
	$(RANLIB) bddapron.a

# TEX rules
.PHONY: bddapron.dvi bddapron.pdf html depend

bddapron.pdf: bddapron.dvi
	$(DVIPDF) bddapron.dvi bddapron.pdf

bddapron.dvi: $(INT) $(MLMOD:%=%.mli)
	$(OCAMLDOC) -latextitle 1,chapter -latextitle 2,section -latextitle 3,subsection -latextitle 4,subsubsection -latextitle 5,paragraph -noheader -notrailer -latex -o ocamldoc.tex $(OCAMLINC) $(MLMOD:%=%.mli)
	$(LATEX) bddapron
	$(MAKEINDEX) bddapron
	$(LATEX) bddapron
	$(LATEX) bddapron

html: $(INT) $(MLMOD:%=%.mli)
	mkdir -p html
	$(OCAMLDOC) -html -d html -colorize-code $(OCAMLINC) $(MLMOD:%=%.mli)


#---------------------------------------
# Test
#---------------------------------------

bddapronrun: bddapron.cma
	$(OCAMLC) -verbose $(OCAMLFLAGS) $(OCAMLINC) -o $@ \
	-g -make-runtime -cc "$(CC)" bigarray.cma gmp.cma camllib.cma \
	apron.cma polka.cma cudd.cma formula.cma bddapron.cma -cclib "-lboxMPQ -lpolkaMPQ"

bddaprontop: bddapron.cma
	$(OCAMLMKTOP) -g -verbose $(OCAMLFLAGS) $(OCAMLINC) -o $@ \
	-custom -cc "$(CC)" bigarray.cma cudd.cma gmp.cma camllib.cma \
	apron.cma box.cma polka.cma formula.cma bddapron.cma -cclib "-lboxMPQ -lpolkaMPQ"

essai: essai.ml bddapron.cma bddapronrun
	$(OCAMLC) -g $(OCAMLFLAGS) $(OCAMLINC) -o $@ -use-runtime bddapronrun bigarray.cma cudd.cma gmp.cma camllib.cma apron.cma box.cma polka.cma formula.cma bddapron.cma essai.ml

essai.opt: essai.ml bddapron.cmxa
	$(OCAMLOPT) -verbose -g $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ bigarray.cmxa cudd.cmxa gmp.cmxa camllib.cmxa apron.cmxa box.cmxa polka.cmxa formula.cmxa bddapron.cmxa essai.ml -cc "$(CC)" -cclib "-lboxMPQ -lpolkaMPQ"

#--------------------------------------------------------------
# IMPLICIT RULES AND DEPENDENCIES
#--------------------------------------------------------------

.SUFFIXES: .ml .mli .cmi .cmo .cmx .tex

#-----------------------------------
# CAML
#-----------------------------------

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $<

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -c $<

depend:
	$(OCAMLDEP) $(MLSRC) >Makefile.depend

Makefile.depend:
	$(OCAMLDEP) $(MLSRC) >Makefile.depend

-include Makefile.depend
