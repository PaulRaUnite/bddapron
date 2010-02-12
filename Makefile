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
	bdd/env \
	bdd/int \
	bdd/enum \
	bdd/cond \
	bdd/expr0 bdd/expr1 \
	bdd/domain0 bdd/domain1

BDDAPRONMOD = \
	bddapron/apronexpr \
	bddapron/env \
	bddapron/cond \
	bddapron/apronexprDD \
	bddapron/apronDD \
	bddapron/expr0 bddapron/expr1 bddapron/expr2 \
	bddapron/descend \
	bddapron/mtbdddomain0 \
	bddapron/bddleaf bddapron/bdddomain0 bddapron/domain0 \
	bddapron/domainlevel1 \
	bddapron/mtbdddomain1 bddapron/bdddomain1 bddapron/domain1 \
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
		/bin/rm -fr html_bdd html_bddapron; \
		/bin/rm -f bddtop bddaprontop *.byte *.opt; \
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

bddapron.cmo bddapron.cmi: $(BDDAPRONMOD:%=%.cmo)
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -pack -o bddapron.cmo $^

bddapron.cmx: $(BDDAPRONMOD:%=%.cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -pack -o bddapron.cmx $^

bdd.cmo bdd.cmi: $(BDDMOD:%=%.cmo)
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -pack -o bdd.cmo $^

bdd.cmx: $(BDDMOD:%=%.cmx)
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -pack -o bdd.cmx $^


# TEX rules
.PHONY: bddapron.dvi bddapron.pdf bdd.dvi bdd.pdf html html_bdd html_bddapron depend
.PRECIOUS: %.cma %.cmo %.cmi %.cmx 
.PRECIOUS: bdd.cma bdd.cmi bdd.cmo bdd.cmx bdd/%.cmi bdd/%.cmo bdd/%.cmx 
.PRECIOUS: bddapron.cma bddapron.cmi bddapron.cmo bddapron.cmx bddapron/%.cmi bddapron/%.cmo bddapron/%.cmx 
.PRECIOUS: bddapron/yacc.ml bddapron/yacc.mli bddapron/lex.ml

bddapron.pdf: bddapron.dvi
	$(DVIPDF) bddapron.dvi bddapron.pdf

bddapron.dvi: $(BDDAPRONMOD:%=%.mli) 
	$(OCAMLDOC) -latextitle 1,chapter -latextitle 2,section -latextitle 3,subsection -latextitle 4,subsubsection -latextitle 5,paragraph -noheader -notrailer -latex -o ocamldoc.tex $(OCAMLINC) -I bddapron $(BDDAPRONMOD:%=%.mli)
	$(LATEX) bddapron/bddapron
	$(MAKEINDEX) bddapron
	$(LATEX) bddapron/bddapron
	$(LATEX) bddapron/bddapron

bdd.pdf: bdd.dvi
	$(DVIPDF) bdd.dvi bdd.pdf

bdd.dvi: $(BDDMOD:%=%.mli)
	$(OCAMLDOC) -latextitle 1,chapter -latextitle 2,section -latextitle 3,subsection -latextitle 4,subsubsection -latextitle 5,paragraph -noheader -notrailer -latex -o ocamldoc.tex $(OCAMLINC) -I bdd $(BDDMOD:%=%.mli)
	$(LATEX) bdd/bdd
	$(MAKEINDEX) bdd
	$(LATEX) bdd/bdd
	$(LATEX) bdd/bdd

html_bdd: $(BDDMOD:%=%.mli)
	mkdir -p html_bdd
	$(OCAMLDOC) -html -d html_bdd -colorize-code $(OCAMLINC) -I bdd -intro bdd/bdd.odoc $(BDDMOD:%=%.mli)

html_bddapron: $(BDDAPRONMOD:%=%.mli)
	mkdir -p html_bddapron
	$(OCAMLDOC) -html -d html_bddapron -colorize-code $(OCAMLINC) -I bddapron -intro bddapron/bddapron.odoc $(BDDAPRONMOD:%=%.mli)

html: html_bdd html_bddapron

homepage: html bdd.pdf bddapron.pdf
	hyperlatex index
	cp -r index.html html_bdd html_bddapron bdd.pdf bddapron.pdf Changes \
		$(HOME)/web/bjeannet-forge/bddapron
	chmod -R ugoa+rx $(HOME)/web/bjeannet-forge/bddapron
	scp -r $(HOME)/web/bjeannet-forge/bddapron johns:/home/wwwpop-art/people/bjeannet/bjeannet-forge
	ssh johns chmod -R ugoa+rx /home/wwwpop-art/people/bjeannet/bjeannet-forge/bddapron

#---------------------------------------
# Test
#---------------------------------------

bddtop: bdd.cma
	$(OCAMLMKTOP) -g -verbose $(OCAMLFLAGS) $(OCAMLINC) -o $@ \
	-custom -cc "$(CC)" cudd.cma camllib.cma bdd.cma

bddaprontop: bddapron.cma
	$(OCAMLMKTOP) -g -verbose $(OCAMLFLAGS) $(OCAMLINC) -o $@ \
	-custom -cc "$(CC)" cudd.cma camllib.cma \
	bigarray.cma gmp.cma apron.cma boxMPQ.cma octMPQ.cma polkaMPQ.cma bddapron.cma 

example1.byte: bdd/example1.ml bdd.cma
	$(OCAMLC) -g $(OCAMLFLAGS) $(OCAMLINC) -o $@ cudd.cma camllib.cma bdd.cma $<

example2.byte: bddapron/example2.ml bddapron.cma
	$(OCAMLC) -g $(OCAMLFLAGS) $(OCAMLINC) -o $@ cudd.cma camllib.cma bigarray.cma gmp.cma apron.cma boxMPQ.cma polkaMPQ.cma bddapron.cma $<

test_random.byte: bddapron/test_random.ml bddapron.cma
	$(OCAMLC) -g $(OCAMLFLAGS) $(OCAMLINC) -o $@ cudd.cma camllib.cma bigarray.cma gmp.cma apron.cma boxMPQ.cma polkaMPQ.cma bddapron.cma $<

example1.opt: bdd/example1.ml bdd.cmxa
	$(OCAMLOPT) -verbose -g $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ cudd.cmxa camllib.cmxa bdd.cmxa $<

example2.opt: bddapron/example2.ml bddapron.cmxa
	$(OCAMLOPT) -verbose -g $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ cudd.cmxa camllib.cmxa bigarray.cmxa gmp.cmxa apron.cmxa boxMPQ.cmxa polkaMPQ.cmxa bddapron.cmxa $<

test_random.opt: bddapron/test_random.ml bddapron.cmxa
	$(OCAMLOPT) -verbose -g $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ cudd.cmxa camllib.cmxa bigarray.cmxa gmp.cmxa apron.cmxa box.cmxa polka.cmxa bddapron.cmxa -noautolink -ccopt "$(LCFLAGS)" -cclib "-lpolkaGrid_caml -lap_pkgrid -lap_ppl_caml -lap_ppl -lppl -lgmpxx -lpolka_caml_debug -lpolkaMPQ_debug -loct_caml -loctMPQ -lbox_caml -lboxMPQ -lapron_caml_debug -lapron_debug -lgmp_caml -lmpfr -lgmp -lcamlidl_debug -lcudd_caml_debug -lcudd_debug -lmtr -lst -lutil -lepd -lbigarray -lunix" $<

LCFLAGS = \
-L$(GMP_PREFIX)/lib \
-L$(MPFR_PREFIX)/lib \
-L$(MLGMPIDL_PREFIX)/lib \
-L$(APRON_PREFIX)/lib \
-L$(PPL_PREFIX)/lib \
-L$(MLCUDDIDL_PREFIX)/lib \
-L$(CUDD_PREFIX)/lib \
-L$(CAML_PREFIX)/lib/ocaml \
-L$(CAMLIDL_PREFIX)/lib/ocaml

test2.opt: bddapron/test2.ml bddapron.cmxa
	$(OCAMLOPT) -verbose -g $(OCAMLOPTFLAGS) $(OCAMLINC) -g -o $@ cudd.cmxa camllib.cmxa bigarray.cmxa gmp.cmxa apron.cmxa box.cmxa polka.cmxa bddapron.cmxa -ccopt "$(LCFLAGS)" -noautolink -cclib "-lpolkaGrid_caml -lap_pkgrid -lap_ppl_caml -lap_ppl -lppl -lgmpxx -lpolka_caml_debug -lpolkaMPQ_debug -loct_caml -loctMPQ -lbox_caml -lboxMPQ -lapron_caml_debug -lapron_debug -lgmp_caml -lmpfr -lgmp -lcamlidl_debug -lcudd_caml_debug -lcudd_debug -lmtr -lst -lutil -lepd -lbigarray -lunix -lasmrun_debug" $<

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

depend: Makefile.depend

Makefile.depend: bddapron/yacc.ml bddapron/yacc.mli bddapron/lex.ml
	$(OCAMLDEP) $(OCAMLINC) -I bdd $(BDDMOD:%=%.mli) $(BDDMOD:%=%.ml) >Makefile.depend
	$(OCAMLDEP) $(OCAMLINC) -I bddapron $(BDDAPRONMOD:%=%.mli) $(BDDAPRONMOD:%=%.ml) >>Makefile.depend

-include Makefile.depend
