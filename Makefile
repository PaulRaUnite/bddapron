include Makefile.config

#---------------------------------------
# Directories
#---------------------------------------

SRCDIR = $(shell pwd)
#
# Installation directory prefix
#
SITE-LIB = $(shell $(OCAMLFIND) printconf destdir)
PKG-NAME = bddapron
SITE-LIB-PKG = $(SITE-LIB)/$(PKG-NAME)

#---------------------------------------
# CAML part
#---------------------------------------

BDD_REQ_PKG = "camllib cudd"
BDDAPRON_REQ_PKG = "camllib cudd gmp apron"

OCAMLINC = 

BDDMOD = output reg env int enum cond expr0 expr1 domain0 domain1
BDDMOD := $(BDDMOD:%=bdd/%)
BDDAPRONMOD = \
	apronexpr env cond apronexprDD apronDD \
	expr0 expr1 expr2 \
	descend mtbdddomain0 bddleaf bdddomain0 domain0 \
	domainlevel1 mtbdddomain1 bdddomain1 domain1 \
	syntax yacc lex parser
BDDAPRONMOD := $(BDDAPRONMOD:%=bddapron/%)

MLMODULES = $(BDDMOD) $(BDDAPRONMOD)

FILES_TOINSTALL = META \
	bdd.cmi bdd.cma bddapron.cmi bddapron.cma \
	bdd.cmx bdd.cmxa bdd.a bddapron.cmx bddapron.cmxa bddapron.a \
	bdd.p.cmx bdd.p.cmxa bdd.p.a bddapron.p.cmx bddapron.p.cmxa bddapron.p.a

ifneq ($(OCAMLPACK),)
FILES_TOINSTALL += bdd_ocamldoc.mli bddapron_ocamldoc.mli
endif

#---------------------------------------
# Rules
#---------------------------------------

# Global rules
all: $(FILES_TOINSTALL)

byte: bdd.cma bddapron.cma
opt: bdd.cmxa bddapron.cmxa

bddapron.cma: bddapron.cmo 
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) $(OCAMLINC) -package $(BDDAPRON_REQ_PKG) \
	-a -o $@ $<
bddapron.cmxa: bddapron.cmx 
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(OCAMLINC) -package $(BDDAPRON_REQ_PKG) \
	-a -o $@ $<
	$(RANLIB) bddapron.a
bddapron.p.cmxa: bddapron.p.cmx 
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(OCAMLINC) -package $(BDDAPRON_REQ_PKG) \
	-a -o $@ $<
	$(RANLIB) bddapron.p.a
bdd.cma: bdd.cmo
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) $(OCAMLINC) -package $(BDD_REQ_PKG) -a -o $@ $^
bdd.cmxa: bdd.cmx
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(OCAMLINC) -package $(BDD_REQ_PKG) -a -o $@ $^
	$(RANLIB) bdd.a
bdd.p.cmxa: bdd.p.cmx
	$(OCAMLFIND) ocamlopt -p $(OCAMLOPTFLAGS) $(OCAMLINC) -package $(BDD_REQ_PKG) -a -o $@ $^
	$(RANLIB) bdd.p.a

bddapron.cmo bddapron.cmi: $(BDDAPRONMOD:%=%.cmo)
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) $(OCAMLINC) -package $(BDDAPRON_REQ_PKG) -pack -o bddapron.cmo $(BDDAPRONMOD:%=%.cmo)

bddapron.cmx: $(BDDAPRONMOD:%=%.cmx) 
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(OCAMLINC) -package $(BDDAPRON_REQ_PKG) -pack -o $@ $(BDDAPRONMOD:%=%.cmx)

bddapron.p.cmx: $(BDDAPRONMOD:%=%.p.cmx)
	$(OCAMLFIND) ocamlopt -p $(OCAMLOPTFLAGS) $(OCAMLINC) -package $(BDDAPRON_REQ_PKG) -pack -o $@ $(BDDAPRONMOD:%=%.p.cmx)

bdd.cmo bdd.cmi: $(BDDMOD:%=%.cmo)
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) $(OCAMLINC) -package $(BDD_REQ_PKG) -pack -o bdd.cmo $^

bdd.cmx: $(BDDMOD:%=%.cmx)
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(OCAMLINC) -package $(BDD_REQ_PKG) -pack -o $@ $^

bdd.p.cmx: $(BDDMOD:%=%.p.cmx)
	$(OCAMLFIND) ocamlopt -p $(OCAMLOPTFLAGS) $(OCAMLINC) -package $(BDD_REQ_PKG) -pack -o $@ $^


META: Makefile
	/bin/rm -f META
	echo "\n\
description = \"Logico-numerical domain(s) based on BDDs and APRON\" \n\
version = \"2.1.0\" \n\
requires = \"bddapron.bdd\" \n\
package \"bdd\" ( \n\
requires = \"camllib cudd\" \n\
archive(byte) = \"bdd.cma\" \n\
archive(native) = \"bdd.cmxa\" \n\
archive(native,gprof) = \"bdd.p.cmxa\" \n\
) \n\
package \"bddapron\" ( \n\
requires = \"bddapron.bdd gmp apron\" \n\
archive(byte) = \"bddapron.cma\" \n\
archive(native) = \"bddapron.cmxa\" \n\
archive(native,gprof) = \"bddapron.p.cmxa\" \n\
) \n\
" >META

install: $(FILES_TOINSTALL)
	$(OCAMLFIND) remove $(PKG-NAME)
	$(OCAMLFIND) install $(PKG-NAME) $^

uninstall:
	$(OCAMLFIND) remove $(PKG-NAME)

distclean: clean
	/bin/rm -f Makefile.depend TAGS

clean:
	/bin/rm -f *.log *.aux *.bbl *.blg *.toc **.idx *.ilg *.ind ocamldoc*.tex ocamldoc.sty *.dvi *.pdf *.out bddapron.tex
	/bin/rm -fr tmp html index.html
	/bin/rm -f bddtop bddaprontop *.byte *.opt bdd_ocamldoc.mli bddapron_ocamldoc.mli
	for i in . bdd bddapron; do \
		cd $(SRCDIR)/$$i; \
		/bin/rm -f *.[aoc] *.cm[ioxa] *.cmxa *.annot; \
	done
	(cd bddapron; /bin/rm -f yacc.ml yacc.mli lex.ml)


# TEX rules
.PHONY: bddapron.dvi bddapron.pdf bdd.dvi bdd.pdf html html_bdd html_bddapron depend
.PRECIOUS: %.cma %.cmo %.cmi %.cmx 
.PRECIOUS: bdd.cma bdd.cmi bdd.cmo bdd.cmx bdd/%.cmi bdd/%.cmo bdd/%.cmx 
.PRECIOUS: bddapron.cma bddapron.cmi bddapron.cmo bddapron.cmx bddapron/%.cmi bddapron/%.cmo bddapron/%.cmx 
.PRECIOUS: bddapron/yacc.ml bddapron/yacc.mli bddapron/lex.ml

DOCLATEX =\
-latextitle 1,part\
-latextitle 2,chapter\
-latextitle 3,section\
-latextitle 4,subsection\
-latextitle 5,subsubsection\
-latextitle 6,paragraph\
-latextitle 7,subparagraph\
-noheader -notrailer

BDDDOCOCAMLINC =\
-I $(shell $(OCAMLFIND) query camllib) \
-I $(shell $(OCAMLFIND) query cudd)
BDDAPRONDOCOCAMLINC = $(BDDDOCOCAMLINC) \
-I $(shell $(OCAMLFIND) query gmp) \
-I $(shell $(OCAMLFIND) query apron)

bdd_ocamldoc.mli: bdd/introduction.odoc $(BDDMOD:%=%.mli)
	(cd bdd; ocamlpack -o ../bdd_ocamldoc -title "Finite-type expressions/properties on top of CUDD" -intro introduction.odoc -intf $(BDDMOD:bdd/%=%))
bddapron_ocamldoc.mli: bddapron/introduction.odoc $(BDDAPRONMOD:%=%.mli)
	(cd bddapron; ocamlpack -o ../bddapron_ocamldoc -title "Finite \& numerical expressions/properties on top of CUDD \& APRON" -intro introduction.odoc -intf $(BDDAPRONMOD:bddapron/%=%))

bddapron.pdf: bddapron.dvi
	$(DVIPDF) bddapron.dvi bddapron.pdf

bddapron.dvi: bdd_ocamldoc.mli bddapron_ocamldoc.mli bdd.cmi bddapron.cmi
	mkdir -p tmp
	cp bdd_ocamldoc.mli tmp/bdd.mli
	cp bddapron_ocamldoc.mli tmp/bddapron.mli
	$(OCAMLDOC) $(OCAMLINC) \
-I $(shell $(OCAMLFIND) query gmp) \
-I $(shell $(OCAMLFIND) query cudd) \
-I $(shell $(OCAMLFIND) query apron) \
-I $(shell $(OCAMLFIND) query camllib) \
-t "BDDAPRON, version 2.1.0, 01/02/11" \
-latextitle 1,part -latextitle 2,chapter -latextitle 3,section -latextitle 4,subsection -latextitle 5,subsubsection -latextitle 6,paragraph -latextitle 7,subparagraph \
-latex -o ocamldoc.tex tmp/bdd.mli tmp/bddapron.mli
	$(SED) -e 's/\\documentclass\[11pt\]{article}/\\documentclass[10pt,twoside,a4paper]{book}\\usepackage{ae,fullpage,makeidx,fancyhdr}\\usepackage[ps2pdf]{hyperref}\\pagestyle{fancy}\\setlength{\\headheight}{2.8ex}\\setlength{\\footskip}{5ex}\\renewcommand{\\chaptermark}[1]{\\markboth{\\MakeUppercase{\\chaptername}\\ \\thechapter.\\ #1}{}}\\renewcommand{\\sectionmark}[1]{}\\setcounter{tocdepth}{2}\\setcounter{secnumdepth}{4}\\setlength{\\parindent}{0em}\\setlength{\\parskip}{0.5ex}\\sloppy\\makeindex\\author{Bertrand Jeannet}/' -e 's/\\end{document}/\\appendix\\printindex\\end{document}/' ocamldoc.tex >bddapron.tex
	$(LATEX) bddapron
	$(MAKEINDEX) bddapron
	$(LATEX) bddapron
	$(LATEX) bddapron

html: bdd_ocamldoc.mli bddapron_ocamldoc.mli bdd.cmi bddapron.cmi bddapron.odoc
	mkdir -p html
	mkdir -p tmp
	cp bdd_ocamldoc.mli tmp/bdd.mli
	cp bddapron_ocamldoc.mli tmp/bddapron.mli
	cp $(shell $(OCAMLFIND) query cudd)/cudd_ocamldoc.mli tmp/cudd.mli
	cp $(shell $(OCAMLFIND) query apron)/apron_ocamldoc.mli tmp/apron.mli
	$(OCAMLDOC) -html -d html -colorize-code -intro bddapron.odoc \
-I $(shell $(OCAMLFIND) query gmp) \
-I $(shell $(OCAMLFIND) query cudd) \
-I $(shell $(OCAMLFIND) query apron) \
-I $(shell $(OCAMLFIND) query camllib) \
$(shell $(OCAMLFIND) query gmp)/*.mli \
tmp/cudd.mli tmp/apron.mli \
$(patsubst %,$(shell $(OCAMLFIND) query apron)/%, box.mli oct.mli polka.mli ppl.mli polkaGrid.mli t1p.mli) \
$(shell $(OCAMLFIND) query camllib)/*.mli \
tmp/bdd.mli tmp/bddapron.mli

homepage: html bddapron.pdf
	hyperlatex index
	scp -r index.html html bddapron.pdf presentation-bddapron.pdf Changes avedon:/home/wwwpop-art/people/bjeannet/bjeannet-forge/bddapron
	ssh avedon chmod -R ugoa+rx /home/wwwpop-art/people/bjeannet/bjeannet-forge/bddapron

#---------------------------------------
# Test
#---------------------------------------

example1.byte: bdd/example1.ml 
	$(OCAMLFIND) ocamlc -g $(OCAMLFLAGS) $(OCAMLINC) -o $@ $< -package bddapron.bdd -linkpkg

example2.byte: bddapron/example2.ml bddapron.cma
	$(OCAMLFIND) ocamlc -g $(OCAMLFLAGS) $(OCAMLINC) -o $@ $< -package "bddapron.bddapron apron.boxMPQ apron.polkaMPQ" -linkpkg

test_random.byte: bddapron/test_random.ml bddapron.cma
	$(OCAMLFIND) ocamlc -g $(OCAMLFLAGS) $(OCAMLINC) -o $@ $< -package "bddapron.bddapron apron.boxMPQ apron.polkaMPQ" -linkpkg

example1.opt: bdd/example1.ml bdd.cmxa
	$(OCAMLFIND) ocamlopt -verbose -g $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ $< -package bddapron.bdd -linkpkg 

example2.opt: bddapron/example2.ml bddapron.cmxa
	$(OCAMLFIND) ocamlopt -verbose -g $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ $< -package "bddapron.bddapron apron.boxMPQ apron.polkaMPQ" -linkpkg

test_random.opt: bddapron/test_random.ml bddapron.cmxa
	$(OCAMLFIND) ocamlopt -verbose -g $(OCAMLOPTFLAGS) $(OCAMLINC) -o $@ cudd.cmxa camllib.cmxa bigarray.cmxa gmp.cmxa apron.cmxa boxMPQ.cmxa polkaMPQ.cmxa bddapron.cmxa -noautolink -ccopt "$(LCFLAGS)" -cclib "-lpolkaGrid_caml -lap_pkgrid -lap_ppl_caml -lap_ppl -lppl -lgmpxx -lpolkaMPQ_caml_debug -lpolkaMPQ_debug -loctMPQ_caml -loctMPQ -lboxMPQ_caml -lboxMPQ -lapron_caml_debug -lapron_debug -lgmp_caml -lmpfr -lgmp -cuddcaml.d -lcamlidl -lbigarray -lunix" $<

#--------------------------------------------------------------
# IMPLICIT RULES AND DEPENDENCIES
#--------------------------------------------------------------

.SUFFIXES: .ml .mli .cmi .cmo .cmx .tex
.PRECIOUS: $(BDDMOD:%=%.cmi) $(BDDMOD:%=%.cmo) $(BDDMOD:%=%.cmx) $(BDDAPRONMOD:%=%.cmi) $(BDDAPRONMOD:%=%.cmo) $(BDDAPRONMOD:%=%.cmx)

#-----------------------------------
# CAML
#-----------------------------------

bdd/%.cmi: bdd/%.mli
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) $(OCAMLINC) -package $(BDD_REQ_PKG) -I bdd -c $<

bdd/%.cmo: bdd/%.ml
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) $(OCAMLINC) -package $(BDD_REQ_PKG) -I bdd -c $<

bdd/%.cmx: bdd/%.ml
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(OCAMLINC) -package $(BDD_REQ_PKG) -I bdd -for-pack Bdd -c $<

bdd/%.p.cmx: bdd/%.ml
	$(OCAMLFIND) ocamlopt -p $(OCAMLOPTFLAGS) $(OCAMLINC) -package $(BDD_REQ_PKG) -I bdd -for-pack Bdd -c -o $@ $<

bddapron/%.cmi: bddapron/%.mli bdd.cmi
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) $(OCAMLINC) -package $(BDDAPRON_REQ_PKG) -I bddapron -c $<

bddapron/%.cmo: bddapron/%.ml bdd.cmi
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) $(OCAMLINC) -package $(BDDAPRON_REQ_PKG) -I bddapron -c $<

bddapron/%.cmx: bddapron/%.ml bdd.cmx
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(OCAMLINC) -package $(BDDAPRON_REQ_PKG) -I bddapron -for-pack Bddapron -c $<

bddapron/%.p.cmx: bddapron/%.ml bdd.p.cmx
	$(OCAMLFIND) ocamlopt -p $(OCAMLOPTFLAGS) $(OCAMLINC) -package $(BDDAPRON_REQ_PKG) -I bddapron -for-pack Bddapron -c -o $@ $<

bddapron/%.ml: bddapron/%.mll
	$(OCAMLLEX) $^

bddapron/%.ml bddapron/%.mli: bddapron/%.mly
	$(OCAMLYACC) $^

depend: bddapron/yacc.ml bddapron/yacc.mli bddapron/lex.ml
	$(OCAMLFIND) ocamldep $(OCAMLINC) -I bdd $(BDDMOD:%=%.mli) $(BDDMOD:%=%.ml) >Makefile.depend
	$(OCAMLFIND) ocamldep $(OCAMLINC) -I bddapron $(BDDAPRONMOD:%=%.mli) $(BDDAPRONMOD:%=%.ml) >>Makefile.depend

Makefile.depend: bddapron/yacc.ml bddapron/yacc.mli bddapron/lex.ml
	$(OCAMLFIND) ocamldep $(OCAMLINC) -I bdd $(BDDMOD:%=%.mli) $(BDDMOD:%=%.ml) >Makefile.depend
	$(OCAMLFIND) ocamldep $(OCAMLINC) -I bddapron $(BDDAPRONMOD:%=%.mli) $(BDDAPRONMOD:%=%.ml) >>Makefile.depend

-include Makefile.depend
