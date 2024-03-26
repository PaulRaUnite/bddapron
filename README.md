# BDDApron
Author: Bertrand Jeannet

Website: https://pop-art.inrialpes.fr/people/bjeannet/bjeannet-forge/bddapron/index.html

**This repository just removes the restriction on compiler version related to camllib**

## About

This package contains two sublibraries/modules:

1. Bdd:
   
   a) Manipulation of formula and expressions involving Boolean,
      enumerated, bounded integer variables, using BDDs
   
   b) BDDs as an "abstract" domain

2. Bddapron extends Bdd with
   
   a) Combined finite-type and numerical expressions
   
   b) Abstract domain for finite type and numerical types,
      implemented as MTBDDs with Boolean decisions and APRON
      abstract values as leaves.

## Required

1) OCaml stuff
OCaml 3.12.1 or up (http://caml.inria.fr/)
Camlidl (http://caml.inria.fr/pub/old_caml_site/camlidl/)
FINDLIB (http://projects.camlcity.org/projects/findlib.html)
C/OCaml  libraries and interfaces
  MLCUDDIDL 2.3.2 (http://pop-art.inrialpes.fr/~bjeannet/mlxxxidl-forge/mlcuddidl/index.html)
  camllib 1.3.0 (http://http://pop-art.inrialpes.fr/~bjeannet/bjeannet-forge/camllib/)
2) C  libraries and interfaces
  GMP 3.x.y (http://gmplib.org)
  MPFR 5.x.y (http://www.mpfr.org/)
  MLGMPIDL 1.2.1 (http://pop-art.inrialpes.fr/~bjeannet/mlxxxidl-forge/mlgmpidl/index.html)
  APRON pre-0.9.11 (http://apron.cri.ensmp.fr/library/) (repository, trunk)


## To compile and install
`dune build`