(lang dune 3.6)

(generate_opam_files true)

(name bddapron)
(version 2.4.0)

(package
 (name bddapron)
 (authors "Bertrand Jeannet")
 (maintainers "Nicolas Berthier <m@nberth.space>")
 (synopsis "Logico-numerical domain(s) based on BDDs and APRON")
 (depends
  (ocaml
   (>= 4.03.0))
  (apron
   (>= 20151015))
  dune
  camllib
  stdlib-shims
  num
  (mlcuddidl
   (>= 2.3.0))
   mlgmpidl
  (odoc :with-doc)))
