(lang dune 3.20)
(using menhir 3.0)

(name typr)

(generate_opam_files true)

(source (github PRL-PRG/typr))
(authors "Mickael Laurent" "Pierre Donat-Bouillud")
(maintainers "Mickael Laurent" "Pierre Donat-Bouillud")
(license MIT)

(package
 (name typr)
 (synopsis "Orchestrator of the TypR static type-checker for R")
 (description
  "Parses an R package (R sources and native C sources), resolves the\
 \n dependencies between their functions, and drives Rsem and NativeSem\
 \n accordingly.")
 (depends (ocaml (>= 5.3.0)) dune rsem r-c-typing rstt mlsem)
 (tags ("R" "types" "C")))
