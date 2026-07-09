# TypR

TypR is a work-in-progress static type-checker for R.
For more information, you can take a look at the [poster](docs/poster.pdf) we presented at the conference useR! 2026.

TypR is not quite ready to be tested on real R codebases yet,
but you can star this repository if you want to stay updated!

## Structure of the project

TypR is composed of several components:

| Component | Description | State of advancement |
| --- | --- | --- |
| [TypR](https://github.com/PRL-PRG/typr) | Orchestrator. Parses R files and native libraries, resolve dependencies, and call the type-checkers. | Not testable yet |
| [Rsem](https://github.com/E-Sh4rk/Rsem) | Gradual type-checker for R functions. | Not testable yet (only works on basic examples) |
| [NativeSem](https://github.com/programLyrique/nativesem) | Type inference for library functions in C (support for Fortran is planned). | Tested on several libraries |
| [RSTT](https://github.com/E-Sh4rk/rstt) | Set-theoretic type algebra for the R language. Defines the type parser and printer, and the operations on types (e.g. subtyping, substitution, constraint solving). | The main R data-structures are supported (atomic vectors, lists, functions, attributes and classes, etc.) |

Other external dependencies:

| Component | Description | State of advancement |
| --- | --- | --- |
| [MLsem](https://github.com/E-Sh4rk/MLsem) | A typing library for dynamic languages used by our type-checkers (both for R and C code). | Released |
| [SSTT](https://github.com/E-Sh4rk/sstt/) | A set-theoretic type library used to encode the R type algebra. | Released |

## Performance and effectiveness

We run a benchmark with typing performance and effectiveness with a dashboard automatically updated on each update on several R packages from CRAN that you can find in this repo: [r-typing](https://github.com/PRL-PRG/r-typing).

[**Dashboard**](https://prl-prg.github.io/r-typing/)

## Acknowledgments

This work is supported by the Czech Ministry of Education, Youth and Sports under program ERC-CZ, grant agreement LL2325, as well as by the Czech Science Foundation Grant No. 23-07580X.
