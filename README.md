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
| [NativeSem](https://github.com/programLyrique/nativesem) | Type inference for library functions in C (support for Fortran is planned). | Tested on several libraries, see [dashboard](https://prl-prg.github.io/r-typing/) |
| [RSTT](https://github.com/E-Sh4rk/rstt) | Set-theoretic type algebra for the R language. Defines the type parser and printer, and the operations on types (e.g. subtyping, substitution, constraint solving). | The main R data-structures are supported (atomic vectors, lists, functions, attributes and classes, etc.) |

Other external dependencies:

| Component | Description | State of advancement |
| --- | --- | --- |
| [MLsem](https://github.com/E-Sh4rk/MLsem) | A typing library for dynamic languages used by our type-checkers (both for R and C code). | Released |
| [SSTT](https://github.com/E-Sh4rk/sstt/) | A set-theoretic type library used to encode the R type algebra. | Released |

## Building and running

TypR drives Rsem and NativeSem as libraries, and all four projects -- TypR,
Rsem, NativeSem and RSTT -- are built together as a single dune workspace. The
three others are submodules, so the versions TypR is known to work with are
pinned here:

```bash
git submodule update --init --recursive
make build
make test
```

`make` sources `nativesem/setup-env.sh`, which locates tree-sitter in a
checkout of [r-parser](https://github.com/E-Sh4rk/r-parser) that has been built
(`make update && make setup && make`, then `opam pin add tree-sitter core/`).
It is looked up at `r-parser` and then `../r-parser`; set `R_PARSER_PATH` if
yours lives elsewhere. It is deliberately not a submodule: what the build needs
from it is `core/tree-sitter`, which its own `make setup` produces rather than
something a checkout provides. Everything else comes from the opam switch
(`opam install . --deps-only`).

```bash
# Report the dependencies TypR resolved, without type-checking
dune exec typr -- --deps path/to/package

# Type-check the package: native sources first, then the R sources
dune exec typr -- path/to/package

# Signatures of the functions the package builds upon (base R, imports)
dune exec typr -- --prelude base.R path/to/package
```

## How the pipeline works

1. **Discovery** (`lib/pkg.ml`) — the R sources under `R/`, the C sources under
   `src/`, and the `.fixes` prefix declared by `useDynLib` in `NAMESPACE`.
2. **Parsing** — each file is parsed exactly once, with the parser of the
   checker that owns the language: `Lang.Driver.parse` for R,
   `R_c_typing.Runner.parse_files` for C. The resulting ASTs are what gets
   handed to the type-checkers, so nothing is parsed twice.
3. **Dependency resolution** (`lib/r_deps.ml`) — walking the R AST gives, per
   top-level definition, the names it uses and the native routines it reaches
   through `.Call`/`.C`/`.Fortran`/`.External`. This replaces the regex scan
   NativeSem used to locate a package's entry points.
4. **Native side** — NativeSem types the C sources, using those entry points as
   the roots of its own call graph.
5. **Linking** (`lib/link.ml`) — a routine's C type is a function over an
   argument *tuple*; the corresponding R binding is a closure over an R
   argument record. TypR converts the type, binds it under the R-visible
   (prefixed) name, and rewrites `.Call(sym, a, b)` into `sym(a, b)` so that
   the R checker uses it. Only `.Call` is rewritten: `.C` and `.External` do
   not pass their arguments through unchanged, so the C type is not the type of
   the call.
6. **R side** — the R files are type-checked in dependency order, with those
   bindings in scope.

## Performance and effectiveness

We run a benchmark with typing performance and effectiveness with a dashboard automatically updated on each update on several R packages from CRAN that you can find in this repo: [r-typing](https://github.com/PRL-PRG/r-typing).

[**Dashboard**](https://prl-prg.github.io/r-typing/)

## Acknowledgments

This work is supported by the Czech Ministry of Education, Youth and Sports under program ERC-CZ, grant agreement LL2325, as well as by the Czech Science Foundation Grant No. 23-07580X.
