# ─── Stage 1: Build ──────────────────────────────────────────────────────────
# Builds TypR together with its three submodules (Rsem, NativeSem, rstt) as one
# dune workspace. Only sstt and MLsem come from opam; rstt, rsem and r-c-typing
# are the submodules, so `opam install . --deps-only` is never run here -- it
# would fail (those three are not in the default repository) and, worse, would
# install an rstt next to the workspace one and shadow it.
FROM ocaml/opam:ubuntu-22.04-ocaml-5.3 AS builder

USER root
RUN apt-get update && apt-get install -y \
    build-essential curl bc git \
    libgmp-dev pkg-config \
    libssl-dev \
    nodejs \
    && rm -rf /var/lib/apt/lists/*

USER opam
WORKDIR /home/opam

# Rust is required by tree-sitter-cli during the r-parser build.
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
      | sh -s -- -y --no-modify-path
ENV PATH="/home/opam/.cargo/bin:${PATH}"

# Pins of the set-theoretic stack. Keep in sync with nativesem's ci.yml and
# Dockerfile: TypR builds NativeSem and Rsem in one process, so they must agree.
ARG SSTT_REF=b44200579ebf2522da24d7d7105bbf1699d1e9d2
ARG MLSEM_REF=7fae576697b3cf29e9331273d8a98ca797c9cfd3

RUN opam update --yes && eval $(opam env) && \
    opam pin add sstt      "git+https://github.com/E-Sh4rk/sstt.git#${SSTT_REF}" --yes --no-action --ignore-pin-depends && \
    opam pin add sstt-repl "git+https://github.com/E-Sh4rk/sstt.git#${SSTT_REF}" --yes --no-action --ignore-pin-depends && \
    opam pin add sstt-bin  "git+https://github.com/E-Sh4rk/sstt.git#${SSTT_REF}" --yes --no-action --ignore-pin-depends && \
    for pkg in mlsem-types mlsem-common mlsem-system mlsem-lang mlsem; do \
      opam pin add "$pkg" "git+https://github.com/E-Sh4rk/MLsem.git#${MLSEM_REF}" --yes --no-action --ignore-pin-depends; \
    done && \
    # Everything the four projects need that opam knows about, explicitly:
    # rstt-repl wants menhir, rstt zarith, r-c-typing ppx_inline_test and
    # (through its c-parser) atdgen; rstt's tests want ppx_expect; typr yojson.
    opam install --yes --ignore-pin-depends \
      ocamlfind dune cmdliner menhir zarith ppx_inline_test ppx_expect \
      yojson atdgen atdgen-runtime \
      sstt sstt-repl mlsem-types mlsem-common mlsem-system mlsem-lang mlsem

# r-parser provides tree-sitter. Its opam file has empty build/install
# sections: the pin only satisfies the dependency, the files come from its own
# `dune install`. That is why this block comes AFTER the last `opam install`
# above -- any later `opam install` may replace the entry with the public
# tree-sitter package, which has no `run` sublibrary.
RUN git clone "https://github.com/E-Sh4rk/r-parser.git" r-parser && \
    cd r-parser && \
    sed -i 's|git@github.com:|https://github.com/|' .gitmodules && \
    git submodule sync && \
    export CARGO_TARGET_DIR="$PWD/core/downloads/tree-sitter/target" && \
    opam exec -- bash -c "make update && make setup" && \
    opam exec -- bash -c "make" && \
    cd core && \
    opam exec -- bash -c "opam pin add tree-sitter . --kind=path --yes" && \
    opam exec -- dune install --prefix=$(opam var prefix) && \
    # Fail here, loudly, rather than at link time.
    test -f "$(opam var lib)/tree-sitter/dune-package" && \
    opam exec -- ocamlfind list | grep -q "tree-sitter.run"

ENV TREESITTER_INCDIR="/home/opam/r-parser/core/tree-sitter/include"
ENV TREESITTER_LIBDIR="/home/opam/r-parser/core/tree-sitter/lib"
ENV LD_LIBRARY_PATH="/home/opam/r-parser/core/tree-sitter/lib"

# The whole workspace, submodules included (.dockerignore keeps them).
COPY --chown=opam:opam . /home/opam/typr/
WORKDIR /home/opam/typr
# Not the default alias: that would pull in every .expected diff rule.
RUN eval $(opam env) && dune build bin/main.exe

# ─── Stage 2: Runtime ────────────────────────────────────────────────────────
FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC

# gcc + libc6-dev: the checker preprocesses the package's real `#include <...>`
# and asks the system compiler where its headers live.
# r-cran-jsonlite: r-typing's parse_output.R reads the JSONL report.
# universal-ctags: r-typing's per-package pages locate definitions with it.
RUN apt-get update && apt-get install -y \
    libstdc++6 bc git parallel \
    gcc libc6-dev \
    r-base r-cran-jsonlite universal-ctags \
    && rm -rf /var/lib/apt/lists/*

# Keep the _build/default/bin/main.exe path run_one_package.sh expects.
RUN mkdir -p /checker/_build/default/bin
COPY --from=builder /home/opam/typr/_build/default/bin/main.exe /checker/_build/default/bin/main.exe

# NativeSem's R-C-API signatures, found among the ancestors of the CWD; and the
# base R prelude the R side is checked against (Rsem's signatures).
COPY --from=builder /home/opam/typr/nativesem/types /checker/types
COPY --from=builder /home/opam/typr/Rsem/sigs/base.R /checker/prelude/base.R

COPY --from=builder /home/opam/r-parser/core/tree-sitter/lib/libtree-sitter.so* /usr/local/lib/
RUN ldconfig

# Consumed by scripts/run_one_package.sh in r-typing.
ENV CHECKER_DIR=/checker
ENV CHECKER=/checker/_build/default/bin/main.exe
ENV TS_LIB_DIR=/usr/local/lib
ENV TYPR_PRELUDE=/checker/prelude/base.R

# Provenance: which commits this image was built from, and whether the
# submodules were at their pinned SHAs or refreshed to their branches. The
# binary copies the environment variables into its `run` record; the file is
# for r-typing's record_checker_version.sh.
ARG TYPR_SHA=unknown
ARG RSEM_SHA=unknown
ARG NATIVESEM_SHA=unknown
ARG RSTT_SHA=unknown
ARG PINNED=true
ARG SUBJECT=
ENV TYPR_SHA=${TYPR_SHA} RSEM_SHA=${RSEM_SHA} NATIVESEM_SHA=${NATIVESEM_SHA} RSTT_SHA=${RSTT_SHA} TYPR_PINNED=${PINNED}
RUN printf '{"typr":"%s","rsem":"%s","nativesem":"%s","rstt":"%s","pinned":%s,"subject":"%s","built_at":"%s"}\n' \
      "$TYPR_SHA" "$RSEM_SHA" "$NATIVESEM_SHA" "$RSTT_SHA" "$PINNED" "$SUBJECT" "$(date -u +%FT%TZ)" \
      > /checker/provenance.json

WORKDIR /checker
