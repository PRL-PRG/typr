.PHONY: build test clean

SHELL := /bin/bash

# Rsem and NativeSem both link against tree-sitter; NativeSem's setup script
# derives the paths from an r-parser checkout. Its own default (next to
# NativeSem) is not the right one here, since NativeSem is a submodule, so look
# for r-parser inside TypR first and next to it otherwise. r-parser is not a
# submodule on purpose: what the build needs from it (core/tree-sitter) is
# produced by its own `make setup`, not by checking it out.
# Override by setting R_PARSER_PATH in the environment.
export R_PARSER_PATH ?= $(firstword \
  $(wildcard $(CURDIR)/r-parser) $(CURDIR)/../r-parser)

define WITH_ENV
	source ./nativesem/setup-env.sh && eval "$$(opam env)" && $(1)
endef

build:
	@$(call WITH_ENV,dune build)

test:
	@$(call WITH_ENV,dune runtest)

clean:
	@dune clean
