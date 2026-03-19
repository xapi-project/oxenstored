DUNE 	= dune
JOBS    = $(shell getconf _NPROCESSORS_ONLN)
PROFILE = release

# K&R style indentation, could use in format target below
INDENT += -nbad -bap -nbc -br -brs -c33 -cd33 -ncdb -ce -ci4 -cli0
INDENT += -d0 -di1 -nfc1 -i4 -ip0 -l75 -lp -npcs
INDENT += -npsl -nsc -nsob

.PHONY: build check test clean format install

build:
	$(DUNE) build -j $(JOBS) --profile=$(PROFILE)

install: build
	$(DUNE) install oxenstored

check:
	dune build @check -j $(JOBS)

test:
	$(DUNE) runtest

deps:
	opam install . --deps-only

clean:
	$(DUNE) clean

utop:
	$(DUNE) utop

format:
	$(DUNE) build --auto-promote @fmt
	dune format-dune-file dune-project > $$$$ && mv $$$$ dune-project
	git ls-files '**/*.[ch]' | xargs -n1 indent -nut -i8
