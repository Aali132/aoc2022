.SUFFIXES:

DAYS := $(shell seq 1 19)
PROBLEMS := $(foreach day,$(DAYS),$(day)a $(day)b)
EXAMPLES := $(foreach day,$(DAYS),$(day)a_example $(day)b_example)

%/a.hs :
	@mkdir $* 2> /dev/null || true
	@cp template.hs $@

%/b.hs :
	@mkdir $* 2> /dev/null || true
	@cp template.hs $@

build/%a : %/a.hs aoc.hs
	@mkdir build/ 2> /dev/null || true
	@ghc aoc.hs $< -o $@

build/%b : %/b.hs aoc.hs
	@mkdir build/ 2> /dev/null || true
	@ghc aoc.hs $< -o $@

%a : build/%a %/input
	@echo -n "$@: "
	@$< $*/input

%b : build/%b %/input
	@echo -n "$@: "
	@$< $*/input

%a_example : build/%a %/example
	@echo -n "$@: "
	@$< $*/example

%b_example : build/%b %/example
	@echo -n "$@: "
	@$< $*/example

%/input :
	@echo "Enter input for day $*:"
	@cat > $@

%/example :
	@echo "Enter example for day $*:"
	@cat > $@

.PRECIOUS: %/a.hs %/b.hs %/input %/example build/%a build/%b

.PHONY: examples all clean

all: $(PROBLEMS)

examples: $(EXAMPLES)

clean:
	@rm -r build
