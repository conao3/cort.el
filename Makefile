TOP        := $(dir $(lastword $(MAKEFILE_LIST)))
EMACS_RAW  := $(filter-out emacs-undumped, $(shell compgen -c emacs- | xargs))
ALL_EMACS  := $(strip $(sort $(EMACS_RAW)))

EMACS      ?= emacs

LOAD_PATH  := -L $(TOP)
ARGS       := -Q --batch $(LOAD_PATH)
BATCH      := $(EMACS) $(ARGS)

ALL_TESTS  := $(addprefix .make-debug-,$(ALL_EMACS))
ELS        := cort.el
ELCS       := $(ELS:.el=.elc)


##################################################

all: git-hook build

git-hook:
# cp git hooks to .git/hooks
	cp -a git-hooks/* .git/hooks/

build: $(ELCS)

%.elc: %.el
	@printf "Compiling $<\n"
	$(BATCH) -f batch-byte-compile $<

test: # build
# If byte compile for specific emacs,
# set specify EMACS such as `EMACS=emacs-26.1 make test`.
	$(MAKE) clean
	$(BATCH) -l cort-tests.el -f cort-run-tests

localtest: $(ALL_TESTS)
.make-debug-%:
	$(MAKE) clean
	EMACS=$* $(MAKE) test

clean:
	-find . -type f -name "*.elc" | xargs rm

include Makefunc.mk
