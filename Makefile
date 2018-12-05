TOP        := $(dir $(lastword $(MAKEFILE_LIST)))
EMACS_RAW  := $(filter-out emacs-undumped, $(shell compgen -c emacs- | xargs))
ALL_EMACS  := $(strip $(sort $(EMACS_RAW)))

EMACS      ?= emacs

LOAD_PATH  := -L $(TOP)
ARGS       := -Q --batch $(LOAD_PATH)
BATCH      := $(EMACS) $(ARGS)

ELS        := cort.el
ELCS       := $(ELS:.el=.elc)

LOGFILE    := .make-check.log

##################################################

all: git-hook build

git-hook:
# cp git hooks to .git/hooks
	cp -a git-hooks/* .git/hooks/

build: $(ELCS)

%.elc: %.el
	@printf "Compiling $<\n"
	$(BATCH) -f batch-byte-compile $<

check: # build
# If byte compile for specific emacs,
# set specify EMACS such as `EMACS=emacs-26.1 make test`.
	$(MAKE) clean --no-print-directory
	$(BATCH) -l cort-tests.el -f cort-run-tests

allcheck: $(ALL_EMACS:%=.make-check-%)
	@echo ""
	@cat $(LOGFILE) | grep =====
	@rm $(LOGFILE)

.make-check-%:
	EMACS=$* $(MAKE) check --no-print-directory 2>&1 | tee -a $(LOGFILE)

# silent `localcheck' job
debug: $(ALL_EMACS:%=.make-debug-%)
	@echo ""
	@cat $(LOGFILE) | grep =====
	@rm $(LOGFILE)

.make-debug-%:
	EMACS=$* $(MAKE) check --no-print-directory 2>&1 >> $(LOGFILE)

clean:
	-find . -type f -name "*.elc" | xargs rm

include Makefunc.mk
