SBCL_BATCH_PROLOGUE = --disable-ldb --lose-on-corruption --noprint

SBCL_BATCH_EVALS = \
	--eval '(ql:quickload :falcon)' \
	--eval '(sb-ext:save-lisp-and-die "./falcon" :executable t :toplevel (function falcon::main))'

SBCL_BATCH_EPILOGUE = --quit

LISP_FILES = $(shell find . -name '*.lisp')
ASD_FILES = $(shell find . -name '*.asd')
SRC_FILES = $(LISP_FILES) $(ASD_FILES)

all: falcon

falcon: $(SRC_FILES)
	sbcl $(SBCL_BATCH_PROLOGUE) $(SBCL_BATCH_EVALS) $(SBCL_BATCH_EPILOGUE)

clean:
	rm -f falcon
