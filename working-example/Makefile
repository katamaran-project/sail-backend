SAIL:=sail
RUBY:=ruby
COQ:=coqc

SAIL_SOURCES = model.sail
COQ_FILE = microsail.v

all: microsail.v rename coq

.PHONY: microsail.v
microsail.v:
	$(SAIL) -katamaran -katamaran_config configuration.lisp -ddump_rewrite_ast intermediate $(SAIL_SOURCES) -o $(COQ_FILE)

.PHONY: rename
rename:
	$(RUBY) ../rename-rewrites.rb

coq:
	$(COQ) $(COQ_FILE)

clean:
	git clean -fx
