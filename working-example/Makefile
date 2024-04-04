SAIL:=sail
RUBY:=ruby
COQ:=coqc

SAIL_SOURCES = model.sail

all: microsail.v rename coq

.PHONY: microsail.v
microsail.v:
	$(SAIL) -katamaran -katamaran_config configuration.lisp -ddump_rewrite_ast intermediate $(SAIL_SOURCES)
#	$(SAIL) -katamaran -katamaran_config configuration.lisp $(SAIL_SOURCES)

.PHONY: rename
rename:
	$(RUBY) ../process-rewrites.rb

coq:
	$(COQ) $(COQ_FILE)

clean:
	git clean -fx
