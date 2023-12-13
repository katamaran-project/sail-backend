SAIL:=sail
RUBY:=ruby
COQ:=coqc

SAIL_SOURCES = model.sail
COQ_FILE = microsail.v

all: microsail.v rename coq

microsail.v: $(SAIL_SOURCES)
	$(SAIL) -katamaran -katamaran_add_original -katamaran_include_untranslated -ddump_rewrite_ast intermediate $(SAIL_SOURCES) -o $(COQ_FILE)

.PHONY: rename
rename:
	$(RUBY) ../rename-rewrites.rb

coq:
	$(COQ) $(COQ_FILE)

clean:
	git clean -fx
