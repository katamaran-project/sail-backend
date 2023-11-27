SAIL:=sail
RUBY:=ruby
SAIL_SOURCES = model.sail


all: microsail.v rename

microsail.v: $(SAIL_SOURCES)
	$(SAIL) -katamaran -katamaran_add_original -katamaran_include_untranslated -ddump_rewrite_ast intermediate $(SAIL_SOURCES) -o microsail.v

.PHONY: rename
rename:
	$(RUBY) rename-rewrites.rb

clean:
	rm -f intermediate_* microsail.v
