SAIL:=sail
SAIL_SOURCES = model.sail


all: microsail.v

microsail.v: $(SAIL_SOURCES)
	$(SAIL) -katamaran -katamaran_add_original -katamaran_include_untranslated -ddump_rewrite_ast intermediate $(SAIL_SOURCES) -o microsail.v

clean:
	rm -f intermediate_* microsail.v
