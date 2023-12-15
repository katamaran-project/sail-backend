SAIL:=sail
RUBY:=ruby
COQ:=coqc
C_OUT_DIR:=./build

SAIL_SOURCES = main.sail
COQ_FILE = microsail.v

all: microsail.v rename coq

microsail.v: $(SAIL_SOURCES)
	$(SAIL) -katamaran -katamaran_add_original -katamaran_include_untranslated -katamaran_include_ignored -ddump_rewrite_ast intermediate $(SAIL_SOURCES) -o $(COQ_FILE)

.PHONY: rename
rename:
	$(RUBY) ../rename-rewrites.rb

coq:
	$(COQ) $(COQ_FILE)

clean:
	git clean -fx

c_files: main.sail
	mkdir -p $(C_OUT_DIR) > /dev/null
	sail -c main.sail -o $(C_OUT_DIR)/main
