SAIL:=sail
RUBY:=ruby
COQ:=coqc
C_OUT_DIR:=./build

SAIL_SOURCES = main.sail
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
	git clean -fdx

c_files: main.sail
	mkdir -p $(C_OUT_DIR) > /dev/null
	sail -c main.sail -o $(C_OUT_DIR)/main
