SAIL := sail


build: model.sail
	$(SAIL) -katamaran -ddump_rewrite_ast intermediate model.sail -o microsail

clean:
	rm -f intermediate_* microsail
