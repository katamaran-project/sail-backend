_=lists
SAIL_DIR=../../../sail
SAIL=sail -plugin ../../_build/default/src/sail_katamaran_backend/sail_plugin_katamaran.cmxs

help:
	$(SAIL) -h

ok:
	$(SAIL) -ok

check: $(_).sail
	$(SAIL) $(_).sail -just_check
 
ast: $(_).sail
	@$(SAIL) $(_).sail -output_sail 

interpreter: $(_).sail
	$(SAIL) $(_).sail -i

katamaran: $(_).sail
	$(SAIL) -katamaran $(_).sail -o $(_)

katamaran-list: $(_).sail
	$(SAIL) -katamaran $(_).sail -list_notations -o $(_)
 
katamaran-rewrites: $(_).sail 
	mkdir -p katamaran-rewrites
	$(SAIL) -katamaran $(_).sail -ddump_rewrite_ast katamaran-rewrites/$(_) -o katamaran_output
	cd katamaran-rewrites && bash ../../reorder_rewrites.sh $(_)

katamaran-rewrites-clean: katamaran-rewrites
	cd katamaran-rewrites && bash ../../clean_rewrites.sh

clean:
	-rm -f $(_).v
	-rm -rf katamaran-rewrites/

# all: c-emulator coq ocaml ast coq-rewrites c-rewrites
# 
# c-emulator: $(_)
# 
# run: $(_)
# 	./$(_) 
# 
# $(_): $(_).c
# 	gcc -O3 $(_).c $(SAIL_DIR)/lib/*.c -lgmp -lz -I $(SAIL_DIR)/lib/ -o $(_)
# 
# $(_).c: $(_).sail
# 	sail -c $(_).sail -o $(_)
# 
# coq: $(_).v
# 
# $(_).v: $(_).sail
# 	sail -coq $(_).sail -o $(_)
# 
# ocaml: $(_).sail
# 	sail -ocaml $(_).sail -o $(_)
# 
# 
# c-rewrites: $(_).sail 
# 	mkdir -p c-rewrites
# 	sail -c $(_).sail -ddump_rewrite_ast c-rewrites/$(_) -o $(_)
# 	cd c-rewrites && bash ../../reorder_rewrites.sh $(_)
# 
# c-rewrites-clean: c-rewrites
# 	cd c-rewrites && bash ../../clean_rewrites.sh
# 
# coq-rewrites: $(_).sail 
# 	mkdir -p coq-rewrites
# 	sail -coq $(_).sail -ddump_rewrite_ast coq-rewrites/$(_) -o $(_)
# 	cd coq-rewrites && bash ../../reorder_rewrites.sh $(_)
# 
# coq-rewrites-clean: coq-rewrites
# 	cd coq-rewrites && bash ../../clean_rewrites.sh
# 
# ocaml-rewrites: $(_).sail
# 	mkdir -p ocaml-rewrites
# 	sail -ocaml $(_).sail -ddump_rewrite_ast ocaml-rewrites/$(_) -o $(_)
# 	cd ocaml-rewrites && bash ../../reorder_rewrites.sh $(_)
# 
# ocaml-rewrites-clean: ocaml-rewrites
# 	cd ocaml-rewrites && bash ../../clean_rewrites.sh

# 	-rm -f $(_).c
# 	-rm -f $(_)
# 	-rm -f $(_).v
# 	-rm -f $(_)_types.v
# 	-rm -rf _sbuild
# 	-rm -rf coq-rewrites/
# 	-rm -rf c-rewrites/
# 	-rm -rf ocaml-rewrites/

