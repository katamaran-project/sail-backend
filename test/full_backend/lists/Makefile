_=lists
SAIL=sail -plugin ../../../_build/default/src/sail_katamaran_backend/sail_plugin_katamaran.cmxs

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
	$(SAIL) -katamaran $(_).sail -ddump_rewrite_ast katamaran-rewrites/$(_) -o $(_)
	cd katamaran-rewrites && bash ../../reorder_rewrites.sh $(_)

katamaran-rewrites-clean: katamaran-rewrites
	cd katamaran-rewrites && bash ../../clean_rewrites.sh

clean:
	-rm -f $(_).v
	-rm -rf katamaran-rewrites/

# coq: $(_).v
# 
# $(_).v: $(_).sail
# 	sail -coq $(_).sail -o $(_)
# 
# coq-rewrites: $(_).sail 
# 	mkdir -p coq-rewrites
# 	sail -coq $(_).sail -ddump_rewrite_ast coq-rewrites/$(_) -o $(_)
# 	cd coq-rewrites && bash ../../reorder_rewrites.sh $(_)
# 
# coq-rewrites-clean: coq-rewrites
# 	cd coq-rewrites && bash ../../clean_rewrites.sh

# 	-rm -f $(_).v
# 	-rm -f $(_)_types.v
# 	-rm -rf coq-rewrites/

