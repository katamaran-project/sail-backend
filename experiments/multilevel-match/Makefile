c:
	sail -c model.sail -o model --ddump-rewrite-ast intermediate
	./process-rewrites.rb

ocaml:
	sail -ocaml model.sail -o model --ddump-rewrite-ast intermediate
	./process-rewrites.rb

o: ocaml

clean:
	rm rewrite-*
	rm *.diff
