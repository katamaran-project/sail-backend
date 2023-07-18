test: generate
	diff types_and_literals.v expected/types_and_literals.v
	diff patterns.v expected/patterns.v
	diff arguments.v expected/arguments.v
	diff ifte.v expected/ifte.v
	diff let.v expected/let.v
	diff rec.v expected/rec.v
	diff anf.v expected/anf.v
	@echo done

generate:
	@sail -katamaran types_and_literals.sail -o types_and_literals
	@sail -katamaran patterns.sail -o patterns
	@sail -katamaran arguments.sail -o arguments
	@sail -katamaran ifte.sail -o ifte
	@sail -katamaran let.sail -o let
	@sail -katamaran rec.sail -o rec
	@sail -katamaran anf.sail -o anf

clean:
	rm types_and_literals.v
	rm patterns.v
	rm arguments.v
	rm ifte.v
	rm let.v
	rm rec.v
	rm anf.v