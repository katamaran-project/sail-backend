test: generate
	@diff types_and_literals.v types_and_literals.expected
	@diff patterns.v patterns.expected
	@diff arguments.v arguments.expected
	@diff ifte.v ifte.expected
	@echo done

generate:
	@sail -katamaran types_and_literals.sail -o types_and_literals
	@sail -katamaran patterns.sail -o patterns
	@sail -katamaran arguments.sail -o arguments
	@sail -katamaran ifte.sail -o ifte

clean:
	rm types_and_literals.v
	rm patterns.v
	rm arguments.v
	rm ifte.v