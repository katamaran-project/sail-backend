test:
	diff types_and_literals.v types_and_literals.expected
	diff patterns.v patterns.expected
	diff arguments.v arguments.expected

generate:
	sail -katamaran types_and_literals.sail -o types_and_literals
	sail -katamaran patterns.sail -o patterns
	sail -katamaran arguments.sail -o arguments

clean:
	rm types_and_literals.v
	rm patterns.v
	rm arguments.v