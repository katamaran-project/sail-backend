SAIL:=sail
SAIL_SOURCES = model.sail


all: transform intermediate

transform:
	$(SAIL) -katamaran $(SAIL_SOURCES) -o microsail.v

intermediate:
	$(SAIL) -ddump_rewrite_ast intermediate $(SAIL_SOURCES)

clean:
	rm -f intermediate_* microsail

