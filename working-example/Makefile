SAIL:=sail
SAIL_SOURCES = model.sail


all: transform intermediate

transform:
	$(SAIL) -katamaran $(SAIL_SOURCES) -o microsail

intermediate:
	$(SAIL) -ddump_rewrite_ast intermediate $(SAIL_SOURCES)
