SAIL:=sail
SAIL_SOURCES = model.sail


all: transform intermediate

transform:
	$(SAIL) -katamaran model.sail -o $(SAIL_SOURCES)

intermediate:
	$(SAIL) -ddump_rewrite_ast intermediate $(SAIL_SOURCES)
