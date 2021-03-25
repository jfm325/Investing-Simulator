TEST=test.byte
OBJECTS=$(MODULES:=.cmo)
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

build:
	$(OCAMLBUILD) $(OBJECTS)

clean:
	ocamlbuild -clean