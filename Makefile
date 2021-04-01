MODULES=interaction user stock init 
TEST=test.byte
MAIN=main.byte
OBJECTS=$(MODULES:=.cmo)
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

install: 
	OCAMLRUNPARAM=b opam install -y ounit

play:
	$(OCAMLBUILD) -tag  'debug'  $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

build:
	$(OCAMLBUILD) $(OBJECTS)

clean:
	ocamlbuild -clean