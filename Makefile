MODULES=interaction user stock init 
TEST=test.byte
MAIN=main.byte
OBJECTS=$(MODULES:=.cmo)
OCAMLBUILD=ocamlbuild -use-ocamlfind
# COREBUILD=corebuild -use-ocamlfind
TIMER=timer.byte

default: build
	OCAMLRUNPARAM=b utop

timer: 
	$(OCAMLBUILD) -tag 'debug' $(TIMER) && OCAMLRUNPARAM=b ./$(TIMER)

play:
	$(OCAMLBUILD) -tag  'debug'  $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

build:
	$(OCAMLBUILD) $(OBJECTS)

clean:
	ocamlbuild -clean