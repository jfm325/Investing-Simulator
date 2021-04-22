MODULES= user stock init interaction stock_history portfolio game
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

zip:
	zip stock_simulator.zip *.ml* *.txt *.sh _tags .merlin .ocamlformat .ocamlinit Makefile	
	

clean:
	ocamlbuild -clean