<<<<<<< HEAD
MODULES= user stock init interaction stock_history portfolio game
=======
MODULES=interaction user stock init stock_history portfolio game cd cd_history
>>>>>>> 4f6014810bb1f70aa27cb4996be9f102895b5016
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