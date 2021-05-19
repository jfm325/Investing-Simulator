MODULES=author cd_history cd game index_history init interaction main portfolio	real_estate_history	stock_history stock user     
TEST=test.byte
MAIN=main.byte
OBJECTS=$(MODULES:=.cmo)
OCAMLBUILD=ocamlbuild -use-ocamlfind

MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)

default: build
	OCAMLRUNPARAM=b utop

install: 
	OCAMLRUNPARAM=b opam install -y ounit ANSITerminal

play:
	$(OCAMLBUILD) -tag  'debug'  $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

build:
	$(OCAMLBUILD) $(OBJECTS)

zip:
	zip stock_simulator.zip *.ml* *.txt *.sh _tags .merlin .ocamlformat .ocamlinit Makefile	

docs: docs-public docs-private
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package ANSITerminal \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package ANSITerminal \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private stock_simulator.zip