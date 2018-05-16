all: expr evaluation miniml test

expr: expr.ml
		ocamlbuild -use-ocamlfind expr.byte

evaluation: evaluation.ml
		ocamlbuild -use-ocamlfind evaluation.byte

miniml: miniml.ml
		ocamlbuild -use-ocamlfind miniml.byte

test: test.ml 
		ocamlbuild -use-ocamlfind test.byte
clean:
	rm -rf _build *.byte