.PHONY: all clean
all:
	ocamlbuild -use-menhir main.native 
	mv main.native main
clean:
	$(RM) -rf _build
	$(RM) -rf build
