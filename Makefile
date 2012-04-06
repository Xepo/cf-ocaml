SOURCES=pixel.ml vec.ml matrix.ml rect.ml outputtable.ml context.ml basic_shape.ml renderable.ml scene.ml scene.mli
TESTSOURCES=$(SOURCES) test.ml
MAINSOURCES=$(SOURCES) s1.ml
ALLSOURCES=$(SOURCES) test.ml s1.ml

all: Makefile.fragment image.png

Makefile.fragment: $(ALLSOURCES)
	ocamldep $(ALLSOURCES) > $@

include Makefile.fragment

redo:
	./main 
	./create_png image.in image.png
	
image.in: main
	./main

image.png: image.in create_png
	./create_png image.in image.png

#-unsafe \

OPT=time ocamlfind ocamlopt \
	-I /opt/local/lib/ocaml/site-lib/core \
	/opt/local/lib/ocaml/site-lib/core/libcore.a \
	-thread \
	-package unix \
	-package threads \
	-package extlib  \
	-package dynlink \
	-package "bin_prot.syntax" \
	-package "sexplib.syntax" \
	-package "variantslib.syntax" \
	-package "fieldslib.syntax" \
	-package "comparelib.syntax" \
	-package "core" \
	-linkpkg

%.cmi: %.mli %.ml
	$(OPT) -c -o $@ $<

%.cmx: %.ml
	$(OPT) -c -o $@ $<

main: $(MAINSOURCES:.ml=.cmx)
	$(OPT) -o $@ $^

test: $(TESTSOURCES:.ml=.cmx)
	$(OPT) -o $@ $^

clean:
	rm -f *.cmo *.cmi *.cmx *.o image.in image.png

create_png: create_png.cpp
	g++ -g create_png.cpp -o create_png `imlib2-config --libs` `imlib2-config --cflags`
