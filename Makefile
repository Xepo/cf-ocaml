all: image.png

image.in: main
	./main

image.png: create_png image.in
	./create_png image.in image.png

#-unsafe \

SOURCES=pixel.ml  vec.ml  matrix.ml  rect.ml  outputtable.ml  context.ml  basic_shape.ml  renderable.ml  scene.ml  s1.ml
OBJECTS=$(SOURCES:.ml=.o)
CMXES=$(SOURCES:.ml=.cmx)
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


%.o: %.ml
	$(OPT) -c -o $@ $<

main: $(OBJECTS)
	$(OPT) -o main $(CMXES) 


create_png: create_png.cpp
	g++ -O3 create_png.cpp -o create_png `imlib2-config --libs` `imlib2-config --cflags`
