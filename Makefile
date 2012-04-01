all: image.png

image.in: main
	./main

image.png: create_png image.in
	./create_png image.in image.png

main: main.ml
	ocamlfind ocamlopt \
	-unsafe \
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
	-linkpkg \
	-o main \
	main.ml


create_png: create_png.cpp
	g++ -O3 create_png.cpp -o create_png `imlib2-config --libs` `imlib2-config --cflags`
