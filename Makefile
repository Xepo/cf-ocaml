SOURCES=pixel.ml vec.ml generic_matrix.ml matrix.ml color.ml rect.ml outputtable.ml settings.ml basic_shape.ml renderable.ml world.ml scene.ml scene.mli 
TESTSOURCES=$(SOURCES) test.ml
MAINSOURCES=$(SOURCES) s1.ml 
ALLSOURCES=$(SOURCES) test.ml s1.ml

all: Makefile.fragment image.png

ccflags=`imlib2-config --cflags | sed -e 's/I/I /g'`
ldflags=-cclib `imlib2-config --libs | sed -e 's/ / -cclib /g'`

Makefile.fragment: $(ALLSOURCES)
	ocamldep $(ALLSOURCES) > $@

include Makefile.fragment

#-unsafe \

OCFINDPARAMS= -I /opt/local/lib/ocaml/site-lib/core \
	-thread \
	-inline 5 \
	-unsafe \
	-ccopt -O3 \
	-nodynlink \
	-package unix \
	-package threads \
	-package extlib  \
	-package dynlink \
	-package "bin_prot.syntax" \
	-package "sexplib.syntax" \
	-package "variantslib.syntax" \
	-package "fieldslib.syntax" \
	-package "comparelib.syntax" \
	-package "core"
OPT=ocamlfind ocamlopt $(OCFINDPARAMS) \
	/opt/local/lib/ocaml/site-lib/core/libcore.a \
	-linkpkg
DOC=ocamlfind ocamldoc -d "doc" -html $(OCFINDPARAMS)

.PHONY: doc
doc: 
	pwd
	echo $(OCFINDPARAMS)
	$(DOC) $(ALLSOURCES)

create_png.o: create_png.c
	ocamlopt -c  $(ccflags) $<

%.cmi: %.mli %.ml
	$(OPT) -c -o $@ $<

%.cmx: %.ml
	$(OPT) -c -o $@ $<

main: create_png.o $(MAINSOURCES:.ml=.cmx)
	$(OPT) $(ldflags) -o $@ $^
test: create_png.o $(TESTSOURCES:.ml=.cmx) 
	$(OPT) $(ldflags) -o $@  $^

redo:
	./main 
	
image.png: main
	time ./main

runtest: test
	./test

clean:
	rm -f *.cmo *.cmi *.cmx *.o image.png test.png

