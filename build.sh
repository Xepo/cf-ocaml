#!/bin/sh
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
main.ml
