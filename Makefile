PREFIX:=/usr/local
DOCLEAN=rm -f *.cmi *.cmi *.cmo *.o *.cmx *~

all: ocaml+twt ppcompose

ocaml+twt: ocaml+twt.ml
	ocamlc -o ocaml+twt str.cma unix.cma ocaml+twt.ml

ppcompose: ocaml+twt ppcompose.ml
	ocamlc -o ppcompose -pp ./ocaml+twt ppcompose.ml

ppx_nop: ppx_nop.ml
	ocamlfind ocamlc -package compiler-libs.common -o ppx_nop ocamlcommon.cma ppx_nop.ml

test: ocaml+twt ppx_nop
	find examples/ -type f -name "*.ml*" | xargs -t -n 1 ocamlc -safe-string -c -pp ./ocaml+twt -ppx ./ppx_nop

clean:
	$(DOCLEAN)
	cd examples; $(DOCLEAN)
	cd doc; rm -f *.log *~ *.aux
	rm -f ocaml+twt ppcompose ppx_nop

install: all
	cp ocaml+twt ppcompose $(DESTDIR)$(PREFIX)/bin
uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/ocaml+twt $(DESTDIR)$(PREFIX)/bin/ppcompose
