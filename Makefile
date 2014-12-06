PREFIX:=/usr/local
DOCLEAN=rm -f *.cmi *.cmi *.cmo *.o *.cmx *~

all: ocaml+twt ppcompose

ocaml+twt: ocaml+twt.ml
	ocamlc -o ocaml+twt str.cma unix.cma ocaml+twt.ml

ppcompose: ocaml+twt ppcompose.ml
	ocamlc -o ppcompose -pp ./ocaml+twt ppcompose.ml

test: ocaml+twt
	find examples/ -type f -name "*.ml*" | xargs -iXXX -n 1 bash -cex "ocamlc -safe-string -c -pp ./ocaml+twt XXX"

clean:
	$(DOCLEAN)
	cd examples; $(DOCLEAN)
	cd doc; rm -f *.log *~ *.aux
	rm -f ocaml+twt ppcompose

install: all
	cp ocaml+twt ppcompose $(DESTDIR)$(PREFIX)/bin
uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/ocaml+twt $(DESTDIR)$(PREFIX)/bin/ppcompose
