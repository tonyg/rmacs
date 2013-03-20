SYSTEM_LIBRARY_SUBPATH:=compiled/native/$(shell racket -e '(display (path->string (system-library-subpath)))')
SYSTEM_LIBRARY_SUFFIX:=$(shell racket -e '(begin (require dynext/file) (display (path->string (append-extension-suffix "DUMMY"))))' | sed -e s:DUMMY::)

all: $(SYSTEM_LIBRARY_SUBPATH)/tty-raw-extension$(SYSTEM_LIBRARY_SUFFIX)

clean:
	rm -rf compiled

$(SYSTEM_LIBRARY_SUBPATH):
	mkdir -p $@

$(SYSTEM_LIBRARY_SUBPATH)/%$(SYSTEM_LIBRARY_SUFFIX): %.c $(SYSTEM_LIBRARY_SUBPATH)
	mzc --xform $*.c
	mzc --3m --cc $*.3m.c
	mzc --3m --ld $@ $*_3m.o
	rm -f $*.3m.c
	rm -f $*_3m.o
