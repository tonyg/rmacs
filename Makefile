SYSTEM_LIBRARY_SUBPATH:=compiled/native/$(shell racket -e '(display (path->string (system-library-subpath)))')

all: $(SYSTEM_LIBRARY_SUBPATH)/tty-raw-extension.so

clean:
	rm -rf compiled

$(SYSTEM_LIBRARY_SUBPATH):
	mkdir -p $@

$(SYSTEM_LIBRARY_SUBPATH)/%.so: %.c $(SYSTEM_LIBRARY_SUBPATH)
	mzc --xform $*.c
	mzc --3m --cc $*.3m.c
	mzc --3m --ld $@ $*_3m.o
	rm -f $*.3m.c
	rm -f $*_3m.o
