AS=xa
CC=cl65
CFLAGS=-ttelestrat
LDFILES=

PROGRAM=telemon31
SOURCE=src/telemon.asm

ASFLAGS=-C -W -e error.txt -l xa_labels.txt -DWITH_ACIA -DWITH_RAMOVERLAY

$(PROGRAM): $(SOURCE)
	$(AS) $(SOURCE) $(ASLAGS) -o $(PROGRAM).rom

test:
	xa tests/xrm.asm -o xrm
	xa tests/xmkdir.asm -o xmkdir
	mkdir build  
	mkdir -p build/usr/share/telemon/3.1/6502/
	mkdir -p build/usr/share/telemon/3.1/65c02/
	mkdir -p build/usr/share/telemon/3.1/65c816/
	mkdir -p build/usr/share/doc/telemon/
	cp $(PROGRAM).rom build/usr/share/telemon/3.1/6502/
	cp README.md build/usr/share/doc/telemon/
	cd build && tar -c * > ../$(PROGRAM).tar &&	cd ..
	filepack  $(PROGRAM).tar $(PROGRAM).pkg
	gzip $(PROGRAM).tar
	mv $(PROGRAM).tar.gz $(PROGRAM).tgz
	php buildTestAndRelease/publish/publish2repo.php $(PROGRAM).pkg ${hash} 6502 pkg official
	php buildTestAndRelease/publish/publish2repo.php $(PROGRAM).tgz ${hash} 6502 tgz official
	echo nothing
