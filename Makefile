AS=xa
CC=cl65
CFLAGS=-ttelestrat
LDFILES=

PROGRAM=telemon30
SOURCE=src/telemon.asm

ASFLAGS=-C -W -e error.txt -l xa_labels.txt -DWITH_ACIA

$(PROGRAM): $(SOURCE)
	$(AS) $(SOURCE) $(ASLAGS) -o $(PROGRAM).rom

test:
	mkdir -p build/usr/share/man
	mkdir -p build/usr/share/ipkg
	xa tests/xrm.asm -o xrm
	xa tests/xmkdir.asm -o xmkdir
	cp $(PROGRAM).rom build/
	cp src/man/$(PROGRAM).hlp build/usr/share/man
	cp src/ipkg/$(PROGRAM).csv build/usr/share/ipkg
	tar -c build/* > $(PROGRAM).tar
	filepack  $(PROGRAM).tar $(PROGRAM).pkg
	gzip $(PROGRAM).tar
	mv $(PROGRAM).tar.gz $(PROGRAM).tgz
	php buildTestAndRelease/publish/publish2repo.php $(PROGRAM).pkg ${hash} 6502 pkg
	php buildTestAndRelease/publish/publish2repo.php $(PROGRAM).tgz ${hash} 6502 tgz
	echo nothing
