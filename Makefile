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
	mkdir -p build/usr/bin/
	mkdir -p build/usr/share/man
	mkdir -p build/usr/share/ipkg  
	echo nothing
  