@echo off
cd src
..\..\..\osdk\bin\xa.exe -C -W -DHAVE_MINITEL=YES  -e error.txt -l xa_labels.txt  telemon.asm 

..\md5sums a.o65 ..\original\telemon.rom

copy a.o65 ..\release\telemon2_4.rom

..\..\..\osdk\bin\xa.exe -C -W -DHAVE_USBDRIVE=YES  -e error.txt -l xa_labels.txt  telemon.asm -o telemon2_5.rom

copy telemon2_5.rom ..\release\telemon2_5.rom
copy telemon2_5.rom ..\..\..\oricutron\roms\telemon2_5.rom
cd ..
 