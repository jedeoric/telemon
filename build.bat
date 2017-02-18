@echo off


SET ORICUTRON="..\..\..\..\oricutron\"


SET RELEASE="30"
SET UNITTEST="NO"

SET ORIGIN_PATH=%CD%



cd src

del a.o65

%OSDK%\bin\xa.exe -C -W  -e error.txt -DWITH_ACIA -l xa_labels.txt  telemon.asm 
%OSDK%\bin\xa.exe -C -W  -e error.txt -l xa_labels.txt -o telemon_noacia.rom  telemon.asm 

..\md5sums a.o65

IF "%1"=="NORUN" GOTO End

copy a.o65 ..\release\telemon%RELEASE%.rom
copy telemon_noacia.rom ..\release\telemon%RELEASE%_noacia.rom


copy ..\release\telemon%RELEASE%.rom %ORICUTRON%\roms\telemon%RELEASE%.rom
copy ..\release\telemon%RELEASE%_noacia.rom %ORICUTRON%\roms\telemon%RELEASE%_noacia.rom


cd %ORICUTRON%
oricutronV4 -mt -d teledisks\stratsed.dsk
:End
cd %ORIGIN_PATH%