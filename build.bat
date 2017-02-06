@echo off


SET ORICUTRON="..\..\..\..\oricutron\"


SET RELEASE="30"
SET UNITTEST="NO"

SET ORIGIN_PATH=%CD%



cd src

del a.o65

%OSDK%\bin\xa.exe -C -W  -e error.txt -l xa_labels.txt  telemon.asm 

..\md5sums a.o65

IF "%1"=="NORUN" GOTO End

copy a.o65 ..\release\telemon%RELEASE%.rom


copy ..\release\telemon%RELEASE%.rom %ORICUTRON%\roms\telemon%RELEASE%.rom


cd %ORICUTRON%
oricutronV4 -mt -d teledisks\stratsed.dsk
:End
cd %ORIGIN_PATH%