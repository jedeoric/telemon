@echo off

SET OSDKB="..\..\..\..\osdk\bin\"
SET ORICUTRON="..\..\..\..\oricutron\"


SET RELEASE="2_5"
SET UNITTEST="NO"

SET ORIGIN_PATH=%CD%

cd src

%OSDKB%\xa.exe -C -W  -e error.txt -l xa_labels.txt  telemon.asm 

..\md5sums a.o65 ..\original\telemon.rom

copy a.o65 ..\release\telemon%RELEASE%.rom


copy ..\release\telemon%RELEASE%.rom %ORICUTRON%\roms\telemon%RELEASE%.rom

IF "%UNITTEST%"=="NO" GOTO End
cd %ORICUTRON%
oricutron -mt 
:End
cd %ORIGIN_PATH%