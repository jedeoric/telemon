@echo off


SET ORICUTRON="..\..\..\..\oricutron\"


SET RELEASE="30"
SET UNITTEST="NO"

SET ORIGIN_PATH=%CD%



cd src

del a.o65

%OSDK%\bin\xa.exe -C -W  -e error.txt -DWITH_ACIA -DWITH_FDC -DWITH_RAMOVERLAY -l xa_labels.txt  telemon.asm 
%OSDK%\bin\xa.exe -C -W  -e error.txt -l xa_labels_for_atmos.txt   -DATMOS -DWITH_DEBUG  -o ..\release\telemon%RELEASE%_for_atmos.rom  telemon.asm 
rem %OSDK%\bin\xa.exe -C -W  -e error.txt -l xa_labels.txt -o telemon_noacia_nofdc.rom  telemon.asm 

..\md5sums a.o65

IF "%1"=="NORUN" GOTO End

copy a.o65 ..\release\telemon%RELEASE%.rom
rem copy telemon_noacia.rom ..\release\telemon%RELEASE%_noacia.rom
rem copy telemon_noacia_nofdc.rom ..\release\telemon%RELEASE%_noacia_nofdc.rom
rem copy telemon30_for_atmos.rom ..\release\telemon%RELEASE%_for_atmos.rom


copy ..\release\telemon%RELEASE%.rom %ORICUTRON%\roms\telemon%RELEASE%.rom
rem copy ..\release\telemon%RELEASE%_noacia.rom %ORICUTRON%\roms\telemon%RELEASE%_noacia.rom
rem copy ..\release\telemon%RELEASE%_noacia_nofdc.rom %ORICUTRON%\roms\telemon%RELEASE%_noacia_nofdc.rom
copy ..\release\telemon%RELEASE%_for_atmos.rom %ORICUTRON%\roms\telemon%RELEASE%_for_atmos.rom


cd %ORICUTRON%
oricutronV4 -mt -d teledisks\stratsed.dsk --symbols "%ORIGIN_PATH%\src\xa_labels.txt"
:End
cd %ORIGIN_PATH%