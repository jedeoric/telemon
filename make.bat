@echo off

SET ORICUTRON="..\..\..\..\oricutron\"

SET RELEASE="30"
SET UNITTEST="NO"

SET ORIGIN_PATH=%CD%

cd src

del a.o65
For /f "tokens=1-4 delims=/ " %%a in ('date /t') do (set mydate=%%c-%%b-%%a)
For /f "tokens=1-2 delims=/:" %%a in ('time /t') do (set mytime=%%a:%%b)

REM OPTION for compiling : -DWITH_PRINTER  -DWITH_ACIA -DWITH_RAMOVERLAY  -WITH_FDC

SET MYDATE=%mydate% %mytime%
rem ADD WITH_FDC for FDC
%OSDK%\bin\xa.exe -C -W  -e error.txt -DWITH_ACIA -DWITH_RAMOVERLAY -l xa_labels.txt  telemon.asm 
..\md5sums a.o65
%OSDK%\bin\xa.exe -C -W  -e error.txt -DWITH_ACIA -DWITH_RAMOVERLAY -D__DATEBUILT__="%MYDATE%"  -l xa_labels.txt  telemon.asm 

rem %OSDK%\bin\xa.exe -C -W  -e error.txt -l xa_labels_for_atmos.txt   -DATMOS -DWITH_DEBUG -D__DATEBUILT__="%MYDATE%"  -o ..\release\telemon%RELEASE%_for_atmos.rom  telemon.asm 
rem %OSDK%\bin\xa.exe -C -W  -e error.txt -l xa_labels.txt -o telemon_noacia_nofdc.rom  telemon.asm 

IF "%1"=="NORUN" GOTO End

copy a.o65 ..\release\telemon%RELEASE%.rom
rem copy telemon_noacia.rom ..\release\telemon%RELEASE%_noacia.rom
rem copy telemon_noacia_nofdc.rom ..\release\telemon%RELEASE%_noacia_nofdc.rom
rem copy telemon30_for_atmos.rom ..\release\telemon%RELEASE%_for_atmos.rom


copy ..\release\telemon%RELEASE%.rom %ORICUTRON%\roms\telemon%RELEASE%.rom
rem copy ..\release\telemon%RELEASE%_noacia.rom %ORICUTRON%\roms\telemon%RELEASE%_noacia.rom
rem copy ..\release\telemon%RELEASE%_noacia_nofdc.rom %ORICUTRON%\roms\telemon%RELEASE%_noacia_nofdc.rom
rem copy ..\release\telemon%RELEASE%_for_atmos.rom %ORICUTRON%\roms\telemon%RELEASE%_for_atmos.rom


cd %ORICUTRON%
oricutronV4 -mt  --symbols "%ORIGIN_PATH%\src\xa_labels.txt"


:End
cd %ORIGIN_PATH%
%OSDK%\bin\MemMap "%ORIGIN_PATH%\src\xa_labels.txt" docs/memmap.html Telemon docs/telemon.css
