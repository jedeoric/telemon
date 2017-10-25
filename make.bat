@echo off

SET ORICUTRON="..\..\..\oricutron\"

SET RELEASE="30"
SET UNITTEST="NO"

SET ORIGIN_PATH=%CD%



del a.o65
For /f "tokens=1-4 delims=/ " %%a in ('date /t') do (set mydate=%%c-%%b-%%a)
For /f "tokens=1-2 delims=/:" %%a in ('time /t') do (set mytime=%%a:%%b)

REM OPTION for compiling : -DWITH_PRINTER  -DWITH_ACIA -DWITH_RAMOVERLAY  -WITH_FDC

SET MYDATE=%mydate% %mytime%
rem %OSDK%\bin\xa.exe -C -W  -e error.txt -DWITH_ACIA -DWITH_RAMOVERLAY -l xa_labels.txt  src\telemon.asm 
%OSDK%\bin\xa.exe -C -W  -e error.txt -DWITH_ACIA -DWITH_RAMOVERLAY -D__DATEBUILT__="%MYDATE%"  -l xa_labels.txt  src\telemon.asm -o  telemon%RELEASE%.rom


rem Generate test
echo Generate test
mkdir  %ORICUTRON%\usbdrive\tests\
%OSDK%\bin\xa.exe tests/xfillm.asm -o tests\xfillm
%OSDK%\bin\xa.exe tests/xrm.asm -o tests\xrm
%OSDK%\bin\xa.exe tests/xmkdir.asm -o tests\xmkdir





IF "%1"=="NORUN" GOTO End
copy telemon%RELEASE%.rom %ORICUTRON%\roms\telemon%RELEASE%.rom
copy tests\xrm  %ORICUTRON%\usbdrive\tests\
copy tests\xfillm  %ORICUTRON%\usbdrive\tests\
copy tests\xmkdir  %ORICUTRON%\usbdrive\tests\
cd %ORICUTRON%
oricutronV5 -mt  --symbols "%ORIGIN_PATH%\xa_labels.txt"


:End
cd %ORIGIN_PATH%
%OSDK%\bin\MemMap "%ORIGIN_PATH%\src\xa_labels.txt" docs/memmap.html Telemon docs/telemon.css
