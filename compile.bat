@echo off
set appname=hello
if "%SETVARS_COMPLETED%"  neq "1"  (
    call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"
)
del /q build\* %appname%.exe
ifort src/*.f90 /nologo /MP /O2 /Qparallel /assume:buffered_io /Qipo /fpp /I\.\include\ /libs:static /threads /module:build\ /object:build\ /link /out:%appname%.exe
%appname%.exe