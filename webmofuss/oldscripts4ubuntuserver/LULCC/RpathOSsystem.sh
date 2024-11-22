# reg Query "HKLM\Hardware\Description\System\CentralProcessor\0" | find /i "x86" > NUL && set OS=32 || set OS=64
# setLocal EnableDelayedExpansion
# cd 
for d in [c]; do (
# for %%d in (c) do (
# for %%d in (c d e f g h i j k l m n o p q r s t u v w x y z) do (
	if [-e d: (
		for -f "tokens=* delims= " $$a in ('dir/b/s $$d:\R.exe 2^>nul') do ( 
export Rexe=$$a
# SET Rexe=%%a
		)
	)
)

#set word=i386
#set str="%Rexe%"
#if %OS%==32BIT (set str=%str:x64=!word!%)

#cd %~dp0
#cd TempTables

#@echo Here it is:
#if %OS%==32 echo Your operating system is 32bit.
#if %OS%==64 echo Your operating system is 64bit.
#@echo Your R.exe path is %str%
#@echo off
#@echo %str%> Rpath.txt

#@echo off
#@echo %OS%> OS_type.txt
#@echo.
#if "%Rexe%" == "" (
#color C 
#@echo UPS We couldn't find your R.exe path. Are you sure you installed R? 
#@echo You can install it from here: https://cran.rstudio.com/
#@echo Or follow closely and in order all steps in the User Manual.
#@echo If you did, please contact aghilardi@ciga.unam.mx

#) else (
#@echo Great. You will now be asked to select a country of interest.
#)

#rem cd %~dp0
#rem %str% CMD BATCH --vanilla --slave "%~dp0from_scratch.R"

