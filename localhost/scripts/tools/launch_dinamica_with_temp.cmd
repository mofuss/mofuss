@echo off
setlocal

rem Dinamica EGO 2.4 predates DINAMICA_TEMP_DIR. Scope Windows TEMP/TMP to
rem this launcher and its Dinamica child instead of changing them globally.
set "DINAMICA_TEMP=%TEMP%"
if exist "D:\dinTemp\" set "DINAMICA_TEMP=D:\dinTemp"
if defined MOFUSS_DINAMICA_TEMP_DIR set "DINAMICA_TEMP=%MOFUSS_DINAMICA_TEMP_DIR%"
if not exist "%DINAMICA_TEMP%\" mkdir "%DINAMICA_TEMP%" 2>nul
if not exist "%DINAMICA_TEMP%\" (
  echo Cannot create Dinamica temporary folder: %DINAMICA_TEMP%
  exit /b 1
)
set "TEMP=%DINAMICA_TEMP%"
set "TMP=%DINAMICA_TEMP%"

if /i "%~1"=="--probe" (
  echo Dinamica TEMP=%TEMP%
  echo Dinamica TMP=%TMP%
  exit /b 0
)

set "DINAMICA_HOME=%ProgramFiles%\Dinamica EGO"
if not exist "%DINAMICA_HOME%\DinamicaLauncher.exe" (
  echo Dinamica EGO launcher is missing: %DINAMICA_HOME%\DinamicaLauncher.exe
  exit /b 1
)
start "" /D "%DINAMICA_HOME%" "%DINAMICA_HOME%\DinamicaLauncher.exe" %*
exit /b %ERRORLEVEL%
