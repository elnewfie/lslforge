@echo off

whoami /groups | find "S-1-16-12288" > nul

if %errorlevel% == 0 (
  stack exec strip C:\Users\User\AppData\Roaming\local\bin\LslForge.exe
  copy C:\Users\User\AppData\Roaming\local\bin\LslForge.exe eclipse\lslforge-win32-x86\os\win32\x86
  timeout /t 60
) else (
  echo. 
  echo You are NOT Administrator. Exiting...
  echo. 
  timeout /t 60
)

