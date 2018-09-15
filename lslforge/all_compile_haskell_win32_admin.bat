@echo off

whoami /groups | find "S-1-16-12288" > nul

if %errorlevel% == 0 (
  call all_compile_haskell_win32.bat
) else (
  echo. 
  echo You are NOT Administrator. Exiting...
  echo. 
  timeout /t 60
)
