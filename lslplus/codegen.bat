@echo off

del eclipse\lslplus\src\lslplus\generated\*.java

haskell\dist\build\LslPlus\LslPlus.exe _CodeGen_ eclipse\lslplus\src\lslplus\generated lslplus.generated