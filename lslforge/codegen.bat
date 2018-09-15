rem ren eclipse\lslforge\src\lslforge\generated\Tuple2.java Tuple2.save
rem ren eclipse\lslforge\src\lslforge\generated\Tuple3.java Tuple3.save
del eclipse\lslforge\src\lslforge\generated\*.java
rem ren eclipse\lslforge\src\lslforge\generated\*.save *.java

haskell\.stack-work\dist\ca59d0ab\build\LslForge\LSLForge.exe _CodeGen_ eclipse\lslforge\src\lslforge\generated lslforge.generated
