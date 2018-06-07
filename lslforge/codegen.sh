mv -v eclipse/lslforge/src/lslforge/generated/Tuple2.java Tuple2.save
mv -v eclipse/lslforge/src/lslforge/generated/Tuple3.java Tuple3.save
rm -v eclipse/lslforge/src/lslforge/generated/*.java
mv -v eclipse/lslforge/src/lslforge/generated/*.save *.java

eclipse/lslforge-linux-x86/os/linux/x86/LslForge _CodeGen_ eclipse/lslforge/src/lslforge/generated lslforge.generated
