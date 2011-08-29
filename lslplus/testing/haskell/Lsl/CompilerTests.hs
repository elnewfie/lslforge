{-# OPTIONS_GHC -XScopedTypeVariables #-}
{-# OPTIONS_GHC -XQuasiQuotes #-}
module Lsl.CompilerTests where

import qualified Control.Monad.State as S
import Data.List
import Debug.Trace

import Language.Lsl.Internal.Compiler
import Language.Lsl.Syntax
import Language.Lsl.Parse
import Language.Lsl.Render
import Language.Lsl.Internal.Optimize
import Language.Lsl.Internal.Pragmas
import Language.Lsl.QQ(lsl,lslm)
import Text.Here

import System
import System.FilePath

import Test.HUnit hiding (State)

basedir = getEnv "LSLPLUS_TEST_DATA"

assertValid :: String -> Validity a -> IO ()
assertValid msg (Right _) = return ()
assertValid msg (Left (CodeErrs ((err,msg1):_))) = assertFailure (msg ++ " -- " ++ msg1 ++ " (" ++ show err ++ ")")

assertInvalid msg (Right _) = assertFailure msg
assertInvalid msg (Left (CodeErrs ((_,s):_))) = 
    if isInfixOf "file does not exist" s then assertFailure "file does not exist!" else return ()

prePrep = do
    (lib,scripts) <- compile (False,[],[])
    return $ length lib
    
prepSingleScript s = do
    dir <- basedir
    let path = dir </> "scripts" </> s
    (lib,scripts) <- compile (False,[],[(s,path)])
    assertEqual "should only have one compiled script" 1 (length scripts)
    let [(_,script)] = scripts
    return script

prepSingleModule s = do
    num <- prePrep
    dir <- basedir
    let path = dir </> "modules" </> s
    (lib,scripts) <- compile (False,[(s,path)],[])
    assertEqual "nothing should be in scripts" 0 (length scripts)
    assertEqual ("should only have " ++ show (num + 1) ++ " compiled modules") (num + 1) (length lib)
    Just m <- return $ lookup s lib
    return m

liftValidity = either (Left . CodeErrs . (:[]) . ((,)Nothing)) (Right . id)

checkScriptStr' :: String -> Validity CompiledLSLScript
checkScriptStr' s = do
    parsedScript <- liftValidity $ parseScriptFromString s
    S.evalState (compileLSLScript parsedScript) emptyValidationState
    
checkScriptStr :: String -> Validity CompiledLSLScript
checkScriptStr s = do
    parsedScript <- liftValidity $ parseScriptFromString s
    compileLSLScript' [] parsedScript
    --return ()
 

compileLibraryFromStrings modules = do
    parsedModules :: [(String, LModule)] <- liftValidity $ mapM ( \ (id,txt) -> parseModuleFromString txt >>= return . ((,) id)) modules
    return $ compileLibrary parsedModules
    
compileScript modules script = do
    auglib <- compileLibraryFromStrings modules
    compileScriptWithLibrary auglib script
compileScriptWithLibrary auglib script = do
    parsedScript <- liftValidity $ parseScriptFromString script
    compileLSLScript' (libFromAugLib auglib) parsedScript
    
validScriptWithModules name modules script = TestLabel name $ TestCase $ do
    let result = compileScript modules script
    assertValid ("invalid!") result
    Right compiled <- return result
    let script' = (renderCompiledScript "" compiled)
    let result' = checkScriptStr script'
    assertValid ("rendered compiled script is invalid!") result'

invalidScriptWithModules name modules script = TestLabel name $ TestCase $ do
     lib <- return $ compileLibraryFromStrings modules
     assertValid ("invalid!") lib
     let Right lib' = lib
     assertAllValidLibrary lib'
     let result = compileScriptWithLibrary lib' script
     assertInvalid ("script should not have compiled") result
     
assertAllValidLibrary lib = mapM_ (\ (name,m) -> assertValid ("module " ++ name ++ " is invalid") m) lib

validScriptStr name s = TestLabel name $ TestCase $ do
    let result = checkScriptStr s
    assertValid (name ++ " is invalid!") result
    let Right compiled = result
    let s' = (renderCompiledScript "" compiled)
    let result' = checkScriptStr s'
    assertValid ("rendered compiled script is invalid!\n" ++ s' ++ "\n") result'
    
validScriptStr' name s = TestLabel name $ TestCase $ do
    let result = checkScriptStr s
    assertValid (name ++ " is invalid!") result
    let Right compiled = result
    let s' = (renderCompiledScript "" compiled)
    let result' = checkScriptStr' s'
    assertValid ("rendered compiled script is invalid!\n" ++ s' ++ "\n") result'
    
invalidScriptStr name s = TestLabel name $ TestCase $ do
    let result = checkScriptStr s
    assertInvalid (name ++ " should not have compiled!") result
     
validScript s = TestLabel s $ TestCase $ do
    script <- prepSingleScript s
    assertValid (s ++ " is invalid!") script
invalidScript s = TestLabel s $ TestCase $ do
    script <- prepSingleScript s
    assertInvalid (s ++ "should not have compiled!") script

validModule s = TestLabel s $ TestCase $ do
    m <- prepSingleModule s
    assertValid (s ++ " is invalid!") m
invalidModule s = TestLabel s $ TestCase $ do
    m <- prepSingleModule s
    assertInvalid (s ++ " should not have compiled!") m

tests' = TestList [validScriptStr' "shouldWork" shouldWork]

tests = TestList [validScriptStr "shouldWork" shouldWork,
                  validScript "3d_radar_rezzer.lslp",
                  validScript "3d_radar_scan_ball.lslp",
                  validScript "describe_chatter.lslp",
                  validModule "trim.lslm",
                  validModule "float2hex.lslm",
                  validScriptStr "touchHello" touchHello,
                  validScriptStr "goodButStrange" goodButStrange,
                  validScriptStr "goodVectorExpression" goodVectorExpression,
                  validScriptStr "goodVectorExpression1" goodVectorExpression1,
                  invalidScriptStr "badVectorExpression" badVectorExpression,
                  validScriptStr "goodRotationExpression" goodRotationExpression,
                  invalidScriptStr "badRotationExpression" badRotationExpression,
                  validScriptStr "shiftExpression" shiftExpression,
                  validScriptStr "goodGlobalExpression" goodGlobalExpression,
                  invalidScriptStr "badGlobalExpression" badGlobalExpression,
                  validScriptStr "chkOwnerSay" chkOwnerSay,
                  validScriptStr "chkSay" chkSay,
                  validScriptStr "chkRegionSay" chkRegionSay,
                  validScriptStr "castneg" "default { state_entry() { string s = (string) -1; }}",
                  validScriptStr "vecsub" "default { state_entry() { vector v = <1,2,3> - <1,2,3>; }}",
                  validScriptStr "anotherGoodGlobal" anotherGoodGlobal,
                  validScriptStr "minusTest1" "default {state_entry() { integer x=-1; }}",
                  validScriptStr "minusTest2" "default {state_entry() { integer x=-1; x = x---x;}}",
                  validScriptStr "minusTest3" "default {state_entry() { integer x=-1; x=-1;}}",
                  validScriptStr "minusTest4" "default {state_entry() { integer x=-1; x=1+-1;}}",
                  validScriptStr "minusTest5" "default {state_entry() { integer x=-1; x=1/-1;}}",
                  validScriptStr "minusTest6" "default {state_entry() { integer x=-1; x=1%-2;}}",
                  validScriptStr "minusTest7" "default {state_entry() { integer x=-1; x=1*-1;}}",
                  validScriptStr "minusTest8" "default {state_entry() { integer x=-1; x=x++-1;}}",
                  validScriptStr "minusTest9" "default {state_entry() { integer x=-1; x=x&&-1;}}",
                  validScriptStr "minusTest10" "default {state_entry() { integer x=-1; x=x||-1;}}",
                  validScriptStr "minusTest11" "default {state_entry() { integer x=-1; x=x^-1;}}",
                  validScriptStr "minusTest12" "default {state_entry() { integer x=-1; x=x|-1;}}",
                  validScriptStr "minusTest13" "default {state_entry() { integer x=-1; x=x&-1;}}",
                  validScriptStr "minusTest14" "default {state_entry() { integer x=-1; x+=-1;}}",
                  validScriptStr "minusTest15" "default {state_entry() { integer x=-1; x-=-1;}}",
                  validScriptStr "minusTest16" "default {state_entry() { integer x=-1; x/=-1;}}",
                  validScriptStr "minusTest17" "default {state_entry() { integer x=-1; x*=-1;}}"
                  ]
    
                  
chkPredef s = "default {\nstate_entry() {\n" ++ s ++ ";\n}\n}\n"

chkOwnerSay = chkPredef "llOwnerSay(\"Hello World\")"
chkSay = chkPredef "llSay(0,\"Hello World\")"
chkRegionSay = chkPredef "llRegionSay(0,\"Hello World\")"


touchHello = "default {\ntouch_start(integer num_detected) {\nllOwnerSay(\"hello\");\n}\n}"

goodButStrange = "default {state_entry() {integer l = 0;integer m = 0;integer n = 0;l = 1 + m = n + 2;vector v = <1,2,3>4>;}}"

goodVectorExpression = "default {state_entry() {vector v = <1.0,2.0,3.0>;}}"
goodVectorExpression1 = "default {state_entry() {vector v;v =<1.0,2.0,3.0>;}}"
badVectorExpression = "default {state_entry() {vector v = <1.0,2.0,\"3.0\">;}}"
goodRotationExpression = "default {state_entry() {rotation v = <1.0,2.0,3.0,4.0>;}}"
badRotationExpression = "default {state_entry() {rotation v = <1.0,2.0,\"3.0\",4.0>;}}"

shiftExpression = "default {state_entry() {integer i = 2 << 8; }}"
goodGlobalExpression = "vector v = <1.0, 2.0, 3.0>;default {state_entry() { }}"
badGlobalExpression = "vector v = <1.0, 2.0, 3.0 + 1.0>;default {state_entry() { }}"

anotherGoodGlobal = "integer i = 0; integer j = i; default {state_entry() {}}"

shouldWork = "default{state_entry(){integer count=0;llSay(0,(string)count++);}}"

mod0 = "$module (integer DEBUG) debug(string msg) { if (DEBUG) llOwnerSay(msg); }"

modScript0 = "integer dbg = FALSE; $import debug(DEBUG=dbg); default { state_entry() { debug(\" hello \"); }}"

importTest0 = validScriptWithModules "importTest1" [("debug",mod0)] modScript0

mod1 = "$module (integer DEBUG) $import debug(DEBUG=DEBUG); integer foo = DEBUG;"
modScript1 = "integer dbg = FALSE; $import debug(DEBUG=dbg); $import mod1(DEBUG=dbg); default { state_entry() { debug(\" hello \"); }}"

importTest1 = validScriptWithModules "importTest1" [("debug",mod0),("mod1",mod1)] modScript1

importTest2 = validScriptWithModules "importTest2" [("debug",mod0),("mod1",mod1)] 
    "integer dbg = FALSE; $import mod1(DEBUG=dbg); $import debug(DEBUG=dbg);  default { state_entry() { debug(\" hello \"); }}"
    
mod2 = "$module () integer deb; $import debug(DEBUG=deb);"

importTest3 = invalidScriptWithModules "importTest3" [("debug",mod0),("mod1",mod1),("mod2",mod2)] 
    "integer dbg = FALSE; $import mod1 (DEBUG=dbg); $import mod2 () ; default { state_entry() { debug(\"hello \"); }}"

mod3 = "$module (integer bar) $import mod1(DEBUG=bar); doit() { debug(\" hello \"); }"
importTest4 = validScriptWithModules "importTest4" [("debug",mod0),("mod1",mod1),("mod3",mod3)] 
    "integer dbg = FALSE; $import mod1 (DEBUG=dbg); $import mod3 (bar=dbg); $import debug(DEBUG=dbg); default { state_entry() { debug(\"hello \"); }}"
importTests = TestLabel "Module System Tests" $ TestList [
    importTest0,
    importTest1,
    importTest2,
    importTest3,
    importTest4]

labelScript = [$here|
default {
    state_entry() {
        if (llGetStartParameter()) {
            float _ret0;
            integer i;
            for ((i = 0); (i < 5); i++) if ((i == 2)) {
                (_ret0 = llFrand(1.0));
                jump _end1;
            }
            (_ret0 = -1.0);
            @_end1;
            float foo = _ret0;
        }
        else  {
            float _ret0;
            integer i;
            for ((i = 0); (i < 5); i++) if ((i == 2)) {
                (_ret0 = llFrand(2.0));
                jump _end1;
            }
            (_ret0 = -1.0);
            @_end1;
            float bar = _ret0;
        }
    }
}|]

labelTest = invalidScriptStr "Label Test" labelScript

retconv = [$here|
    float foo() {
       integer i = 0;
       return i;
    } 
    
    default {
        state_entry() {
            llOwnerSay((string)foo());
        }
    }|]
    
casts = [$here|
    default {
        state_entry() {
            list l = (list) 1;
            l = (list) 1.1;
            l = (list) ((key)"");
            l = (list) <1,2,3>;
            l = (list) ZERO_ROTATION;
        }
    }|]
    
floatWierdness = [$here|
    default {
        state_entry() {
            float f = 1.0;
            llOwnerSay((string)(f*.2));
        }
    }|]
    
moreFloatWierdness = [$here|
    default {
        state_entry() {
            float f = .0;
            vector v = <.0,.0,0.>;
            
      //      llOwnerSay((string)v);
        }
    }|]
    
allTests = TestLabel "All Tests" $ TestList [
    tests,
    importTests,
    labelTest,
    validScriptStr "return implicit conversion test" retconv,
    validScriptStr "casts to list" casts,
    validScriptStr "float wierdness" floatWierdness,
    validScriptStr "more float wierdness" moreFloatWierdness,
    validScriptStr "eq-not" eqNotScript ]

    
eqNotScript = [$here|
    default {
        state_entry() {
            integer x = 0;
            x =!0;
            llOwnerSay((string)x);
        }
    }|]