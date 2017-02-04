{-# OPTIONS_GHC -XQuasiQuotes #-}
module Lsl.CompilationServerTests where

import Data.List

import Language.Lsl.Internal.DOMCombinators
import Language.Lsl.Internal.SerializationGenerator
import Language.Lsl.Internal.CompilationServer
import Language.Lsl.Syntax
import System.Environment
import System.FilePath

import Test.HUnit hiding (State,Label)

import Text.Here

basedir = getEnv "LSLFORGE_TEST_DATA"

checkV :: JavaRep a => String -> IO a
checkV s = case parse elemDescriptor s of
     Left s -> assertFailure s >> return undefined
     Right Nothing -> assertFailure "couldn't find compilation status list" >> return undefined
     Right (Just v) -> do
         assertEqual "deserialize/serialize mismatch" s (xmlSerialize Nothing v "")
         return v

-- testInit = TestLabel "testInit" $ TestCase $ do
--     dir <- basedir
--     (_,s) <- handleCommand emptyCState (Init (False,[],[("simple.lslp",dir </> "scripts" </> "simple.lslp")]))
--     checkV s :: IO [CompilationStatus]
--     return ()

-- testUpdateScript = TestLabel "testUpdateScript" $ TestCase $ do
--     dir <- basedir
--     (st,cs) <- handleCommand emptyCState (Init (False,[],[("simple.lslp",dir </> "scripts" </> "simple.lslp")]))
--     v <- checkV cs :: IO [CompilationStatus]
--     (_,cs') <- handleCommand st (UpdateScript ("simple.lslp",dir </> "scripts" </> "simple-alt.lslp"))
--     v' <- checkV cs'
--     case find (\ (CompilationStatus s _) -> s == "simple.lslp") v' of
--         Nothing -> assertFailure "compiled script not found!"
--         Just (CompilationStatus _ (Left s)) -> assertFailure "script should have compiled!"
--         Just (CompilationStatus _ (Right ([],[EPSummary EPHandler nm _ _]))) -> assertEqual "wrong handler name!" nm "default.state_exit"

-- testCheckScript = TestLabel "testCheckScript" $ TestCase $ do
--     dir <- basedir
--     (st,cs) <- handleCommand emptyCState (Init (False,[],[("simple.lslp",dir </> "scripts" </> "simple.lslp")]))
--     v <- checkV cs :: IO [CompilationStatus]
--     (st,s) <- handleCommand st (CheckScript (CodeElement "foo" sample))
--     checkV s :: IO (LSLScript,[ErrInfo])
--     return ()

sample = [here|
    default {
        state_entry() {
        }
    }|]

testDeserializeCommand = TestLabel "testDeserializeCommand" $ TestCase $ do
    let v = parse elemDescriptor command1
    case v of
        Left s -> assertFailure s
        Right (Just (CheckScript (CodeElement name text))) -> assertEqual "---" "foo.lslp" name
        Right Nothing -> assertFailure "didn't find element!"

command1 = [here|<CompilationCommand__CheckScript>
  <el1 class="CodeElement_CodeElement">
    <codeElementName>foo.lslp</codeElementName>
    <codeElementText>sample</codeElementText>
  </el1>
</CompilationCommand__CheckScript>|]

command2 = [here|<CompilationCommand__Init>
  <el1>
    <el1 class="boolean">true</el1>
    <el2 class="linked-list"/>
    <el3 class="linked-list">
      <Tuple2>
        <el1 class="string">test.lslp</el1>
        <el2 class="string">C:\Documents and Settings\rgreayer\My Documents\workspaces\runtime-EclipseApplication\abd\test.lslp</el2>
      </Tuple2>
    </el3>
  </el1>
</CompilationCommand__Init>|]

testDeserializeComand2 = TestLabel "tstDeser2" $ TestCase $ do
    let v = parse elemDescriptor command2
    case v of
        Left s -> assertFailure s
        Right (Just (Init _)) -> return ()
        Right Nothing -> assertFailure "didn't find element!"
