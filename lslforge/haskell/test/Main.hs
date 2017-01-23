module Main where

import System.Exit (exitFailure)
import Test.HUnit

import qualified Lsl.CompilationServerTests
import qualified Lsl.CompilerTests
import qualified Lsl.OptTests
import qualified Lsl.SimTests

main :: IO ()
main = do
  Counts _ _ es fs <- runTestTT $ TestList
    [ Lsl.SimTests.tests
    , Lsl.OptTests.optTests
    , Lsl.CompilerTests.allTests
    , Lsl.CompilationServerTests.testInit
    ]
  if es > 0 || fs > 0
    then exitFailure
    else return ()
