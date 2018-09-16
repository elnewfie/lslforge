module Main where

import qualified Language.Lsl.Internal.MetaData as MetaData
import qualified Language.Lsl.Internal.Compiler as Compiler
import qualified Language.Lsl.Internal.ExpressionHandler as ExpressionHandler
import qualified Language.Lsl.Internal.SimMetaData as SimMetaData
import qualified Language.Lsl.Internal.SystemTester as SystemTester
import qualified Language.Lsl.Internal.UnitTester as UnitTester
import qualified Language.Lsl.Internal.CompilationServer as CompilationServer

import Control.Monad
import System.Environment
import System.Exit
import System.IO

version="0.1.9.6"

usage progName = "Usage: " ++ progName ++ " [Version|MetaData|Compiler|ExpressionHandler|SimMetaData|SystemTester|UnitTester|CompilationServer]"

main = do
    progName <- getProgName
    args <- getArgs
    when (length args < 1) $ do
        hPutStrLn stderr "Invalid number of command line arguments"
        hPutStrLn stderr (usage progName)
        exitFailure
    case head args of
        "Version" -> putStrLn version
        "MetaData" -> MetaData.printMeta
        "Compiler" -> Compiler.main0
        "ExpressionHandler" -> ExpressionHandler.validateExpression stdin stdout
        "SimMetaData" -> SimMetaData.printSimMeta
        "SystemTester" -> SystemTester.testSystem
        "UnitTester" -> UnitTester.main2
        "CompilationServer" -> CompilationServer.compilationServer
        "_CodeGen_" -> case tail args of
            [path,packageName] -> CompilationServer.codeGen path packageName
            _ -> hPutStrLn stderr ("The super secret _CodeGen_ function " ++
                  "requires that you supply a path and a package name for the generated Java code.")
        val -> do
            hPutStrLn stderr ("Invalid argument: " ++ val)
            hPutStrLn stderr (usage progName)
            exitFailure
