{-# OPTIONS_GHC -XTemplateHaskell -XScopedTypeVariables #-}
module Lsl.Serialization where

-- serialization tests
import Control.Monad
import Language.Lsl.Internal.DOMCombinators
import Language.Lsl.Internal.XmlCreate
import Language.Lsl.Internal.SerializationGenerator
import Language.Haskell.TH
import Language.Lsl.Syntax
import Language.Lsl.Internal.Pragmas
import Data.Data
import System.Directory
import System.IO
import Text.Here

data Example = Example1 { val :: String, val2 :: Int }
             | Example2 { ival :: Int }
             | Example3 { lval :: [Int] }
             | Example4 { info :: (Int,String) }
    deriving (Show,Eq)

data Sample = Sample { sample :: [Int] } deriving (Show)

data Poly a = Poly1 { x :: String }
            | Poly2 { y :: a } deriving (Show,Eq)

type Sample1 = Sample

data Sample2 = Sample2 Sample1            
-- x = $(deriveJavaRep ''Example >>= stringE . show . ppr)
$(deriveJavaRepTups [2..9])

$(deriveJavaRep ''Example)
$(deriveJavaRep ''Poly)
$(deriveJavaRep ''Sample)
$(deriveJavaRep ''Sample2)

example1 = Example1 "hello" 25
example4 = Example4 (1,"yes") 
sample1 = Sample [1]

m :: Int
m = 5

example2 = Poly2 m


repsf = $(collectReps [''Sample2,''Poly])
-- $(deriveJavaRep ''Thing)

-- $(deriveJavaRepTups [2..10])

-- $(deriveAllJavaRepsFor "reps" "lslforge.generated" [t|LSLScript|])

-- xstreamBindings :: [String] -> String
-- xstreamBindings ss = [$here|
-- package lslforge.generated;

-- import com.thoughtworks.xstream.XStream;
-- import com.thoughtworks.xstream.DomDriver;
-- public class Serializer {
--     public static XStream xstream;
--     
--     static {
--         xstream = new XStream(new DomDriver());
-- |] ++ concatMap xstreamBinding ss ++ [$here|
--     }
--     
--     public static LSLScript deserialize(String s) {
--         return (LSLScript) xstream.fromXML(s);
--     }
-- }|]

-- xstreamBinding s = "        xstream.alias(\"" ++ s ++ "\"," ++ s ++ ".class);\n"

-- main = do
--     let reps' = concat reps
--     exist <- doesDirectoryExist "out"
--     when (not exist) $ createDirectory "out"
--     setCurrentDirectory "out"
--     forM_ reps' $ \ (nm,txt) -> writeFile (nm ++ ".java") txt
--     writeFile "Serializer.java" (xstreamBindings (map fst reps'))

instance Show (a -> b) where
    show f = "a function"