module Language.Lsl.Internal.Load(
    loadScript,
    loadScripts,
    loadModules) where

import Control.Exception(SomeException(..),tryJust)
import Language.Lsl.Internal.BuiltInModules(avEventGen)
import Language.Lsl.Syntax(compileLSLScript',compileLibrary,
    Library,CompiledLSLScript,Validity,CodeErrs(..))
import Language.Lsl.Parse(parseModule, parseScript)

parseFiles p files = mapM (parseFile p) files

parseFile p (name,path) =
        do result <- tryJust (\ e@(SomeException x) -> Just (show e)) $ p path
           case result of
               Left msg -> return (name,Left (Nothing,msg))
               Right (Left err) -> return (name,Left err)
               Right (Right m) -> return (name,Right m)

loadModules files =
    do parseResults <- parseFiles parseModule files
       let (bad,ok) = splitResults parseResults
       let augLib = compileLibrary (avEventGen:ok)
       return (augLib ++ (map (\ (n,err) -> (n,Left $ CodeErrs [err])) bad))

-- loadModules' files =
--     do parseResults <- parseFiles parseModule files
--        let (bad,ok) = splitResults parseResults
--        let augLib = validLibrary (avEventGen:ok)
--        return (augLib ++ (map (\ (n,err) -> (n,Left [err])) bad))
--        --return (validated ++ (map (\ (n,err) -> (n,Left err)) bad))

loadScript ::  Library -> (t,String) -> IO (t,Validity CompiledLSLScript)
loadScript lib sinfo =
     parseFile parseScript sinfo
     >>= \ r -> case r of
         (t,Left e) -> return (t,Left $ CodeErrs [e])
         (t,Right script) -> return (t, compileLSLScript' lib script)
loadScripts library = mapM (loadScript library)

-- loadScripts library files =
--     do parseResults <- parseFiles parseScript files
--        let (bad,ok) = splitResults parseResults
--        return $ (map (\ (n,script) -> (n,compileLSLScript' library script)) ok) ++
--            (map (\ (n,err) -> (n,Left [err])) bad)

splitResults [] = ([],[])
splitResults ((name,Left err):xs) =
    let (lefts,rights) = splitResults xs in
        ((name,err):lefts,rights)
splitResults ((name, Right m):xs) =
    let (lefts,rights) = splitResults xs in
        (lefts,(name,m):rights)
