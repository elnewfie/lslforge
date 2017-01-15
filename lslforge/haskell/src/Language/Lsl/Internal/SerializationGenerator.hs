{-# OPTIONS_GHC -XTemplateHaskell -XFlexibleInstances -XTypeSynonymInstances
                -XScopedTypeVariables -XOverlappingInstances
  #-}
module Language.Lsl.Internal.SerializationGenerator where

import Language.Haskell.TH
import Control.Monad
import Data.Maybe
import qualified Control.Monad.State as State
import qualified Data.Map as M
import Language.Lsl.Internal.XmlCreate
import Language.Lsl.Internal.DOMCombinators
import Data.Generics
import Data.Int
import Data.List
import Debug.Trace
import Text.XML.HaXml.Parse hiding (fst3,snd3,thd3)
import Text.XML.HaXml.Posn

class JavaRep a where
    representative :: a
    xmlSerialize :: Maybe String -> a -> String -> String
    xmlDefaultTag :: a -> String
    subElemDescriptor :: String -> ElementTester a
    elemDescriptor :: ElementTester a
    contentFinder :: String -> ContentFinder a
        
instance JavaRep Int where
    representative = (0 :: Int)
    xmlSerialize t i = emit (maybe "int" id t) [] [shows i]   
    xmlDefaultTag _ = "int"
    subElemDescriptor tag = el tag id readableContent
    elemDescriptor = el "int" id readableContent
    contentFinder tag = mustHaveElem $ subElemDescriptor tag

instance JavaRep Int32 where
    representative = (0 :: Int32)
    xmlSerialize t i = emit (maybe "int" id t) [] [shows i]   
    xmlDefaultTag _ = "int"
    subElemDescriptor tag = el tag id readableContent
    elemDescriptor = el "int" id readableContent
    contentFinder tag = mustHaveElem $ subElemDescriptor tag

instance JavaRep Int64 where
    representative = (0 :: Int64)
    xmlSerialize t i = emit (maybe "long" id t) [] [shows i]   
    xmlDefaultTag _ = "long"
    subElemDescriptor tag = el tag id readableContent
    elemDescriptor = el "int" id readableContent
    contentFinder tag = mustHaveElem $ subElemDescriptor tag
    
instance JavaRep Float where
    representative = (0 :: Float)
    xmlSerialize t i = emit (maybe "float" id t) [] [shows i]   
    xmlDefaultTag _ = "float"
    subElemDescriptor tag = el tag id readableContent
    elemDescriptor = el "float" id readableContent
    contentFinder tag = mustHaveElem $ subElemDescriptor tag
    
instance JavaRep Double where
    representative = (0 :: Double)
    xmlSerialize t i = emit (maybe "double" id t) [] [shows i]   
    xmlDefaultTag _ = "double"
    subElemDescriptor tag = el tag id readableContent
    elemDescriptor = el "double" id readableContent
    contentFinder tag = mustHaveElem $ subElemDescriptor tag

instance JavaRep Char where
    representative = (' ')
    xmlSerialize t c = emit (maybe "char" id t) [] [shows c]
    xmlDefaultTag _ = "char"
    subElemDescriptor tag = el tag id readableContent
    elemDescriptor = el "char" id readableContent
    contentFinder tag = mustHaveElem $ subElemDescriptor tag
        
instance JavaRep Bool where
    representative = True
    xmlSerialize t b = emit (maybe "boolean" id t) [] [shows b]   
    xmlDefaultTag _ = "boolean"
    subElemDescriptor tag = el tag id boolContent
    elemDescriptor = el "boolean" id boolContent
    contentFinder tag = mustHaveElem $ subElemDescriptor tag
    
instance JavaRep String where
    representative = ""
    xmlSerialize t s = emit (maybe "string" id t) (maybe [] (const [("class","string")]) t) [(showString . xmlEscape) s]
    xmlDefaultTag _ = "string"
    subElemDescriptor tag = el tag id simpleContent
    elemDescriptor = el "string" id simpleContent
    contentFinder tag = mustHaveElem $ subElemDescriptor tag
    
instance JavaRep a => JavaRep [a] where
    representative = [ (representative :: a) ]
    xmlSerialize t l = 
        emit (maybe (xmlDefaultTag (representative :: [a])) id t) (maybe [] (const [("class","linked-list")]) t) 
            (map (\ v -> xmlSerialize Nothing v) l)
    xmlDefaultTag _ = "linked-list"
    subElemDescriptor tag = el tag id (many $ elemDescriptor)
    elemDescriptor = el (xmlDefaultTag (representative :: [a])) id (many $ elemDescriptor)
    contentFinder tag = mustHaveElem $ subElemDescriptor tag
    
-- instance JavaRep a => JavaRep (Maybe a) where
--     representative = Just (representative :: a)
--     xmlSerialize t Nothing = id
--     xmlSerialize t (Just v) = xmlSerialize t v
--     xmlDefaultTag x = xmlDefaultTag (representative :: a)
--     subElemDescriptor tag = \ p e -> subElemDescriptor tag p e >>= return . Just
--     elemDescriptor = \ p e -> elemDescriptor p e >>= return . Just
--     contentFinder tag = (canHaveElem $  (\ p e -> subElemDescriptor tag p e))

deriveJavaRepTups l = mapM deriveJavaRepTup l >>= return . concat

deriveJavaRepTup n = do
    let ns = show n
    vs <- mapM newName (replicate n "v")
    let ctx =  mapM (javaRepPred . varT) vs
    let typ = appT javaRepCon $ foldl appT (tupleT n) (map varT vs)
    let representativeD = 
            valD (varP 'representative) (normalB $ tupE (replicate n (varE 'representative))) []
    let nameE = stringE ("Tuple" ++ show n)
    let repGenE = do
            pkgVarName <- newName "pkg"
            let pkgE = varE pkgVarName
            let pkgP = varP pkgVarName
            let tparms = intercalate "," (zipWith (\ x y -> x ++ show y) (replicate n "E") [1..n])
            let fields = concatMap (\ i -> "    public E" ++ show i ++ " el" ++ show i ++ ";\n") [1..n]
            let clsE = [| "package " ++ $pkgE ++ 
                    $(stringE ( concat [
                    ";\nimport com.thoughtworks.xstream.XStream;\n",
                    "public class Tuple", ns, "<", tparms, "> {\n", fields,
                    "    public static void init(XStream xstream) {\n",
                    "         xstream.alias(\"Tuple", ns, "\",Tuple", ns, ".class); //$NON-NLS-1$\n    }\n",
                    "}\n"])) |]
            lam1E pkgP ([|[($nameE,$clsE)]|])
    let mkClause = do
            tagName <- newName "tag"
            vs <- mapM newName (replicate n "v")
            let tagNmE = varE tagName
            let tagE = [| maybe $nameE id $tagNmE |]
            let attrsE = [| maybe [] (const [("class",$nameE)]) $tagNmE |]
            let subElem i x = [e|xmlSerialize (Just $(stringE ("el" ++ show i))) $(varE x)|]
            let lst = listE (zipWith subElem [1..n] vs)
            let expr = [| emit $tagE $attrsE $lst |]
            clause [varP tagName, return (TupP (map VarP vs))] (normalB $ expr) []
    let xmlSerD = funD 'xmlSerialize [mkClause]
    let stmts vs [] = return [noBindS [| return $(tupE (map varE vs))|] ]
        stmts vs ((t,v):ts) = do
            rest <- stmts vs ts
            let stmt = bindS (varP v) [| contentFinder $(stringE t) |]
            return (stmt : rest)
    let fields vs terms = doE =<< (stmts vs terms)
    let subElemDescriptorD = do
            tagNm <- newName "tag"
            vs <- mapM newName (replicate n "v")
            let tagNmE = varE tagNm
            let tags = map (("el" ++) . show) [1..n]
            let terms = zip tags vs
            let elE = [| el $tagNmE id $ 
                                comprises $(fields vs terms) |]
            funD 'subElemDescriptor [clause [varP tagNm] (normalB elE) []]
    let elemDescriptorD = do
            vs <- mapM newName (replicate n "v")
            let tags = map (("el" ++) . show) [1..n]
            let terms = zip tags vs
            let elE = [| el $nameE id (comprises $(fields vs terms)) |]
            funD 'elemDescriptor [clause [] (normalB elE) []]
    let contentFinderD = do
            tagNm <- newName "tag"
            funD 'contentFinder  
                [clause [(varP tagNm)] (normalB [| mustHaveElem (subElemDescriptor $(varE tagNm)) |]) [] ]
    sequence [instanceD ctx typ [representativeD,xmlSerD,funD 'xmlDefaultTag [clause [wildP] (normalB nameE) []], 
                       subElemDescriptorD, elemDescriptorD, contentFinderD],
              valD (varP $ mkName ("jrep'" ++ "Tuple" ++ show n)) (normalB repGenE) []]

deriveJavaRep nm = if nm == ''[] then return [] else
    do  info <- reify nm
        case info of
            TyConI d -> deriveInstance d
            _ -> fail $ ("can't generate serializer for specified name: " ++ show nm)
    where
        deriveInstance (DataD _ tnm vs cs _) = do
            checkAllFields cs 
            sequence [instanceD ctx typ [representativeD,xmlSerializeD,dec1, 
                          subElemDescriptorD,elemDescriptorD,contentFinderD],
                      valD (varP (mkName $ "jrep'" ++ (nameBase tnm))) (normalB representationE) []]
            where tyVarName (PlainTV n) = n
                  tyVarName (KindedTV n _) = error ("unexpected higher kinded type variable: " ++ show n)
                  names = map tyVarName vs
                  ctx = mapM (javaRepPred . varT) names
                  typ = appT javaRepCon $ foldl appT (conT tnm) (map varT names)
                  representativeV = varE 'representative
                  mkRepresentative [] = [e|undefined|]
                  mkRepresentative (c:_) =  getCInfo c >>= 
                      \ (cnm,fts) -> foldl appE (conE cnm) (replicate (length fts) representativeV)
                  representativeD = firstDec [d|representative = $(mkRepresentative cs)|]
                  nameE = stringE (nameBase tnm)
                  representationE = do
                     pkgVarName <- newName "pkg"
                     let pkgE = varE pkgVarName
                     let pkgP = varP pkgVarName
                     let tparms = zipWith (\ v i -> (v, "E" ++ show i)) names [1..]
                     let gparms = if null tparms then "" else "<" ++ intercalate "," (map snd tparms) ++ ">"
                     let baseclassBody = 
                             "import com.thoughtworks.xstream.XStream;\n" ++
                             "public class " ++ (nameBase tnm ++ gparms) ++ "{\n" ++
                             "    public static void init(XStream xstream) {\n" ++
                             "        xstream.alias(\"" ++ (nameBase tnm) ++ "\"," ++
                                                           (nameBase tnm) ++ ".class); //$NON-NLS-1$\n    }\n}\n"
                     let baseclass = [|"package " ++ $pkgE ++ ";\n"  ++ $(stringE baseclassBody)|]
                     let mkSubClassExpr c = do 
                             (cnm,fts) <- getCInfo c
                             let cname = (nameBase tnm) ++ "_" ++ (nameBase cnm)
                             let basenm = (nameBase tnm)
                             fts' <- forM fts $ (\ (nm,t) -> (deSyn [] t) >>= return . (,) nm)
                             let importList = hasList (map snd fts')
                             let fields = 
                                     flip concatMap fts' (\ (nm,t) -> 
                                         "    public " ++ (repT tparms 0 t) ++ 
                                         " " ++ nameBase nm ++ ";\n") 
                             let classStr = "import com.thoughtworks.xstream.XStream;\n" ++
                                            (if importList 
                                                 then "import java.util.LinkedList;\n" 
                                                 else "") ++
                                            "public class " ++ cname ++ gparms ++ 
                                            " extends " ++  basenm ++ gparms ++ "{\n" ++
                                            fields ++
                                            "    public static void init(XStream xstream) {\n" ++
                                            "        xstream.alias(\"" ++ cname ++ "\"," ++ cname ++ ".class); //$NON-NLS-1$\n" ++
                                            "    }\n}\n"
                             let subclass = [|"package " ++ $pkgE ++ ";\n" ++ $(stringE classStr)|]
                             [e|($(nameE) ++ "_" ++ $(stringE $ nameBase cnm),$subclass)|]
                     let subClassExprs = map mkSubClassExpr cs
                     lam1E pkgP [e| ($nameE,$baseclass) : $(listE subClassExprs) |]
                  xmlSerializeD = funD (mkName "xmlSerialize") (map mkClause cs)
                  dec1 = funD (mkName "xmlDefaultTag") [clause [wildP] (normalB nameE) []]
                  computeVarInfo (nm,_) = newName "x" >>= return . ((,) nm)
                  mkClause c = do
                      (cnm,fts) <- getCInfo c
                      vis <- mapM computeVarInfo fts
                      tagVarName <- newName "tag"
                      let tagVarE = varE tagVarName
                      let cnmE = [| $(nameE) ++ "__" ++ $(stringE $ nameBase cnm)|]
                      let cnmEA = [| $(nameE) ++ "_" ++ $(stringE $ nameBase cnm)|]
                      let termf (fnm,vnm) = [|xmlSerialize (Just $(stringE $ nameBase fnm)) $(varE vnm)|]
                      let terms = map termf vis
                      let maybeE = [| maybe $cnmE id $tagVarE|]
                      let attrs = [| maybe [] (const [("class",$cnmEA)]) $tagVarE|]
                      let termsE = listE (if null terms then [] else [foldl1 (\ x y -> [| $x . $y |]) terms])
                      let exp = [| emit $maybeE $attrs $termsE |]
                      clause [varP tagVarName,conP cnm (map (varP . snd) vis)] (normalB exp) []
                  mkNoBindS cnm vs = [noBindS $ [| return  $(foldl (appE) (conE cnm) (map varE $ reverse vs)) |]]
                  mkCnmE cnm = [| $(nameE) ++ "__" ++ $(stringE $ nameBase cnm)|]
                  mkCnmEA cnm = [| $(nameE) ++ "_" ++ $(stringE $ nameBase cnm)|]
                  stmts cnm vs [] = return (mkNoBindS cnm vs)
                  stmts cnm vs ((fnm,_):vis) = do
                      vn <- newName "v"
                      rest <- stmts cnm (vn:vs) vis
                      let stmt = bindS (varP vn) [| contentFinder $(stringE (nameBase fnm)) |]
                      return $ stmt : rest
                  fields cnm vis = doE =<< (stmts cnm [] vis)
                  contentE cnm vis = [| comprises $(fields cnm vis) |]
                  subElemDescriptorD = do
                          tagNm <- newName "tag"
                          let choices = listE (map (mkChoice tagNm) cs)
                          funD 'subElemDescriptor 
                               [clause [(varP tagNm)] (normalB [| choice $choices |]) []]
                      where mkChoice tagNm c = do
                                (cnm,fts) <- getCInfo c
                                let tagNmE = varE tagNm
                                let cnmE = mkCnmEA cnm
                                vis <- mapM computeVarInfo fts
                                [| elWith $tagNmE (const id) (thisAttr "class" $cnmE) $(contentE cnm vis) |]
                  elemDescriptorD = do
                          funD 'elemDescriptor [clause [] (normalB [| choice $choices |]) []]
                      where choices = listE (map mkChoice cs)
                            mkChoice c = do
                                (cnm,fts) <- getCInfo c
                                let cnmE = mkCnmE cnm
                                vis <- mapM computeVarInfo fts
                                [| el $cnmE id $(contentE cnm vis) |]
                  contentFinderD = do
                          tagNm <- newName "tag"
                          funD 'contentFinder 
                              [clause [(varP tagNm)] 
                                     (normalB [| mustHaveElem (subElemDescriptor $(varE tagNm)) |]) []]
        deriveInstance dec = fail ("can't derive instance for: " ++ show (ppr dec) ++ " (" ++ show dec ++ ")")
                      
getCInfo c =  case c of
    RecC cnm fvs -> return (cnm, map (\ (f,_,t) -> (f,t)) fvs)
    NormalC cnm vs -> return (cnm, zipWith (\ i (_,t) -> (mkName $ "el" ++ show i, t)) [1..(length vs)] vs)
    _ -> fail ("can't create java representation for " ++ show (ppr c))

checkField (_,t) | appliesVar t = fail ("can't generate serializer for type with type variables of kind other than '*'")
                 | otherwise    = return ()
checkFields = mapM checkField                

checkAllFields = mapM (\ c -> getCInfo c >>= \ (_,fts) -> checkFields fts)

appliesVar :: Type -> Bool
appliesVar = everything (||) (False `mkQ` chkApp)
    where chkApp :: Type -> Bool
          chkApp (AppT (VarT _) _) = True
          chkApp t = False 

cName :: Type -> [Name]
cName (ConT nm) = [nm]
cName _ = []

deriveRep nm = case nameBase nm of
--    '(':cs -> deriveJavaRepTups [(length cs)]
    _ -> deriveJavaRep nm
    
predef nm = nm `elem` [''Char,''Int,''Float,''Double,''String,''[],''Bool {-,''Maybe -}]

subst args t@(VarT nm) = maybe t id (lookup nm args)
subst args (AppT t0 t1) = AppT (subst args t0) (subst args t1)
subst args t = t

firstDec :: Q [Dec] -> Q Dec
firstDec = liftM head

collectReps names = do
        allNames <- foldM go M.empty names >>= return . catMaybes . M.elems
        let allFuncs = listE $ map (varE . mkName . ("jrep'" ++)) allNames
        [| \ pkg -> concatMap (\ f -> f pkg) $allFuncs |]
    where go m n = case M.lookup n m of
              Just _ -> return m
              Nothing -> reify n >>= \ info -> case info of
                  PrimTyConI _ _ _ -> return (M.insert n Nothing m)
                  TyConI (DataD _ nm _ cs _) -> case nameBase nm of
                       '(':rest -> return (M.insert nm (Just $ "Tuple" ++ show (length rest)) m)
                       _ -> foldM collectCReps (M.insert nm (Just $ nameBase nm) m) cs
                  TyConI (NewtypeD _ nm _ c _) -> collectCReps (M.insert nm (Just $ nameBase nm) m) c
                  TyConI (TySynD _ _ t1) -> decomposeType (M.insert n Nothing m) t1
          decomposeType m (VarT _) = return m
          decomposeType m (TupleT n) = return m
          decomposeType m ListT = return m
          decomposeType m (ConT nm) | nm == ''Bool ||
                                      nm == ''Int ||
                                      nm == ''Char ||
                                      nm == ''[] ||
                                      -- nm == ''Maybe ||
                                      nm == ''Float ||
                                      nm == ''Double  = return m
                                    | otherwise = go m nm
          decomposeType m (AppT x y) = decomposeType m x >>= flip decomposeType y
          collectCReps m (NormalC _ sts) = foldM decomposeType m (map snd sts)
          collectCReps m (RecC _ vsts) = foldM decomposeType m (map (\ (_,_,t) -> t) vsts)
          collectCReps m (InfixC (_,t0) _ (_,t1)) = decomposeType m t0 >>= \ m' -> decomposeType m' t1

saveReps :: String -> [(String,String)] -> IO ()
saveReps pkg codeInfo = do
        mapM_ ( \ (nm,txt) -> writeFile (nm ++ ".java") txt) codeInfo
        writeFile "InitAll.java" $
            "package " ++ pkg ++ ";\n" ++
            "import com.thoughtworks.xstream.XStream;\n" ++
            "public class InitAll {\n" ++
            "    public static void initAll(XStream xstream) {\n" ++
                concatMap (\ (nm,_) -> "        " ++ nm ++ ".init(xstream);\n") codeInfo ++
            "    }\n" ++
            "}\n"
    
    

concatNM = '(++)
concatV = varE concatNM

specialNames = [(''[],"LinkedList"),
                (''(,),"Tuple2"),
                (''(,,),"Tuple3"),
                (''(,,,),"Tuple4"),
                (''(,,,,),"Tuple5"),
                (''(,,,,,),"Tuple6"),
                (''(,,,,,,),"Tuple7"),
                (''Int,"Integer"),
                (''Bool,"Boolean"),
                (''Char,"Character"),
                (''Int32,"Integer"),
                (''Int64,"Long")]
                
deriveName nm = case lookup nm specialNames of
    Nothing -> nameBase nm
    Just s -> s

stringForms = [AppT ListT (ConT ''Char),AppT (ConT ''[]) (ConT ''Char)]

hasList :: [Type] -> Bool
hasList = foldl (\ x y -> x || hasList' y) False
    where hasList' (VarT _) = False
          hasList' (ConT nm) | nm == ''[] = True
                             | otherwise = False
          hasList' form@(AppT x y) | form `elem` stringForms = False
                                   | otherwise =  hasList' x || hasList' y
          hasList' _ = False

repT :: [(Name,String)] -> Int -> Type -> String
repT dict _ (ConT nm)  = deriveName nm
repT dict _ (VarT nm)  = maybe "?" id (lookup nm dict)
repT dict mult (AppT (ConT nm) y) | nm == ''[] && y == (ConT ''Char) = "String"
                                  -- | nm == ''Maybe = repT dict mult y
                                  | otherwise = (deriveName nm) ++ "<" ++ repT dict 0 y ++ (if mult /= 0 then "," else ">")
repT dict mult (AppT ListT y) =  "LinkedList" ++ "<" ++ repT dict 0 y ++ ">" ++ (if mult == 0 then ">" else ",")
repT dict mult (AppT x y) = repT dict (mult + 1) x ++ repT dict 0 y ++ if mult == 0 then ">" else ","

deSyn :: [Type] -> Type -> Q Type
deSyn targs t@(ConT nm) = reify nm >>= \ info -> case info of
     PrimTyConI _ _ _ ->  return (foldl AppT t targs)
     TyConI (DataD _ _ _ _ _) -> return (foldl AppT t targs)
     TyConI (NewtypeD _ _ _ _ _) -> return (foldl AppT t targs)
     TyConI (TySynD _ params t1) -> return (foldl AppT (subst t1) targs')
         where targs' = drop (length params) targs
               substs = zip params targs
               subst t@(VarT nm) = maybe t id (lookup (PlainTV nm) substs)
               subst (AppT x y)  = (AppT (subst x) (subst y))
               subst t           = t
     other -> fail ("can't handle " ++ show (ppr other))
deSyn targs (AppT x y) = do
    arg <- deSyn [] y
    deSyn (arg:targs) x
deSyn targs t@(VarT nm) = return $ foldl AppT t targs

javaRepCon = conT ''JavaRep

javaRepPred = classP ''JavaRep . (:[])
