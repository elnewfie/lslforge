{-
   The 'Internal Functions' of LSL are not a formal grouping of functions, but by my definition
   are all the functions that -
   * do not affect the SL environment in any way.  I.e. they don't communicate information
     'outside' the script in any way.
   * do not have any state.  Call them with the same arguments, and they will return the same
     result, each and every time.

   Note that this notion of affecting the SL environment excludes cetain types of effects:
   * time usage.  All actions within a script take time, and this indirectly affects the
     SL environment.
   * energy cost.  SL has a notion of the 'energy' of an object that sometimes comes into
     play.  These functions may affect that energy, but that has no impact on the results
     of the functions themselves.

   Note that a couple of functions that don't affect the SL environment don't fall into
   my category of 'internal' because they are not stateless.  Those functions are
   * llFrand: each time you call this function it returns a different number, even if
     you pass the exact same argument.  SL completely hides the seed and sequence generator
     from the script.
   * llRandomizeList.  Essentially the same issue as for llFrand.

   To implement these functions, either the execution model for the script must hava some
   storage set aside for the state of the pseudo-random number generator, or the 'world
   state' must contain this state.  I've chosen to make this part of the 'world state'
   so the execution model doesn't know anything about the pseudo random number generator.

   Not that anybody was wondering...

   So this module implements the functions that are left, and my assumption is that these
   implementations would be used regardless of how the SL 'world' the scripts run in is
   modelled.  Thus, the operation of all other LSL predefined functions is specific to
   the implementation of the 'world', but these are not.
-}
module Language.Lsl.Internal.InternalLLFuncs(
    -- String Functions
    llStringLength,
    llGetSubString,
    llDeleteSubString,
    llInsertString,
    llParseString2List,
    llParseStringKeepNulls,
    llToUpper,
    llToLower,
    llSubStringIndex,
    llEscapeURL,
    llUnescapeURL,
    llIntegerToBase64,
    llBase64ToInteger,
    llBase64ToString,
    llStringToBase64,
    llStringTrim,
    llXorBase64Strings,
    llXorBase64StringsCorrect,
    llSHA1String,
    -- Math functions
    llCos,
    llSin,
    llAcos,
    llAsin,
    llSqrt,
    llFabs,
    llLog,
    llLog10,
    llTan,
    llAtan2,
    llAbs,
    llCeil,
    llFloor,
    llRound,
    llPow,
    llModPow,
    llListStatistics,
    -- Math Vector/Rotation
    llAngleBetween,
    llEuler2Rot,
    llAxisAngle2Rot,
    llRot2Angle,
    llRot2Axis,
    llAxes2Rot,
    llRot2Fwd,
    llRot2Left,
    llRot2Up,
    llRot2Euler,
    llRotBetween,
    llVecMag,
    llVecDist,
    llVecNorm,
    -- List functions
    llGetListLength,
    llList2List,
    llDeleteSubList,
    llDumpList2String,
    llListFindList,
    llListInsertList,
    llListReplaceList,
    llList2ListStrided,
    llListSort,
    llGetListEntryType,
    llList2Float,
    llList2Integer,
    llList2Key,
    llList2Rot,
    llList2String,
    llList2Vector,
    llList2CSV,
    llCSV2List,
    internalLLFuncs,
    internalLLFuncNames
    ) where

import Language.Lsl.Internal.Util(Permutation3(..),axisAngleToRotation,cut,dist3d,elemAtM,
               filtMap,fromInt,indexOf,mag3d,mag3d2,matrixToQuaternion,quaternionToMatrix,
               quaternionToRotations,rotationBetween,rotationsToQuaternion)
import Language.Lsl.Internal.Type(LSLType(..),LSLValue(..),lslValString,parseFloat,parseInt,rot2RVal,toSVal,typeOfLSLValue,vVal2Vec)
import Language.Lsl.Internal.Evaluation(EvalResult(..))
import Language.Lsl.Internal.Constants(findConstVal)
import Language.Lsl.Internal.Key(LSLKey(..))
import Language.Lsl.Internal.SHA1(hashStoHex)
import Data.List(elemIndex,find,foldl',intersperse,isPrefixOf,sort)
import Data.Char(toLower,toUpper)
import Data.Bits((.|.),(.&.),shiftL,shiftR,xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.ByteString.UTF8 as UTF8
import Network.URI(escapeURIChar,unEscapeString)
import Codec.Binary.UTF8.String(encodeString,decodeString)

internalLLFuncNames :: [String]
internalLLFuncNames = map fst (internalLLFuncs :: (Read a, RealFloat a) => [(String, a -> [LSLValue a] -> Maybe (EvalResult,LSLValue a))])

internalLLFuncs :: (Read a, RealFloat a, Monad m) => [(String, b -> [LSLValue a] -> m (EvalResult,LSLValue a))]
internalLLFuncs = [
    ("llAbs",llAbs),
    ("llAcos",llAcos),
    ("llAngleBetween",llAngleBetween),
    ("llAsin",llAsin),
    ("llAtan2",llAtan2),
    ("llAxes2Rot",llAxes2Rot),
    ("llAxisAngle2Rot",llAxisAngle2Rot),
    ("llBase64ToInteger",llBase64ToInteger),
    ("llBase64ToString",llBase64ToString),
    ("llCeil",llCeil),
    ("llCos",llCos),
    ("llCSV2List",llCSV2List),
    ("llDeleteSubList",llDeleteSubList),
    ("llDeleteSubString",llDeleteSubString),
    ("llDumpList2String",llDumpList2String),
    ("llEscapeURL",llEscapeURL),
    ("llEuler2Rot",llEuler2Rot),
    ("llFabs",llFabs),
    ("llFloor",llFloor),
    ("llGetListEntryType",llGetListEntryType),
    ("llGetListLength",llGetListLength),
    ("llGetSubString",llGetSubString),
    ("llInsertString",llInsertString),
    ("llIntegerToBase64",llIntegerToBase64),
    ("llList2CSV",llList2CSV),
    ("llList2Float",llList2Float),
    ("llList2Integer",llList2Integer),
    ("llList2Key",llList2Key),
    ("llList2List",llList2List),
    ("llList2ListStrided",llList2ListStrided),
    ("llList2Rot",llList2Rot),
    ("llList2String",llList2String),
    ("llList2Vector",llList2Vector),
    ("llListFindList",llListFindList),
    ("llListInsertList",llListInsertList),
    ("llListReplaceList",llListReplaceList),
    ("llListSort",llListSort),
    ("llListStatistics",llListStatistics),
    ("llLog",llLog),
    ("llLog10",llLog10),
    ("llMD5String",llMD5String),
    ("llModPow",llModPow),
    ("llParseString2List",llParseString2List),
    ("llParseStringKeepNulls",llParseStringKeepNulls),
    ("llPow",llPow),
    ("llRot2Angle",llRot2Angle),
    ("llRot2Axis",llRot2Axis),
    ("llRotBetween",llRotBetween),
    ("llRot2Euler",llRot2Euler),
    ("llRot2Fwd",llRot2Fwd),
    ("llRot2Left",llRot2Left),
    ("llRot2Up",llRot2Up),
    ("llRound",llRound),
    ("llSHA1String",llSHA1String),
    ("llSin",llSin),
    ("llSqrt",llSqrt),
    ("llStringLength",llStringLength),
    ("llStringToBase64",llStringToBase64),
    ("llStringTrim", llStringTrim),
    ("llSubStringIndex",llSubStringIndex),
    ("llTan",llTan),
    ("llToLower",llToLower),
    ("llToUpper",llToUpper),
    ("llUnescapeURL",llUnescapeURL),
    ("llVecDist",llVecDist),
    ("llVecMag",llVecMag),
    ("llVecNorm",llVecNorm),
    ("llXorBase64Strings",llXorBase64Strings),
    ("llXorBase64StringsCorrect", llXorBase64StringsCorrect)]

continueWith x = return (EvalIncomplete,x)

-- String Functions
llStringLength _ [SVal s] = continueWith $ IVal (length s)
llGetSubString _ [SVal source, IVal start, IVal end] = continueWith $ SVal (subList source start end)
llDeleteSubString _ [SVal source, IVal start, IVal end] = continueWith $ SVal (deleteSubList source start end)
llInsertString _ [SVal dst, IVal pos, SVal src] =
   -- TODO: wiki says this function does not support negative indices...
   -- how does it deal with out of range indices?
   let (x,y) = splitAt pos dst in continueWith $ SVal $ x ++ src ++ y


separate :: Eq a => [a] -> [[a]] -> [[a]] -> [a] -> Bool -> [[a]]
separate [] _ _ accum keepNulls = if keepNulls || length accum > 0 then [reverse accum] else []
separate l seps spacers accum keepNulls =
   case find ((flip isPrefixOf) l) seps of
       Nothing ->
           case find ((flip isPrefixOf) l) spacers of
               Nothing -> let rest = tail l in separate rest seps spacers (head l:accum) keepNulls
               Just p -> let rest = drop (length p) l in
                         if accum == [] && not keepNulls then
                             p:(separate rest seps spacers [] keepNulls)
                         else (reverse accum):p:(separate rest seps spacers [] keepNulls)
       Just p -> let rest = drop (length p) l in
                 if accum == [] && not keepNulls then
                     separate rest seps spacers [] keepNulls
                 else (reverse accum):(separate rest seps spacers [] keepNulls)


llStringTrim _ [SVal string, trimType ] =
   let trimEnd = reverse . dropWhile (==' ') . reverse
       trimBegin = dropWhile (==' ')
       trimBoth = trimBegin . trimEnd in
   continueWith $ SVal $ (case trimType of
       tt  | Just tt == findConstVal "STRING_TRIM_HEAD" -> trimBegin
           | Just tt == findConstVal "STRING_TRIM_TAIL" -> trimEnd
           | Just tt == findConstVal "STRING_TRIM" -> trimBoth
           | otherwise -> id) string

llParseString2List _ [SVal string, LVal separators, LVal spacers] =
    continueWith $ LVal $ map SVal $ separate string (map lslValString separators) (map lslValString spacers) [] False

llParseStringKeepNulls _ [SVal string, LVal separators, LVal spacers] =
    continueWith $ LVal $ map SVal $ separate string (map lslValString separators) (map lslValString spacers) [] True

llToUpper _ [SVal string] = continueWith $ SVal $ map toUpper string
llToLower _ [SVal string] = continueWith $ SVal $ map toLower string

llSubStringIndex _ [SVal source, SVal pattern] = continueWith $ IVal $
    case indexOf pattern source of
        Nothing -> (-1)
        Just i -> i

unescapedChars = ['A'..'Z'] ++ ['0'..'9'] ++ ['a'..'z']

escapeURL "" n = ""
escapeURL (c:cs) n =
    let s = escapeURIChar (`elem` unescapedChars) c
        len = length s
    in if len > n then "" else s ++ escapeURL cs (n - len)
maxResult = 254::Int

llEscapeURL _ [SVal string] =
    continueWith $ SVal $ escapeURL (encodeString string) maxResult
    --continueWith $ SVal $ escapeURL string maxResult

llUnescapeURL _ [SVal string] =
    continueWith $ SVal $ decodeString $ take maxResult $ unEscapeString string
    --continueWith $ SVal $ take maxResult $ unEscapeString string

llMD5String _ [SVal string, IVal nonce] =
    continueWith $ SVal $ (show . MD5.md5 . L.pack . B.unpack . UTF8.fromString) (string ++ ":" ++ show nonce)

llSHA1String _ [SVal string] = continueWith $ SVal (hashStoHex string)
-- Math functions

unaryToLL :: (RealFloat a, Monad m) => (a -> a) -> [LSLValue a] -> m (EvalResult,LSLValue a)
unaryToLL f [FVal v] = continueWith $ FVal (f v)

llCos _ = unaryToLL cos
llSin _ = unaryToLL sin
llAcos _ = unaryToLL acos
llAsin _ = unaryToLL asin
llSqrt _ = unaryToLL sqrt
llFabs _ = unaryToLL abs
llLog _ = unaryToLL log
llLog10 _ = unaryToLL (logBase 10)
llTan _ = unaryToLL tan

llAtan2 _ [FVal x,FVal y] = continueWith $ FVal (atan2 x y)
llAbs _ [IVal i] = continueWith $ IVal (abs i)
llCeil _ [FVal f] = continueWith $ IVal (ceiling f)
llFloor _ [FVal f] = continueWith $ IVal (floor f)
llRound _ [FVal f] = continueWith $ IVal (round f)
-- llRand is not an 'internal' function, in the sense that it
-- is not stateless, like the other 'internal' functions.
-- this is because of its seed, and current position in
-- the sequence.

llPow _ [FVal base, FVal exp] = continueWith $ FVal (base ** exp)
llModPow _ [IVal base, IVal exp, IVal modulus] = continueWith $ IVal (modpow base exp modulus)

modpow a b c | b < 0 = 0
             | otherwise =
                let b' = (if b > 0xFFFF then 0xFFFF else b)
                    pow _ 0 = 1 `mod` c
                    pow x n = (let p' = pow x (n `div` 2) in ((if odd n then (x *) else id) (p' * p')) `mod` c)
                in pow (a `mod` c) b'

-- should use a map for this, but have been using lists for everything, so will stick with it...
statFuncs :: RealFloat a => [(Int, [a] -> a)]
statFuncs = map (\(Just (IVal op), f) -> (op,f)) [
    (findConstVal "LIST_STAT_RANGE", listStatRange),
    (findConstVal "LIST_STAT_MAX", listStatMax),
    (findConstVal "LIST_STAT_MIN", listStatMin),
    (findConstVal "LIST_STAT_MEAN", listStatMean),
    (findConstVal "LIST_STAT_MEDIAN", listStatMedian),
    (findConstVal "LIST_STAT_STD_DEV", listStatStdDev),
    (findConstVal "LIST_STAT_SUM", sum),
    (findConstVal "LIST_STAT_SUM_SQUARES", sum . map (\x -> x * x )),
    (findConstVal "LIST_STAT_NUM_COUNT", fromInt . length),
    (findConstVal "LIST_STAT_GEOMETRIC_MEAN", listStatGeometricMean)]

toF :: Num a => LSLValue a -> Maybe a
toF (IVal i) = Just (fromInt i)
toF (FVal f) = Just f
toF _ = Nothing

fvals list = filtMap toF list

llListStatistics _ [IVal operation,LVal list] =
    case lookup operation statFuncs of
        Nothing -> continueWith $ FVal 0 -- this seems to be what lsl does...
        Just f -> continueWith $ FVal (f $ fvals list)

listStatRange l = listStatMax l - listStatMin l
listStatMax [] = 0.0
listStatMax l = foldl1 max l
listStatMin [] = 0.0
listStatMin l = foldl1 min l

listStatMean l = sum l / fromInt (length l)
listStatMedian [] = 0.0
listStatMedian l =
    let l' = sort l
        n = length l
    in if odd n then l' !! (n `div` 2) else ((l' !! (n `div` 2)) + (l' !! (n `div` 2 - 1))) / 2.0

listStatStdDev l =
    let mean = listStatMean l in
        sqrt $ listStatMean $ map ((**2.0).(mean-)) l
listStatSumSquares l = sum $ map (**2.0) l
listStatGeometricMean l = product l ** (1.0 / fromInt (length l))

normalizeQuaternion (x,y,z,w) = (x/mag,y/mag,z/mag,w/mag)
    where mag = sqrt (x*x + y*y + z*z + w*w)
llRot2Angle _ [RVal x y z w] = continueWith $ FVal (2.0 * acos w')
    where (_,_,_,w') = normalizeQuaternion (x,y,z,w)
llRot2Axis _ [RVal x y z w] = continueWith v
    where (x',y',z',w') = normalizeQuaternion (x,y,z,w)
          sinVal = sqrt (1.0 - w'*w')
          v = if sinVal == 0.0 then VVal 0.0 0.0 0.0
              else VVal (x'/sinVal) (y'/sinVal) (z'/sinVal)

llAxisAngle2Rot _ [vval@(VVal x y z), FVal r] = (continueWith . rot2RVal) (axisAngleToRotation (vVal2Vec vval) r)

llAxes2Rot _ [VVal xf yf zf, VVal xl yl zl, VVal xu yu zu] =
    let (x,y,z,s) = matrixToQuaternion ((xf,xl,xu),(yf,yl,yu),(zf,zl,zu)) in
        continueWith $ RVal x y z s

llRot2Fwd _ [RVal x y z s] =
    let ((x',_,_),(y',_,_),(z',_,_)) = quaternionToMatrix (x,y,z,s) in
        continueWith $ VVal x' y' z'
llRot2Left _ [RVal x y z s] =
    let ((_,x',_),(_,y',_),(_,z',_)) = quaternionToMatrix (x,y,z,s) in
        continueWith $ VVal x' y' z'
llRot2Up _ [RVal x y z s] =
    let ((_,_,x'),(_,_,y'),(_,_,z')) = quaternionToMatrix (x,y,z,s) in
        continueWith $ VVal x' y' z'

llRot2Euler _ [RVal x y z s] =
    let (x',y',z') = quaternionToRotations P123 False (x,y,z,s)
    in continueWith $ VVal x' y' z'

llEuler2Rot _ [VVal x y z] =
    let (x',y',z',s) = rotationsToQuaternion P123 (x,y,z)
    in continueWith $ RVal x' y' z' s

llRotBetween n [v0@(VVal x1 y1 z1),v1@(VVal x2 y2 z2)] = (continueWith . rot2RVal) (rotationBetween (vVal2Vec v0) (vVal2Vec v1))

llAngleBetween _ [RVal aX aY aZ aS,RVal bX bY bZ bS] =
    continueWith $ FVal $ 2 * acos ((aX * bX + aY * bY + aZ * bZ + aS * bS)
                  / sqrt ((aX^2 + aY^2 + aZ^2 + aS^2) *
                           (bX^2 + bY^2 + bZ^2 + bS^2)))

llVecMag _ [v@(VVal x y z)] =
    continueWith $ FVal $ mag3d (vVal2Vec v)

llVecDist _ [v1@(VVal x1 y1 z1),v2@(VVal x2 y2 z2)] =
    continueWith $ FVal $ dist3d (vVal2Vec v1) (vVal2Vec v2)

llVecNorm _ [v@(VVal x y z)] =
    let mag2 = mag3d2 (vVal2Vec v) in
        continueWith $ if mag2 == 0.0 then VVal 0.0 0.0 0.0
                       else let mag = sqrt mag2 in
                           VVal (x/mag) (y/mag) (z/mag)

-- List Functions

--llGetListLength	 Gets the number of elements in a list
llGetListLength _ [LVal list] = continueWith $ IVal (length list)
llList2List _ [LVal source, IVal start, IVal end] = continueWith $ LVal (subList source start end)
llDeleteSubList _ [LVal source, IVal start, IVal end] = continueWith $ LVal (deleteSubList source start end)

llDumpList2String _ [LVal list, SVal sep] =
    continueWith $ SVal $ concat $ intersperse sep (map lslValString list)

deleteSubList source start end =
    let n = length source
        s = convertIndex n start
        e = convertIndex n end
    in if s <= e then take s source ++ drop (e + 1) source
       else drop (e + 1) $ take s source

subList source start end =
    let n = length source
        (s, e) = (\ s' e' -> if s' >= 0 && e' >= 0 then (s', e')
                        else if s' >= 0 && e' < 0 then (s', n)
                        else if s' < 0 && e' >= 0 then (0, e')
                        else (n, n)) (convertIndex n start) (convertIndex n end)
    in
        if s <= e then take (e - s + 1) $ drop s source
        else take (e+1) source ++ drop s source

convertIndex length index = if index < 0 then length + index else index


-- llCSV2List	Converts comma-separated values (CSV) to a list
-- llList2CSV	Converts a list to comma-separated values (CSV)

llCSV2List _ [SVal s] =
    continueWith $ LVal $ map SVal (lslCsvToList [] s)
llList2CSV _ [LVal l] =
    continueWith $ SVal $ concat (intersperse ", " (map lslValToString l))

lslValToString (VVal x y z) = "<" ++ (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ ">"
lslValToString (RVal x y z s) = "<" ++ (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ "," ++ (show s) ++ ">"
lslValToString (SVal s) = s
lslValToString (KVal k) = unLslKey k
lslValToString (IVal i) = show i
lslValToString (FVal f) = show f

lslCsvToList partial [] = [reverse partial]
lslCsvToList partial (',':rst) = (reverse partial) : (lslCsvToList [] rst)
lslCsvToList partial ('<':rst) = bracketPattern 0 ('<':partial) rst
lslCsvToList partial (x:rst) = lslCsvToList (x:partial) rst

bracketPattern 0 partial ('>':rst) = lslCsvToList ('>':partial) rst
bracketPattern n partial ('>':rst) = bracketPattern (n-1) ('>':partial) rst
bracketPattern n partial ('<':rst) = bracketPattern (n+1) ('<':partial) rst
bracketPattern n partial (x:rst) = bracketPattern n (x:partial) rst
bracketPattern n partial [] = lslCsvToList partial []

llListFindList _ [LVal source, LVal pattern] = continueWith $ IVal $
    case indexOf pattern source of
        Nothing -> (-1)
        Just i -> i
llListInsertList _ [LVal dst, LVal src, IVal pos] =
   let (x,y) = splitAt pos dst in continueWith $ LVal $ x ++ src ++ y

-- TODO: does SL handle start > end?
llListReplaceList _ [LVal dst, LVal src, IVal start, IVal end] =
    let n = length src
        s = convertIndex n start
        e = convertIndex n end
        (x,y) = cut s (e+1) dst in continueWith $ LVal $ x ++ src ++ y

-- llList2ListStrided	Extracts a subset of a strided list
-- TODO: assumes this works like llList2List, but first extracting every nth elem
llList2ListStrided _ [LVal src, IVal start, IVal end, IVal stride] =
    let start' = (start `div` stride) + (if start `mod` stride /= 0 then 1 else 0)
        end' = (end `div` stride)
        stridedSrc = filtMap (\ (x,y) -> if x `mod` stride == 0 then Just y else Nothing) $ zip [0..] src
    in
        continueWith $ LVal $ subList stridedSrc start' end'

strideList l stride =
    if length l <= stride || stride <= 1 then [l] else (take stride l):(strideList (drop stride l) stride)

llListSort _ [LVal list, IVal stride, IVal ascending] =
  let Just (IVal true) = findConstVal "TRUE" -- this will crash if true isn't defined...
      -- according to the documentation for this function
      -- ascending must EQUAL the constant TRUE, or sort
      -- will be descending (which is odd)
      direction = if ascending == true then id else reverse
  in continueWith $ LVal $
      concat (direction $ sort $ strideList list stride)


typeCodes :: RealFloat a => [(LSLType, LSLValue a)]
typeCodes = map (\ (t,Just v) -> (t,v)) [
    (LLInteger, findConstVal "TYPE_INTEGER"),
    (LLFloat, findConstVal "TYPE_FLOAT"),
    (LLString, findConstVal "TYPE_STRING"),
    (LLKey, findConstVal "TYPE_KEY"),
    (LLVector, findConstVal "TYPE_VECTOR"),
    (LLRot, findConstVal "TYPE_ROTATION")]

invalidType :: RealFloat a => LSLValue a
invalidType = let Just v = findConstVal "TYPE_INVALID" in v

-- TODO: might this function work with negative indices? assuming no.
llGetListEntryType _ [LVal l, IVal index] =
    continueWith $
        if index < 0 || index >= length l then invalidType
        else let Just v = lookup (typeOfLSLValue $ l !! index) typeCodes in v

elemAtM' i = if i >= 0 then elemAtM i else elemAtM ((-1) - i) . reverse

llList2Float _ [LVal l, IVal index] =
    continueWith $ FVal $
        case elemAtM' index l of
            Just (SVal s) -> parseFloat s
            Just (IVal i) -> fromInt i
            Just (FVal f) -> f
            _ -> 0.0

llList2Integer _ [LVal l, IVal index] =
    continueWith $ IVal $
        case elemAtM' index l of
            Just (SVal s) -> parseInt s
            Just (IVal i) -> i
            Just (FVal f) -> truncate f
            _ -> 0

llList2Key _ [LVal l, IVal index] =
    continueWith $
        case elemAtM' index l of
            Just v -> let (SVal v') = toSVal v in KVal $ LSLKey v'
            _ -> let (Just (SVal v)) = findConstVal "NULL_KEY" in KVal $ LSLKey v
llList2Rot _ [LVal l, IVal index] =
    continueWith $ case elemAtM' index l of
            Just r@(RVal _ _ _ _) -> r
            _ -> let (Just r) = findConstVal "ZERO_ROTATION" in r

llList2String _ [LVal l, IVal index] =
    continueWith $
        case elemAtM' index l of
            Nothing -> SVal ""
            Just v -> toSVal v

llList2Vector _ [LVal l, IVal index] =
    continueWith $ case elemAtM' index l of
            Just v@(VVal _ _ _) -> v
            _ -> let (Just v) = findConstVal "ZERO_VECTOR" in v


base64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

llIntegerToBase64 _ [IVal i] =
    let  digit1 = 63 .&. (i `shiftR` 26)
         digit2 = 63 .&. (i `shiftR` 20)
         digit3 = 63 .&. (i `shiftR` 14)
         digit4 = 63 .&. (i `shiftR` 8)
         digit5 = 63 .&. (i `shiftR` 2)
         digit6 = 63 .&. (i `shiftR` (-4))
    in continueWith $ SVal $ map (base64chars !!) [digit1,digit2,digit3,digit4,digit5,digit6] ++ "=="

charToBits :: Char -> Int
charToBits c = case elemIndex c base64chars of
    Nothing -> 0
    Just i -> i

llBase64ToInteger _ [SVal s] =
     let n = length s
         s' = if n >= 8 then s else (s ++ (replicate (6 - n) 'A'))
         charToBits c = case elemIndex c base64chars of
             Nothing -> 0
             Just i -> i
     in continueWith $ IVal $ foldl' (.|.) 0 $ zipWith (shiftL) (map charToBits (take 6 s')) [26,20..]

slPrintable :: String
slPrintable = map toEnum (10:[32..127])

mkPrintable c = if c `elem` slPrintable then c else '?'

-- some valid cases
decodeB64 :: String -> String
decodeB64 [] = []
decodeB64 (x:y:'=':'=':[]) =
    let (c1,c2,_) = decodeQuartet(charToBits x,charToBits y,0,0) in [c1]
decodeB64 (x:y:z:'=':[]) =
    let (c1,c2,_) = decodeQuartet(charToBits x,charToBits y,charToBits z,0) in [c1,c2]
decodeB64 (w:x:y:z:rest) =
    let (c1,c2,c3) = decodeQuartet(charToBits w,charToBits x,charToBits y,charToBits z) in
        (c1:c2:c3:(decodeB64 rest))
-- some invalid ones
decodeB64 (x:[]) =
    let (c1,_,_) = decodeQuartet(charToBits x,0,0,0) in [c1]
decodeB64 (x:y:[]) =
    let (c1,_,_) = decodeQuartet(charToBits x,charToBits y,0,0) in [c1]
decodeB64 (x:y:z:[]) =
    let (c1,c2,_) = decodeQuartet(charToBits x,charToBits y,charToBits z,0) in [c1]

decodeQuartet :: (Int,Int,Int,Int) -> (Char,Char,Char)
decodeQuartet (w,x,y,z) =
    let c1 = (w `shiftL` 2) .|. (x `shiftR` 4)
        c2 = ((x `shiftL` 4) .|. (y `shiftR` 2)) .&. 255
        c3 = ((y `shiftL` 6) .|. z) .&. 255
    in (toEnum c1,toEnum c2, toEnum c3)


stripPadChars ('=':'=':[]) = []
stripPadChars ('=':[]) = []
stripPadChars [] = []
stripPadChars (x:xs) = x:(stripPadChars xs)

llXorBase64Strings _ [SVal s1, SVal s2] =
    let i1 = map charToBits $ stripPadChars s1
        i2 = map charToBits $ stripPadChars s2
        n = length i2
        i3 = if null i2 then i1 else zipWith (\ v i -> v `xor` (i2 !! (i `mod` n))) i1 [0..]
    in continueWith $ SVal $ map (base64chars !!) i3

-- test strings s1 = YWJjZGVm, s2 = eHl6 => GRsZHBwc
--              s1 = YWJjZGVm, s2 = eHl6dw== => GRsZEx0f
--              s1 = YWJjZGVm, s2 = eHl6d3h5 => GRsZEx0f

--trace1 v = trace (show v) v
llXorBase64StringsCorrect _ [SVal s1, SVal s2] =
    let s1' = map fromEnum $ decodeB64 s1
        s2' = map fromEnum $ decodeB64 s2
        n = length s2'
        s3 = if null s2' then s1' else zipWith (\ v i -> v `xor` (s2' !! (i `mod` n))) s1' [0..]
    in continueWith $ SVal $ encodeB64 (map toEnum s3)
llBase64ToString _ [SVal s] = continueWith $ SVal $ map mkPrintable $ decodeB64 s

llStringToBase64 _ [SVal s] =
    let s' = (encodeB64 s) in
        continueWith $ SVal s'

encodeB64 [] = []
encodeB64 [x] = encode1 x
encodeB64 [x,y] = encode2 (x,y)
encodeB64 (x:y:z:rest) = encode3 (x,y,z) ++ (encodeB64 rest)

encode3 :: (Char,Char,Char) -> [Char]
encode3 (c1,c2,c3) =
    let b1 = ((fromEnum c1) `shiftR` 2) .&. 63
        b2 = (((fromEnum c1) `shiftL` 4) .|. ((fromEnum c2) `shiftR` 4)) .&. 63
        b3 = (((fromEnum c2) `shiftL` 2) .|. ((fromEnum c3) `shiftR` 6)) .&. 63
        b4 = (fromEnum c3) .&. 63
    in [base64chars !! b1,base64chars !! b2,base64chars !! b3,base64chars !! b4]
encode2 :: (Char,Char) -> [Char]
encode2 (c1,c2) =
    let b1 = ((fromEnum c1) `shiftR` 2) .&. 63
        b2 = (((fromEnum c1) `shiftL` 4) .|. ((fromEnum c2) `shiftR` 4)) .&. 63
        b3 = ((fromEnum c2) `shiftL` 2) .&. 63
    in [base64chars !! b1,base64chars !! b2,base64chars !! b3,'=']
encode1 :: Char -> [Char]
encode1 c1 =
    let b1 = ((fromEnum c1) `shiftR` 2) .&. 63
        b2 = ((fromEnum c1) `shiftL` 4) .&. 63
    in [base64chars !! b1,base64chars !! b2,'=','=']
