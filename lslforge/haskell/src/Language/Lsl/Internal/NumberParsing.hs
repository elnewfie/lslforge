module Language.Lsl.Internal.NumberParsing(readHexFloat,readInt) where

import Data.Char(digitToInt)
import Text.ParserCombinators.Parsec --(GenParser(..),(<|>),char,digit,hexDigit,
   --many,many1,oneOf,option,parse,satisfy,optional)

hexFloat =
    do char '0'
       oneOf "xX"
       wholeDigits <- many1 hexDigit
       let w = foldl (\ x y -> x * 16.0 + y) 0.0 $ map (fromIntegral . digitToInt) wholeDigits
       frac <- option 0.0 fractionalPart
       p <- expPart
       return $ (w + frac) * p
       
fractionalPart =
    do char '.'
       digits <- many1 hexDigit
       return $ foldr (\ x y -> (x + y) / 16.0) 0.0 $ map (fromIntegral . digitToInt) digits
       
expPart =
    do oneOf "pP"
       f <- option (2^) ((char '+' >> return (2^)) <|> (char '-' >> return ((1/).(2^)))) 
       digits <- many1 digit
       let exp = foldl (\ x y -> 10 * x + y) 0 $ map digitToInt digits
       return $ f exp

hexFloatAndTail :: Fractional a => GenParser Char b [(a,String)]
hexFloatAndTail =
    do v <- hexFloat
       rest <- many (satisfy (const True))
       return [(realToFrac v,rest)]

readHexFloat s = 
    case parse doFloat "" s of
        Left _ -> []
        Right v -> v

doFloat :: Fractional a => Parser [(a,String)]
doFloat = try hexFloatAndTail <|> (do
    sign <- option 1 $ 
           choice [char '-' >> return (-1), char '+' >> return 1]
    v <- float
    rest <- many (satisfy (const True))
    return [(sign * realToFrac v, rest)])
        
hexInt =
    do oneOf "xX"
       digits <- many1 hexDigit
       return $ foldl (\ x y -> x * 16 + y) 0 $ map digitToInt digits
       
decimalInt =
    do digits <- many1 digit
       return $ foldl (\ x y -> x * 10 + y) 0 $ map digitToInt digits

int = do char '0'
         (hexInt <|> decimalInt <|> return 0)
   <|> decimalInt
   
intAndTail :: Parser (Int, String)
intAndTail =
    do sign <- option 1 $ 
           choice [char '-' >> return (-1), char '+' >> return 1]
       i <- int
       rest <- many (satisfy (const True))
       return (sign * i,rest)
       
readInt s =
    case parse intAndTail "" s of
        Left _ -> []
        Right v -> [v]

-----------------------
decimalFraction w = char '.' >> fracPart True w
                       
fracPart reqDigit w = do 
    digits <- (if reqDigit then many1 else many) digit 
        <?> "fractional part of decimal"
    p <- option 1.0 expon
    return $ p * (w + (foldr (\ b d -> (b + d) / 10.0) 0 
        $ map (fromIntegral.digitToInt) digits))

expon :: Fractional a => GenParser Char st a
expon = do 
    oneOf "eE"
    s <- option '+' (oneOf "+-")
    let k x = if s == '+' then x else 1/x
    digits <- many1 digit <?> "exponent"
    let p = foldl (\ b d -> b * 10 + d) 0 $ map digitToInt digits
    return (k (10^p))

float :: Fractional a => GenParser Char st a
float =  decimalFraction 0.0
     <|> try (char '0' >> option 0 prefZeroNum)
     <|> float'

prefZeroNum :: Fractional a => GenParser Char st a
prefZeroNum = decimalFraction 0.0 <|> float'
  
float' :: Fractional a => GenParser Char st a        
float' = do
    wholeDigits <- many1 digit <?> "number"
    let w = foldl (\ b d -> b * 10 + d) 0 $ map digitToInt wholeDigits
    mf <- option Nothing (char '.' >> option (fromIntegral w) (fracPart False (fromIntegral w)) >>= return . Just)
    case mf of
        Nothing -> try ( expon >>= \ p -> return $ (fromIntegral w * p) ) <|> (return $ fromIntegral w)
        Just f -> return f
        
