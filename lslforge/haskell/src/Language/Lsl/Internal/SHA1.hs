module Language.Lsl.Internal.SHA1(hashStoI,hashStoHex) where

import Text.Printf
import Codec.Binary.UTF8.String
import Data.Array(Array,array,(!))
import Data.Bits(Bits,(.|.),(.&.),xor,rotateL,shiftR,shiftL,complement)
import Data.List(foldl')
import Data.Word(Word8,Word32)

hashStoI = digestToInteger . hash . encode
hash = foldl' (\ buf block -> addDigest buf (processMessageBlock buf block)) ss . blocks512

hashStoHex :: String -> String
hashStoHex s = printf "%040X" (hashStoI s)

data Digest = Digest { eA :: !Word32,
                       eB :: !Word32,
                       eC :: !Word32,
                       eD :: !Word32,
                       eE :: !Word32 }
            deriving (Eq, Show)

addDigest (Digest a1 b1 c1 d1 e1) (Digest a2 b2 c2 d2 e2) = Digest (a1+a2) (b1+b2) (c1+c2) (d1+d2) (e1+e2)

digestToInteger :: Digest -> Integer
digestToInteger (Digest a b c d e) = let n = fromIntegral e +
                                            (fromIntegral d `shiftL` 32) +
                                            (fromIntegral c `shiftL` 64) +
                                            (fromIntegral b `shiftL` 96) +
                                            (fromIntegral a `shiftL` 128)
                                     in n

ks :: Array Int Word32
ks = array (0,79) $ zip [0..] $ concatMap (replicate 20) [0x5a827999,0x6ed9eba1,0x8f1bbcdc,0xca62c1d6]

fs :: Array Int (Word32 -> Word32 -> Word32 -> Word32)
fs  = array (0,79) $ zip [0..] $ concatMap (replicate 20) [ \ x y z -> (x .&. y) .|. ((complement x) .&. z),
                                                            \ x y z -> x `xor` y `xor` z,
                                                            \ x y z -> (x .&. y) .|. (x .&. z) .|. (y .&. z),
                                                            \ x y z -> x `xor` y `xor` z ]

processMessageBlock i words = 
    let foo' n i [] = i
        foo' n i (w:ws) = 
            let i' = (Digest ((eA i `rotateL` 5)  + ((fs ! n) (eB i) (eC i) (eD i)) + eE i + w + (ks ! n)) (eA i) (eB i `rotateL` 30) (eC i) (eD i))
            in foo' (n + 1) i' ws
    in foo' 0 i (compute80Ws words)
    
compute80Ws :: [Word32] -> [Word32]
compute80Ws xs = 
    let foo i (q:qs) rs
            | i < 16 = q : foo (i + 1) qs (q:rs)
            | otherwise = 
                let v = (rs !! 2 `xor` rs !! 7 `xor` rs !! 13`xor` rs !! 15) `rotateL` 1
                in v : foo (i + 1) qs (v:rs)
    in take 80 $ foo 0 (xs ++ repeat 0) []
    
ss :: Digest
ss = Digest 0x67452301 0xefcdab89 0x98badcfe 0x10325476 0xc3d2e1f0

getByte v i = fromIntegral $ (v `shiftR` (i*8)) .&. 0x0ff

padMessage :: [Word8] -> [Word8]
padMessage msg =
   let f :: Int -> [Word8] -> [Word8]
       f n [] = 0x80:((replicate (64 - ((n + 9) `mod` 64)) 0) ++ map (getByte (8*n)) [7,6..0])
       f n (b:bs) = b : (f (n+1) bs)
   in f 0 msg

bytesToWord :: [Word8] -> Word32
bytesToWord = go 0
   where go acc [] = acc
         go acc (b:bs) = go ((acc `shiftL` 8) .|. fromIntegral b) bs
   
chunk _ [] = []
chunk n l = take n l : chunk n (drop n l)
  
blocks512 :: [Word8] -> [[Word32]]
blocks512 = (map (map bytesToWord . chunk 4)) . chunk 64 . padMessage
