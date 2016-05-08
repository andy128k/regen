module Regen where

import Control.Monad
import Data.List
import Data.Either
import Data.Foldable as F
import CharSet
import Seq


type TSeq = RSeq RAlt
data RAlt = RAlt CharSet [TSeq] deriving (Eq)
data Expr = Seq TSeq | Alt RAlt deriving (Eq)


-- partitionx :: Eq a => ([b] -> b) -> ([b] -> [b]) -> [[a]] -> [(a, [[a]])]
partitionx head tail sequences =
  map (\group -> (group, map tail (startsWith group))) groups
  where
    groups = nub (map head sequences)
    startsWith v = filter (\x -> v == (head x)) sequences


partition1 :: [TSeq] -> [(RAlt, [Either TSeq RAlt])]
partition1 = partitionx headSeq tailSeq

partition9 :: [TSeq] -> [(RAlt, [Either TSeq RAlt])]
partition9 = partitionx lastSeq initSeq


sq_seq :: [TSeq] -> [TSeq]
sq_seq sequences =
  sf $ pr sequences
  where
    pr :: [TSeq] -> [TSeq]
    pr x = map prepend $ partition1 x

    sf :: [TSeq] -> [TSeq]
    sf x = map append $ partition9 x

    prepend :: (RAlt, [Either TSeq RAlt]) -> TSeq
    prepend (start, [Left tail])  = prependSeq start tail
    prepend (start, [Right tail]) = newSeq start tail
    prepend (start, tails)        = newSeq start (squeezeAlt $ alts tails)

    append :: (RAlt, [Either TSeq RAlt]) -> TSeq
    append (end, [Left init])     = appendSeq init end
    append (end, [Right init])    = newSeq init end
    append (end, inits)           = newSeq (squeezeAlt $ alts inits) end

    alts :: [Either TSeq RAlt] -> RAlt -- length >= 2
    alts seqs = alt cs (ss ++ ss2)
      where
        (ss, as) = partitionEithers seqs
        cs = map (\a -> case a of RAlt x y -> x) as
        ss2 = Data.List.concatMap (\a -> case a of RAlt x y -> y) as

    alt :: [CharSet] -> [TSeq] -> RAlt
    alt cs ss = RAlt (Data.List.foldl (><) empty cs) ss

squeezeAlt :: RAlt -> RAlt
squeezeAlt (RAlt chars sequences) = RAlt chars (sq_seq sequences)

squeeze :: Expr -> Expr
squeeze (Seq s) = Seq $ fmap squeezeAlt s
squeeze (Alt r) = Alt $ squeezeAlt r


achar :: Char -> RAlt
achar c = RAlt (single c) []

str2expr :: String -> Either CharSet TSeq -- RSeq CharSet
str2expr str = case str of
  []       -> error "empty input line"
  [c]      -> Left $ single c
  c1:c2:cs -> Right $ Data.List.foldl appendSeq (newSeq (achar c1) (achar c2)) (map achar cs)


range :: [String] -> RAlt
range s = Data.List.foldl altEmAll (RAlt empty []) (map str2expr s)
    where
      altEmAll :: RAlt -> Either CharSet TSeq -> RAlt
      altEmAll (RAlt c s) (Left c2) = RAlt (c >< c2) s
      altEmAll (RAlt c s) (Right s2) = RAlt c (s ++ [s2])

sss :: RAlt -> Expr
sss = Alt

shift :: [String] -> [String]
shift x = map (indent ++) x
  where indent = replicate 4 ' '

printE :: Expr -> [String]
printE (Seq s) = ["seq["] ++ (F.concatMap (\x -> shift $ printE (sss x)) s) ++ ["]"]
printE (Alt (RAlt c s)) = ["alt["] ++ (shift $ [show c] ++ (Data.List.concatMap printE (map Seq s))) ++ ["]"]


numbers :: [Int] -> Int -> [String]
numbers s p = map i2s s
    where i2s = (pad p) . show

pad :: Int -> String -> String
pad p s = (replicate (p - (length s)) '0') ++ s


instance Show Expr where
  show a = intercalate "\n" $ printE a
