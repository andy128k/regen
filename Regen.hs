module Regen where

import Control.Monad
import Data.List
import Data.Either
import Data.Foldable as F
import CharSet
import Seq

type TSeqItem = Either CharSet RAlt
type TSeq = RSeq TSeqItem
data RAlt = RAlt CharSet [TSeq] deriving (Eq)
data Expr = Set CharSet | Seq TSeq | Alt RAlt deriving (Eq)


alt2 :: RAlt -> Expr -> RAlt
alt2 (RAlt c s) (Alt (RAlt c2 s2)) = RAlt (c >< c2) (s ++ s2)
alt2 (RAlt c s) (Seq s2) = RAlt c (s ++ [s2])
alt2 (RAlt c s) (Set c2) = RAlt (c >< c2) s


-- partitionx :: Eq a => ([b] -> b) -> ([b] -> [b]) -> [[a]] -> [(a, [[a]])]
partitionx head tail sequences =
  map (\group -> (group, map tail (startsWith group))) groups
  where
    groups = nub (map head sequences)
    startsWith v = filter (\x -> v == (head x)) sequences


partition1 :: [TSeq] -> [(TSeqItem, [Either TSeq TSeqItem])]
partition1 = partitionx headSeq tailSeq

partition9 :: [TSeq] -> [(TSeqItem, [Either TSeq TSeqItem])]
partition9 = partitionx lastSeq initSeq


sq_seq :: [TSeq] -> [TSeq]
sq_seq sequences =
  sf $ pr sequences
  where
    pr :: [TSeq] -> [TSeq]
    pr x = map prepend $ partition1 x

    sf :: [TSeq] -> [TSeq]
    sf x = map append $ partition9 x

    prepend :: (TSeqItem, [Either TSeq TSeqItem]) -> TSeq
    prepend (start, [Left tail])  = prependSeq start tail
    prepend (start, [Right tail]) = newSeq start tail
    prepend (start, tails)        = newSeq start (Right $ squeezeAlt $ alts tails)

    append :: (TSeqItem, [Either TSeq TSeqItem]) -> TSeq
    append (end, [Left init])     = appendSeq init end
    append (end, [Right init])    = newSeq init end
    append (end, inits)           = newSeq (Right $ squeezeAlt $ alts inits) end

    alts :: [Either TSeq TSeqItem] -> RAlt -- length > 1
    alts seqs = alt (cs ++ cs2) (ss ++ ss2)
      where
        (ss, rs) = partitionEithers seqs
        (cs, as) = partitionEithers rs
        cs2 = map (\a -> case a of RAlt x y -> x) as
        ss2 = Data.List.concatMap (\a -> case a of RAlt x y -> y) as

    alt :: [CharSet] -> [TSeq] -> RAlt
    alt cs ss = RAlt (Data.List.foldl (><) empty cs) ss

--    alt1 :: [Expr] -> RAlt
    alt1 x = Data.List.foldl alt2 (RAlt empty []) x

squeezeAlt :: RAlt -> RAlt
squeezeAlt (RAlt chars sequences) = RAlt chars (sq_seq sequences)

squeeze :: Expr -> Expr
squeeze (Seq s) = Seq $ fmap sq s
    where
      sq (Left c) = Left c
      sq (Right a) = Right $ squeezeAlt a
squeeze (Alt r) = Alt $ squeezeAlt r
squeeze s = s


str2expr :: String -> Either CharSet TSeq -- RSeq CharSet
str2expr str = case str of
  []       -> error "empty input line"
  [c]      -> Left $ single c
  c1:c2:cs -> Right $ Data.List.foldl appendSeq (newSeq (Left $ single c1) (Left $ single c2)) (map (Left . single) cs)


range :: [String] -> Expr
range s = Alt $ Data.List.foldl altEmAll (RAlt empty []) (map str2expr s)
    where
      altEmAll :: RAlt -> Either CharSet TSeq -> RAlt
      altEmAll (RAlt c s) (Left c2) = RAlt (c >< c2) s
      altEmAll (RAlt c s) (Right s2) = RAlt c (s ++ [s2])

sss :: TSeqItem -> Expr
sss (Left s) = Set s
sss (Right a) = Alt a

printE :: Int -> Expr -> [String]
printE 0 (Seq s) = ["seq["] ++ (F.concatMap (\x -> printE 1 (sss x)) s) ++ ["]"]
printE 0 (Alt (RAlt c s)) = ["alt["] ++ (printE 1 (Set c)) ++ (Data.List.concatMap (printE 1) (map Seq s)) ++ ["]"]
printE 0 (Set s) = [show s]
printE i x = map (indent ++) (printE 0 x)
  where indent = replicate (i * 4) ' '


numbers :: [Int] -> Int -> [String]
numbers s p = map i2s s
    where i2s = (pad p) . show

pad :: Int -> String -> String
pad p s = (replicate (p - (length s)) '0') ++ s


instance Show Expr where
  show a = intercalate "\n" $ printE 0 a
