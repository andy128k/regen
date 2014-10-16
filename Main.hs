module Main (main) where

import Control.Monad
import Data.Foldable
import qualified Data.Sequence as S
import Data.List
import CharSet

newtype RSeq = RSeq (S.Seq Expr) deriving (Eq)

isEmptySeq :: RSeq -> Bool
isEmptySeq (RSeq s) = S.null s

lengthSeq :: RSeq -> Int
lengthSeq (RSeq q) = S.length q

headSeq :: RSeq -> Expr
headSeq (RSeq q) = S.index q 0

tailSeq :: RSeq -> RSeq
tailSeq (RSeq q) = RSeq $ S.drop 1 q

lastSeq :: RSeq -> Expr
lastSeq (RSeq q) = S.index q ((S.length q) - 1)

initSeq :: RSeq -> RSeq
initSeq (RSeq q) = RSeq $ S.take ((S.length q) - 1) q

joinSeq :: RSeq -> RSeq -> RSeq
joinSeq (RSeq p) (RSeq q) = RSeq $ p S.>< q

prependSeq :: Expr -> RSeq -> RSeq
prependSeq e (RSeq q) = RSeq $ e S.<| q

appendSeq :: Expr -> RSeq -> RSeq
appendSeq e (RSeq q) = RSeq $ q S.|> e

newSeq :: [Expr] -> RSeq
newSeq l = RSeq $ S.fromList l


data RAlt = RAlt CharSet [RSeq] deriving (Eq)

data Expr = Set CharSet | Seq (RSeq) | Alt RAlt deriving (Eq)


-- alt2 :: Expr -> Expr -> Expr
-- alt2 (Alt x) (Alt y) = Alt (x ++ y)
-- alt2 (Alt x) y = Alt (x ++ [y])
-- alt2 x (Alt y) = Alt (x : y)
-- alt2 (Set x) (Set y) = Set (x >< y)
-- alt2 x y = Alt [x, y]

-- alt :: [Expr] -> Expr
-- alt [x] = x
-- alt x = Data.List.foldl1 alt2 x

alt2 :: RAlt -> Expr -> RAlt
alt2 (RAlt c s) (Alt (RAlt c2 s2)) = RAlt (c >< c2) (s ++ s2)
alt2 (RAlt c s) (Seq s2) = RAlt c (s ++ [s2])
alt2 (RAlt c s) (Set c2) = RAlt (c >< c2) s

alt1 :: [Expr] -> RAlt
alt1 x = Data.List.foldl alt2 (RAlt empty []) x

alt :: [Expr] -> Expr
alt [x] = x
alt x = case alt1 x of
  RAlt c [] -> Set c
  x1 -> Alt x1


flattened :: RSeq -> RSeq
flattened x
  | isEmptySeq x = x
flattened x = case headSeq x of
  Seq s ->               flattened (joinSeq s (tailSeq x))
  h     -> prependSeq h (flattened $ tailSeq x)


mseq :: RSeq -> Expr
mseq x
  | isEmptySeq x = Set empty

mseq x
  | (1 == lengthSeq x) = headSeq x

mseq x = Seq $ flattened x


-- detect :: (CharSet, [RSeq], [Expr]) -> Expr -> (CharSet, [RSeq], [Expr])
-- detect (r, s, a) (Set chars) = (r >< chars, s, a)
-- detect (r, s, a) (Seq exprs) = (r, s ++ [exprs], a)
-- detect (r, s, a) (Alt exprs) = (r, s, a ++ exprs)

-- split :: [Expr] -> (CharSet, [RSeq], [Expr])
-- split l = Data.List.foldl detect (empty, [], []) l


-- partitionx :: Eq a => ([b] -> b) -> ([b] -> [b]) -> [[a]] -> [(a, [[a]])]
partitionx head tail sequences =
  map (\group -> (group, map tail (startsWith group))) groups
  where
    groups = nub (map head sequences)
    startsWith v = filter (\x -> v == (head x)) sequences


partition1 :: [RSeq] -> [(Expr, [RSeq])]
partition1 = partitionx headSeq tailSeq

-- partition9 :: Eq a => [[a]] -> [(a, [[a]])]
partition9 = partitionx lastSeq initSeq


sq_seq :: [RSeq] -> [RSeq]
sq_seq sequences =
--  sf $ pr sequences
  pr sequences
  where
    pr :: [RSeq] -> [RSeq]
    pr x = map prepend $ partition1 x

    sf :: [RSeq] -> [RSeq]
    sf x = map append $ partition9 x

    prepend :: (Expr, [RSeq]) -> RSeq
    prepend (start, seqs) = flattened $ newSeq [start, (squeeze (alt (map mseq seqs)))]

    append :: (Expr, [RSeq]) -> RSeq
    append (end, seqs) = flattened $ newSeq [(squeeze (alt (map mseq seqs))), end]


squeeze :: Expr -> Expr
squeeze (Seq (RSeq s)) = mseq $ newSeq (map squeeze (toList s))
squeeze (Alt (RAlt chars sequences)) = Alt $ RAlt chars (sq_seq sequences)
squeeze s = s


range :: [String] -> Expr
range s = Alt $ Data.List.foldl altEmAll (RAlt empty []) (map i2e s)
    where
      altEmAll :: RAlt -> Either CharSet RSeq -> RAlt
      altEmAll (RAlt c s) (Left c2) = RAlt (c >< c2) s
      altEmAll (RAlt c s) (Right s2) = RAlt c (s ++ [s2])

      i2e str = case str of
                  []  -> error "empty input line"
                  [c] -> Left $ single c
                  s   -> Right $ newSeq (map (Set . single) s)


printE :: Int -> Expr -> [String]
printE 0 (Seq (RSeq s)) = ["seq["] ++ (Data.List.concatMap (printE 1) (toList s)) ++ ["]"]
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


-- #+nil
-- (loop
--    for (from to) in '(("0" "9")
-- 		      ("0" "0")
-- 		      ("000" "999")
-- 		      ("0999" "1100")
-- 		      ("000" "009")
-- 		      ("3" "7")
-- 		      ("01" "10")
-- 		      ("01" "30")
-- 		      ("001" "365")
-- 		      )
--    do (format t "(~A ~A) -> ~A~%"
-- 	      from to
-- 	      (range1 (coerce from 'list)
-- 		      (coerce to 'list))))

main = do
  --   print $ squeeze (range [1, 2..365] 7)
  --   print $ squeeze (range [3, 4..7] 3)
  --   print $ squeeze (range [3, 5..37] 0)
  print $ squeeze $ range $ numbers [1, 2..31] 1
        
