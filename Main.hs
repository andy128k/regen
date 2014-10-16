module Main (main) where

import Control.Monad
-- import Data.Foldable
import Data.List
import CharSet
import Seq

data RAlt = RAlt CharSet [RSeq Expr] deriving (Eq)

data Expr = Set CharSet | Seq (RSeq Expr) | Alt RAlt deriving (Eq)


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


flattened :: RSeq Expr -> RSeq Expr
flattened x
  | isEmptySeq x = x
flattened x = case headSeq x of
  Seq s ->               flattened (joinSeq s (tailSeq x))
  h     -> prependSeq h (flattened $ tailSeq x)


mseq :: RSeq Expr -> Expr
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


partition1 :: [RSeq Expr] -> [(Expr, [RSeq Expr])]
partition1 = partitionx headSeq tailSeq

-- partition9 :: Eq a => [[a]] -> [(a, [[a]])]
partition9 = partitionx lastSeq initSeq


sq_seq :: [RSeq Expr] -> [RSeq Expr]
sq_seq sequences =
--  sf $ pr sequences
  pr sequences
  where
    pr :: [RSeq Expr] -> [RSeq Expr]
    pr x = map prepend $ partition1 x

    sf :: [RSeq Expr] -> [RSeq Expr]
    sf x = map append $ partition9 x

    prepend :: (Expr, [RSeq Expr]) -> RSeq Expr
    prepend (start, seqs) = flattened $ newSeq [start, (squeeze (alt (map mseq seqs)))]

    append :: (Expr, [RSeq Expr]) -> RSeq Expr
    append (end, seqs) = flattened $ newSeq [(squeeze (alt (map mseq seqs))), end]


squeeze :: Expr -> Expr
squeeze (Seq s) = mseq $ newSeq $ foldSeq (\x -> \v -> x ++ [squeeze v]) [] s
squeeze (Alt (RAlt chars sequences)) = Alt $ RAlt chars (sq_seq sequences)
squeeze s = s


range :: [String] -> Expr
range s = Alt $ Data.List.foldl altEmAll (RAlt empty []) (map i2e s)
    where
      altEmAll :: RAlt -> Either CharSet (RSeq Expr) -> RAlt
      altEmAll (RAlt c s) (Left c2) = RAlt (c >< c2) s
      altEmAll (RAlt c s) (Right s2) = RAlt c (s ++ [s2])

      i2e str = case str of
                  []  -> error "empty input line"
                  [c] -> Left $ single c
                  s   -> Right $ newSeq (map (Set . single) s)


printE :: Int -> Expr -> [String]
printE 0 (Seq s) = ["seq["] ++ (foldSeq (\x -> \v -> x ++ (printE 1 v)) [] s) ++ ["]"]
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
  print $ squeeze $ range $ numbers [1, 2..31] 3
        
