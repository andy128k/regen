import Data.List

data Expr = Set [Char] | Seq [Expr] | Alt [Expr] deriving (Eq)

alt2 :: Expr -> Expr -> Expr
alt2 (Alt x) (Alt y) = Alt (x ++ y)
alt2 (Alt x) y = Alt (x ++ [y])
alt2 x (Alt y) = Alt (x : y)
alt2 (Set x) (Set y) = Set (x ++ y)
alt2 x y = Alt [x, y]

alt :: [Expr] -> Expr
alt [x] = x
alt x = foldl1 alt2 x


flattened :: [Expr] -> [Expr]
flattened [] = []
flattened ((Seq s) : xs) = flattened (s ++ xs)
flattened (x : xs) = x : flattened xs


mseq :: [Expr] -> Expr
mseq [] = Set []
mseq [(Set r)] = Set r
mseq x = Seq $ flattened x


detect :: ([Char], [[Expr]], [Expr]) -> Expr -> ([Char], [[Expr]], [Expr])
detect (r, s, a) (Set chars) = (r ++ chars, s, a)
detect (r, s, a) (Seq exprs) = (r, s ++ [exprs], a)
detect (r, s, a) (Alt exprs) = (r, s, a ++ exprs)

split :: [Expr] -> ([Char], [[Expr]], [Expr])
split l = foldl detect ([], [], []) l


-- partitionx :: Eq a => ([b] -> b) -> ([b] -> [b]) -> [[a]] -> [(a, [[a]])]
partitionx head tail sequences =
  map (\group -> (group, map tail (startsWith group))) groups
  where
    groups = nub (map head sequences)
    startsWith v = filter (\x -> v == (head x)) sequences


partition1 :: Eq a => [[a]] -> [(a, [[a]])]
partition1 = partitionx head tail

partition9 :: Eq a => [[a]] -> [(a, [[a]])]
partition9 = partitionx last init


sq_seq :: [[Expr]] -> [Expr]
sq_seq sequences =
  map Seq $ (sf . pr) sequences
  where
    pr :: [[Expr]] -> [[Expr]]
    pr x = map prepend $ partition1 x

    sf :: [[Expr]] -> [[Expr]]
    sf x = map append $ partition9 x

    prepend :: (Expr, [[Expr]]) -> [Expr]
    prepend (start, seqs) = flattened [start, (squeeze (alt (map mseq seqs)))]

    append :: (Expr, [[Expr]]) -> [Expr]
    append (end, seqs) = flattened [(squeeze (alt (map mseq seqs))), end]


squeeze :: Expr -> Expr
squeeze (Seq s) = mseq $ map squeeze s
squeeze (Alt s) = case split s of
  (chars, sequences, alternates) -> alt (
      (makeChars chars) ++
      (sq_seq sequences) ++
      alternates )
  where
    makeChars :: [Char] -> [Expr]
    makeChars [] = []
    makeChars chars = [Set chars]
squeeze s = s


pad p s = (replicate (p - (length s)) '0') ++ s

i2e :: Int -> Int -> Expr
i2e p i = mseq $ map (\x -> Set [x]) ((pad p) $ show i)

range :: [Int] -> Int -> Expr
range s p = Alt $ map (i2e p) s


printE :: Int -> Expr -> [String]
printE 0 (Seq s) = ["seq["] ++ (concatMap (printE 1) s) ++ ["]"]
printE 0 (Alt s) = ["alt["] ++ (concatMap (printE 1) s) ++ ["]"]
printE 0 (Set s) = [s]
printE i x = map (indent ++) (printE 0 x)
  where indent = replicate (i * 4) ' '


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
   print $ squeeze (range [1, 2..1000000] 1)

