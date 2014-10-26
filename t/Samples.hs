module Main(main) where

import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import CharSet as C
import Regen


testCharSetUnion :: Assertion
testCharSetUnion = C.empty C.>< C.empty @?= C.empty


propCharSetSingle ch = s C.>< s == s
  where s = C.single ch


test09 :: Assertion
test09 = (squeeze $ range $ numbers [1, 2..9] 0) @?= (Alt $ RAlt (C.single '0') [])

--    for (from to) in '(("0" "9")
-- 		      ("0" "0")
-- 		      ("000" "999")
-- 		      ("0999" "1100")
-- 		      ("000" "009")
-- 		      ("3" "7")
-- 		      ("01" "10")
-- 		      ("01" "30")
-- 		      ("001" "365")

  --   print $ squeeze (range [1, 2..365] 7)
  --   print $ squeeze (range [3, 4..7] 3)
  --   print $ squeeze (range [3, 5..37] 0)


main :: IO ()
main = defaultMainWithOpts
       [ testCase "charset union" testCharSetUnion
       , testProperty "charset single" propCharSetSingle
       , testCase "0..9" test09
       ] mempty

