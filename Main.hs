module Main (main) where

import Regen

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
  print $ squeeze $ range $ numbers [1, 2..9] 0

