module CharSet (CharSet, single, empty, isEmpty, (><)) where

newtype CharSet = CharSet [Char] deriving (Eq, Show)

single :: Char -> CharSet
single c = CharSet [c]

empty = CharSet []

isEmpty :: CharSet -> Bool
isEmpty (CharSet []) = True
isEmpty (CharSet _) = False

(><) :: CharSet -> CharSet -> CharSet
(><) (CharSet s1) (CharSet s2) = CharSet (s1 ++ s2)

