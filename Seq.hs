module Seq (RSeq, isEmptySeq, lengthSeq, headSeq, tailSeq, lastSeq, initSeq, joinSeq, prependSeq, appendSeq, newSeq, foldSeq) where

import Data.Foldable as F
import qualified Data.Sequence as S

newtype RSeq a = RSeq (S.Seq a) deriving (Eq)

isEmptySeq :: RSeq a -> Bool
isEmptySeq (RSeq s) = S.null s

lengthSeq :: RSeq a -> Int
lengthSeq (RSeq q) = S.length q

headSeq :: RSeq a -> a
headSeq (RSeq q) = S.index q 0

tailSeq :: RSeq a -> RSeq a
tailSeq (RSeq q) = RSeq $ S.drop 1 q

lastSeq :: RSeq a -> a
lastSeq (RSeq q) = S.index q ((S.length q) - 1)

initSeq :: RSeq a -> RSeq a
initSeq (RSeq q) = RSeq $ S.take ((S.length q) - 1) q

joinSeq :: RSeq a -> RSeq a -> RSeq a
joinSeq (RSeq p) (RSeq q) = RSeq $ p S.>< q

prependSeq :: a -> RSeq a -> RSeq a
prependSeq e (RSeq q) = RSeq $ e S.<| q

appendSeq :: a -> RSeq a -> RSeq a
appendSeq e (RSeq q) = RSeq $ q S.|> e

newSeq :: [a] -> RSeq a
newSeq l = RSeq $ S.fromList l

foldSeq :: (x -> a -> x) -> x -> RSeq a -> x
foldSeq f start (RSeq s) = F.foldl f start s

