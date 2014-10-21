module Seq (RSeq, headSeq, tailSeq, lastSeq, initSeq, joinSeq, prependSeq, appendSeq, newSeq) where

import Control.Monad
import Data.Foldable as F
import qualified Data.Sequence as S
import TwoOrMore

newtype RSeq a = RSeq (S.Seq a) deriving (Eq)

instance Functor RSeq where
  fmap f (RSeq s) = RSeq $ fmap f s

instance F.Foldable RSeq where
 foldr f z (RSeq s) = F.foldr f z s


headSeq :: RSeq a -> a
headSeq (RSeq q) = S.index q 0

tailSeq :: RSeq a -> Either (RSeq a) a
tailSeq (RSeq q) = case l of
  0 -> error "WTF??"
  1 -> error "WTF??"
  2 -> Right $ S.index q 1
  _ -> Left $ RSeq $ (S.drop 1 q)
  where l = S.length q

lastSeq :: RSeq a -> a
lastSeq (RSeq q) = S.index q ((S.length q) - 1)

initSeq :: RSeq a -> Either (RSeq a) a
initSeq (RSeq q) = case l of
  0 -> error "WTF??"
  1 -> error "WTF??"
  2 -> Right $ S.index q 0
  _ -> Left $ RSeq $ S.take (l - 1) q
  where l = S.length q

joinSeq :: RSeq a -> RSeq a -> RSeq a
joinSeq (RSeq p) (RSeq q) = RSeq $ p S.>< q

prependSeq :: a -> RSeq a -> RSeq a
prependSeq e (RSeq q) = RSeq $ e S.<| q

appendSeq :: RSeq a -> a -> RSeq a
appendSeq (RSeq q) e = RSeq $ q S.|> e

newSeq :: a -> a -> RSeq a
newSeq s1 s2 = RSeq $ S.fromList [s1, s2]

