module TwoOrMore () where

import qualified Data.Sequence as S

newtype TwoOrMore a = TwoOrMore (a, (S.Seq a), a) deriving (Show)

create :: a -> a -> TwoOrMore a
create first second = TwoOrMore (first, S.empty, second)

