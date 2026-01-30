module Triangle where

import Numeric.Natural (Natural)

countBiggerThan :: Ord a => [a] -> a -> Int
countBiggerThan xs b = length (filter (\ x -> x >= b) xs)

data Triangle = MkTriangle Natural Natural Natural

createTriangle :: Natural -> Natural -> Natural -> Maybe Triangle
createTriangle alpha beta gamma
  = if countBiggerThan [alpha, beta, gamma] 90 <= 1 then
      if alpha + beta + gamma == 180 then
        Just (MkTriangle alpha beta gamma) else Nothing
      else Nothing

createTriangle