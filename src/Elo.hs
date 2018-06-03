module Elo where

data Result = Win | Draw | Loss

kFactor :: Int
kFactor = 32

transformedRating :: Int -> Int -> (Int, Int)
transformedRating r1 r2 = let
    r1' = 10^(r1 / 400)
    r2' = 10^(r2 / 400)
    in
    (r1', r2')

expectedScore :: (Int, Int) -> (Int, Int)
expectedScore (x,y) = let
    x' = x / (x + y)
    y' = y / (x + y)
    in (x', y')

scoreMultiplier :: Result -> Float
scoreMultiplier Win  = 1
scoreMultiplier Draw = 0.5
scoreMultiplier Loss = 0

elo :: Int -> Int -> Result -> Int
elo ownElo opponentElo result = let
    (own', opponent') = transformedRating ownElo opponentElo
    (ownE, opponentE') = expectedScore own' opponent'
    s = scoreMultiplier result
    in own' + kFactor * (s - ownE)

