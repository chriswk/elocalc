module Elo where

type Elo = Float
type Rank = Float
type Score = Float
type KFactor = Float

simplifiedRating :: Elo -> Rank
simplifiedRating r = 10 ** (r/400)

expectedScore :: Rank -> Rank -> (Score, Score)
expectedScore r1 r2 = let
    totalRank = r1 + r2
    e1 = r1 / totalRank
    e2 = r2 / totalRank
    in (e1, e2)

elo :: KFactor -> Elo -> Elo -> (Elo, Elo)
elo kFactor winnersElo losersElo = let
    r1 = simplifiedRating winnersElo
    r2 = simplifiedRating losersElo
    (e1, e2) = expectedScore r1 r2
    winningAddition = kFactor * (1 - e1)
    losingSubtraction = kFactor * (0 - e2)
    in (winnersElo + winningAddition, losersElo + losingSubtraction)

