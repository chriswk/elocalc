module EloSpec where

import           Elo
import           Test.Hspec
import           Test.QuickCheck

data Game = Game { winnerRating :: Float, loserRating :: Float } deriving (Show, Eq)

instance Arbitrary Game where
    arbitrary = Game <$> randFloat 100 5000 <*> randFloat 100 5000

randFloat :: Int -> Int -> Gen Float
randFloat min max = fromIntegral <$> choose (min, max)



spec =
    describe "Elo" $ do
        it "is a zero sum game" $ property $
            \game ->
                let
                    win = winnerRating game
                    lose = loserRating game
                    (newWinnerRating, newLoserRating) = elo win lose
                    in win + lose == newWinnerRating + newLoserRating
        it "A player with a higher rating gets a lower delta from a win" $ property $
            \game ->
                let
                    delta :: Float -> Float -> Float
                    delta old new = abs $ new - old
                    win = winnerRating game
                    lose = loserRating game
                    (newWinnerRating, newLoserRating) = elo win lose
                    (losersWinnerRating, winnersLoserRating) = elo lose win
                    in if win > lose then
                        delta win newWinnerRating < delta lose losersWinnerRating
                       else
                        delta win newWinnerRating > delta lose losersWinnerRating

