module EloSpec where

import           Elo
import           Test.Hspec
import           Test.QuickCheck

data Game = Game { winnerRating :: Int, loserRating :: Int } deriving (Show, Eq)

instance Arbitrary Game where
    arbitrary = Game <$> choose (100, 5000) <*> choose (100, 5000)




spec =
    describe "Elo" $ do
        it "is a zero sum game" $ property $
            \game ->
                let
                    winnerRatingF = fromIntegral $ winnerRating game
                    loserRatingF = fromIntegral $ loserRating game
                    (newWinnerRating, newLoserRating) = elo winnerRatingF loserRatingF
                in winnerRatingF + loserRatingF == newWinnerRating + newLoserRating
        it "A player with a higher rating gets a lower delta from a win" $ property $
            \game ->
                let
                    delta :: Float -> Float -> Float
                    delta old new = abs $ new - old
                    winnerRatingF = fromIntegral $ winnerRating game
                    loserRatingF = fromIntegral $ loserRating game
                    (newWinnerRating, newLoserRating) = elo winnerRatingF loserRatingF
                    (losersWinnerRating, winnersLoserRating) = elo loserRatingF winnerRatingF
                in if winnerRatingF > loserRatingF then
                    delta winnerRatingF newWinnerRating < delta loserRatingF losersWinnerRating
                   else
                    delta winnerRatingF newWinnerRating > delta loserRatingF losersWinnerRating


