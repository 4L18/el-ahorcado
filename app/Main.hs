{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use print" #-}
{-# LANGUAGE BlockArguments #-}
{-# HLINT ignore "Use lambda-case" #-}

module Main where

import System.Console.ANSI ( clearScreen )
import Data.Char (toUpper)
import Control.Monad.Trans.State.Lazy ()
import Data.Maybe (isNothing)


type Name = String
type Score = Int
data Player = Player { name :: Name, score :: Score} deriving Show

data Category = BOOK | MOVIE | PERSON | SERIE | SONG deriving (Show, Read)

data Round = Round { verdugo :: Player
                   , ahorcado :: Player
                   , category :: Category
                   , answer :: String
                   , letters :: [Char]
                   , panel :: [Char]
                   , strikes :: Int
                   } deriving (Show)
type Rounds = [Round]

data Game = Game { player1 :: Player
                 , player2 :: Player
                 , rounds :: Rounds
                 } deriving (Show)


main :: IO ()
main = do
    game <- initGame
    gameResult <- playGame game 1
    putStrLn "Bye"


initGame :: IO Game
initGame = do
    putStrLn "Hello, players!"
    putStrLn "Player 1, enter your name"
    name1 <- map toUpper <$> getLine
    putStrLn "Player 2, enter your name"
    name2 <- map toUpper <$> getLine
    return Game { player1 = Player name1 0
                , player2 = Player name2 0
                , rounds = []
                }


playGame :: Game -> Int -> IO Game
playGame (Game p1 p2 rounds) round = do
    putStrLn $ "Playing round " ++ show round
    roundStart <- initRound round p1 p2
    rondaRes <- playRound roundStart 1
    putStr "Do you want to play another round (y/n): "
    res <- getLine
    if res == "y" then
        playGame (Game p1 p2 (rounds ++ [rondaRes])) (round+1)
    else
        return (Game p1 p2 (rounds ++ [rondaRes]))


initRound :: Int -> Player -> Player -> IO Round
initRound numRound (Player name1 _) (Player name2 _) = do
    let verdugo = if even numRound then Player name1 0 else Player name2 0
        ahorcado = if even numRound then Player name2 0 else Player name1 0
    selectCategory verdugo >>= (\selectedCategory -> case selectedCategory of
                                                        Nothing -> selectCategory verdugo
                                                        (Just cat) -> do
                                                            let category = cat
                                                            requestAnswer verdugo ahorcado category
                               )
    answer <- map toUpper <$> getLine
    clearScreen
    return (Round verdugo ahorcado category answer [] (createPanel answer) 0)


requestAnswer :: Player -> Player -> Category -> IO ()
requestAnswer verdugo ahorcado category = putStrLn $ name verdugo ++ ", introduce a " ++ show category ++ " and don't let " ++ name ahorcado ++ " see it!"


selectCategory :: Player -> Maybe Category
selectCategory verdugo =
    putStrLn $ name verdugo ++ ", choose one of these categories: book, movie, person, serie or song."
    do
        return if isCategory $ map toUpper (\input -> map toUpper <$> getLine)
               then Just input
               else Nothing


isCategory :: String -> Bool
isCategory s
    | s `elem` ["BOOK", "MOVIE", "PERSON", "SERIE", "SONG"] = True
    | otherwise = False


createPanel :: String -> String
createPanel [] = []
createPanel (' ':xs) = ' ':createPanel xs
createPanel (x:xs) = '_':createPanel xs


playRound :: Round -> Int -> IO Round
playRound (Round verdugo ahorcado category answer letters panel strikes) turn = do
    putStrLn (name ahorcado ++ " if you want to guess then type \"resolve\" else type a letter")
    input <- map toUpper <$> getLine
    updatedRound <- updateRound input (Round verdugo ahorcado category answer letters panel strikes)
    if strikes == 7
    then do
        putStrLn (name verdugo ++ " wins")
        showUpdatedRound updatedRound
        return updatedRound
    else do
        if answer == panel
        then do
            putStrLn (name ahorcado ++ " wins")
            showUpdatedRound updatedRound
            return updatedRound
        else do
            showUpdatedRound updatedRound
            playRound updatedRound (turn+1)


updateRound :: [Char] -> Round -> IO Round
updateRound input (Round verdugo ahorcado category answer letters panel strikes)
  | map toUpper input == "RESOLVE" = resolve (Round verdugo ahorcado category answer letters panel strikes)
  | length input == 1 = isCharPresent (Round verdugo ahorcado category answer letters panel strikes) $ toUpper (head input)
  | otherwise = do
       putStrLn "Invalid input"
       return (Round verdugo ahorcado category answer letters panel strikes)


showUpdatedRound :: Round -> IO ()
showUpdatedRound round = do
    clearScreen
    showHangingMan (strikes round)
    showTriedLetters (letters round)
    showPanel (panel round)


resolve :: Round -> IO Round
resolve (Round verdugo ahorcado category answer letters panel strikes) = do
    putStrLn "Enter your awnser: "
    guess <- getLine
    return (if guess == answer
            then Round verdugo ahorcado category answer letters panel strikes
            else Round verdugo ahorcado category answer letters panel (strikes+1))


isCharPresent :: Round -> Char -> IO Round
isCharPresent (Round verdugo ahorcado category answer letters panel strikes) c = do
    return (if c `elem` answer
            then Round verdugo ahorcado category answer letters (updatePanel c answer) strikes
            else Round verdugo ahorcado category answer (letters ++ [c]) panel (strikes+1))


updatePanel :: Char -> [Char] -> [Char]
updatePanel _ [] = []
updatePanel letter (x:xs)
  | x == ' ' = ' ':updatePanel letter xs
  | letter == x = x:updatePanel letter xs
  | x /= '_' = x:updatePanel letter xs
  | otherwise = '_':updatePanel letter xs


showHangingMan :: Int -> IO ()
showHangingMan 1 = drawOneStrike
showHangingMan 2 = drawTwoStrikes
showHangingMan 3 = drawThreeStrikes
showHangingMan 4 = drawFourStrikes
showHangingMan 5 = drawFiveStrikes
showHangingMan 6 = drawSixStrikes
showHangingMan 7 = drawSevenStrikes
showHangingMan _ = putStrLn ""

showTriedLetters :: [Char] -> IO ()
showTriedLetters letters = putStrLn ("Tried letters: " ++ letters ++ "\n")

showPanel :: String -> IO ()
showPanel panel = putStrLn $ "Panel: " ++ show panel ++ "\n"

drawOneStrike :: IO ()
drawOneStrike = do
    putStrLn "  ___"
    putStrLn " |   O"
    putStrLn " |     "
    putStrLn " |     "
    putStrLn " |     "
    putStrLn " |     "
    putStrLn "_|_____"

drawTwoStrikes :: IO ()
drawTwoStrikes = do
    putStrLn "  ___  "
    putStrLn " |   O "
    putStrLn " |   | "
    putStrLn " |     "
    putStrLn " |     "
    putStrLn " |     "
    putStrLn "_|_____"

drawThreeStrikes :: IO ()
drawThreeStrikes = do
    putStrLn "  ___  "
    putStrLn " |   O "
    putStrLn " |  /| "
    putStrLn " |     "
    putStrLn " |     "
    putStrLn "_|_____"

drawFourStrikes :: IO ()
drawFourStrikes = do
    putStrLn "  ___  "
    putStrLn " |   O "
    putStrLn " |  /|\\"
    putStrLn " |     "
    putStrLn " |     "
    putStrLn "_|_____"

drawFiveStrikes :: IO ()
drawFiveStrikes = do
    putStrLn "  ___   "
    putStrLn " |   O  "
    putStrLn " |  /|\\"
    putStrLn " |  /   "
    putStrLn " |      "
    putStrLn "_|_____ "
drawSixStrikes :: IO ()
drawSixStrikes = do
    putStrLn "  ___   "
    putStrLn " |   O  "
    putStrLn " |  /|\\"
    putStrLn " |  / \\"
    putStrLn " |      "
    putStrLn "_|_____ "

drawSevenStrikes :: IO ()
drawSevenStrikes = do
    putStrLn "  ___   "
    putStrLn " |   |  "
    putStrLn " |   O  "
    putStrLn " |  /|\\"
    putStrLn " |  / \\"
    putStrLn "_|_____ "
