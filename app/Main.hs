module Main where

import System.Console.ANSI
import Data.Char
import Control.Monad.Trans.State.Lazy

type Name = String
type Score = Int
data Player = Player Name Score deriving Show

data Rol = Verdugo | Ahorcado

-- todo Answer
newtype Answer a = Answer { ans :: String}
data Category = Book | Movie | Person | Serie | Song deriving (Show, Read)

data Round = Round { verdugo :: Player
                             , ahorcado :: Player
                             , category :: Category
                             , answer :: String
                             , triedLetters :: [Char]
                             , panel :: [Char]
                             , strikes :: Int
                             } deriving (Show)
type Rounds = [Round]

data Game = Game { player1 :: Player
                 , player2 :: Player
                 , rounds :: Rounds
                 } deriving (Show)

initGame :: IO Game
initGame = do {
    putStrLn "Hello, players!";
    putStrLn "Player 1, enter your name";
    name1 <- getLine;
    putStrLn "Player 2, enter your name";
    name2 <- getLine;
    return Game {player1 = Player name1 0, 
        player2 = Player name2 0, rounds=[]}
}

initRound :: Int -> Player -> Player -> IO Round
initRound numRonda (Player name1 _) (Player name2 _) = do {
    verdugo = if mod round 2 == 0 then Player name1 0 else Player name2 0
    putStrLn $ name1 ++ ", choose a category";
    category <- getLine;
    if category 
    putStrLn $ name1 ++ ", introduce a " ++ category ++ " and don't let " ++ player2 ++ " see it!";
    answer <- getLine;
    return (Round )
    --clearScreen;
}

#playRound :: Round -> IO Round


playGame :: Game -> Int -> IO Game
playGame (Game p1 p2 rounds) round = do
    putStrLn $ "Playing round " ++ show round
    if mod round 2 == 0 then
        playRound (Round p2, p1)
    else
        playRound (Round p1, p2)

    putStr "Do you want to play another round (y/n): "
    res <- getLine
    if res == "y" then
        playGame (Game p1 p2 rounds) (round+1)
    else
        return (Game p1 p2 rounds)

main = do
    game <- initGame
    gameResult <- playGame game 1
    putStrLn "Bye"


guess :: [Char] -> Maybe String
guess s = if (s == "resolve")
    then resolve
    else if (size s == 1) 
         then isCharPresent s
         else Nothing
    else Nothing


resolve :: String -> Maybe String
resolve answer = do {
    putStr "Enter your awnser"
    guess <- getLine
    if guess == answer
    then do {
        putStr "You win"
        return Nothing
    }
    else updateHang Nothing
    updateRound
}


isCharPresent Char String:: IO ()
isCharPresent c answer= do {
    if (any (c==) answer)
    then updatePanel c answer
    else updateHang c
    updateRound
}


updatePanel :: Char -> String -> String
updatePanel letter [] = []
updatePanel letter x:xs = if letter == x
                          then x:(updatePanel xs)
                          else "_":(updatePanel xs)


updateHang :: Maybe Char -> Int
updateHang Just c =  do {
                     triedLetters ++ c;
                     strikes++;
                     return strikes;
                     }
updateHang Nothing = do {
                     strikes++;
                     return strikes;
                     }


updateRound ::
updateRound = do {
    showHangingMan
    showTriedLetters
    showPanel
}