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
data Category = Book | Movie | Person | Serie | Song deriving Show

data Game = Game { player1 :: Player
                 , player2 :: Player
                 , rounds :: Rounds
                 } deriving (Show)

game :: State Game
--data State s a = State (a -> (a, s))
--runState (State f) s = f s

setPlayer :: String -> State Game ()
setPlayer name = do {
                        old <- get
                        put (player1 game = name)
                    }

getGameState :: State s s
getGameState = State (\state -> (state, state))

modifyGameState :: (s -> s) -> State s ()
modifyGameState f = State (\state -> ((), f state))

instance Monad (State s) where
    return x = State (\s -> (x, s))
    op >>= f = State h
        where h state0 = let (val, state1) = runState op state0
                             op1 = f val
                         in runState op1 state1

data Round = Round { verdugo :: Player
                             , ahorcado :: Player
                             , category :: Category
                             , answer :: String
                             , triedLetters :: [Char]
                             , panel :: [Char]
                             , strikes :: Int
                             } deriving (Show)
type Rounds = [Round]

main :: IO ()
main = do {
    initGame;
    --startRound;
    --putStrLn (player2 ++ " if you want to guess then type \"resolve\" else type a letter");
    --getLine >>= (\input -> if (map toLower input == "resolve")
      --                     then inputResolve answer
        --                   else if (length input == 1)
          --                      then inputLetter (Just input) answer
            --                    else inputLetter Nothing answer
              --  )
}         

initGame :: GameState
initGame = do {
    putStrLn "Hello, players!";
    putStrLn "Player 1, enter your name";
    player1 <- getLine;
    putStrLn "Player 2, enter your name";
    player2 <- getLine;
    return
}