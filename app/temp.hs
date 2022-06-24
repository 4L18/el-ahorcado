inputResolve :: String -> IO ()
inputResolve answer = do {
                            putStrLn "what is your guess?";
                            guess <- getLine;
                            if ((map toLower guess) == (map toLower answer));
                            then putStrLn "you won!";
                            else putStrLn "to-do"
                         }


inputLetter :: Maybe a -> String -> IO ()
inputLetter (Just c) answer = if (isAlphaNum c)
                       then putStrLn . checkLetter c answer
                       else putStrLn (c ++ " is not a valid input")
inputLetter Nothing _ = putStrLn "Not a valid input"


checkLetter :: Char -> String -> String
checkLetter c s = "to-do"



startRound :: IO ()
startRound = do {
    putStrLn $ player1 ++ ", choose a category";
    category <- getLine;
    putStrLn $ player1 ++ ", introduce a " ++ category ++ " and don't let " ++ player2 ++ " see it!";
    answer <- getLine;
    clearScreen;
}

-- startTurn :: Either 
-- playerWantsToResolve :: [Char] -> Bool
--      if TRUE guessAnswer >>= isGuessCorrect :: String -> Bool
--      if FALSE guessChar >>= isCharPresent :: Char -> Bool
-- updateHang :: Maybe Char -> ()
-- isDead :: Bool
-- updatePanel :: 
-- isPanelCompleted :: Bool






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