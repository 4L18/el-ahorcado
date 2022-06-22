



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