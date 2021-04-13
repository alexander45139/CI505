module Main where

import System.IO
import Log
import Data.List

readLogFile :: String -> IO [String]
readLogFile path = fmap lines (readFile path)

parseMessage :: String -> MaybeLogMessage
parseMessage s = let wds = words s in
    case head wds of
        "I" -> let n   = read (wds !! 1) :: Int
                   msg = unwords (drop 2 wds) in
               ValidLM (LogMessage Info n msg)
        "E" -> let e_n = read (wds !! 1) :: Int
                   n   = read (wds !! 2) :: Int
                   msg = unwords (drop 3 wds) in
               ValidLM (LogMessage (Error e_n) n msg)
        "W" -> let n   = read (wds !! 1) :: Int
                   msg = unwords (drop 2 wds) in
               ValidLM (LogMessage Warning n msg)
        _ -> InvalidLM s

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly []               = []
validMessagesOnly (InvalidLM _:xs) = validMessagesOnly xs
validMessagesOnly (ValidLM lm:xs)  = lm : validMessagesOnly xs

parse :: String -> IO [LogMessage]
parse = fmap (validMessagesOnly . map parseMessage) . readLogFile

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ ts1 _) (LogMessage _ ts2 _) = if ts1 < ts2 then LT else (if ts1 > ts2 then GT else EQ)

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages [] = []
sortMessages xs = sortBy (\x y -> compareMsgs x y) xs

whatWentWrong :: [LogMessage] -> [(TimeStamp, String)]
whatWentWrong []                                 = []
whatWentWrong (LogMessage (Error sev) ts msg:xs) = if sev >= 50 then (ts, msg) : whatWentWrong xs else whatWentWrong xs
whatWentWrong (x:xs)                             = whatWentWrong xs

processLogFile :: String -> String -> IO ()
processLogFile ogFile newFile = do listOfMessages <- parse ogFile
                                   let worstErrors   = whatWentWrong listOfMessages
                                       formattedText = map (\(ts, msg) -> "[" ++ show ts ++ "] " ++ msg) worstErrors
                                   writeFile newFile (unlines formattedText)

------------------------------
-- Main function does nothing
------------------------------
main :: IO ()
main = putStrLn ""
          
