module Main where

import Data.List

import Network.HTTP
import Network.URI.Encode
import Text.Regex.TDFA
import Data.Char (isSpace)

trim = f . f
   where f = reverse . dropWhile isSpace

files_count_regex = "[0-9]+ files found"
count_regex = "showing results from [0-9]+ to [0-9]+"
title_regex = ">[a-zA-Z0-9:' ]+</a></b></td>"

mirror = "libgen.rs"

searchRequest query = simpleHTTP (getRequest url)
    where encoded_query = encode query
          url = "http://" ++ mirror ++ "/search.php?&view=detailed&req=" ++ encoded_query

main :: IO ()
main = do
    putStrLn "Input: "
    query <- getLine

    putStrLn $ "Querying \"" ++ query ++ "\" on " ++ mirror ++ "..."
    rsp <- searchRequest query
    ret <- getResponseBody rsp

    putStrLn $ if (ret =~ files_count_regex :: Bool) then (ret =~ files_count_regex :: String) else "0 files found"
    putStrLn $ (ret =~ count_regex :: String)

    let matches = (getAllTextMatches (ret =~ title_regex) :: [String])
    mapM_ putStrLn $ filter (not . ("Libgen Librarian"`isInfixOf`)) $ map (trim . (takeWhile (\x -> x /= '<')) . (drop 1)) matches

