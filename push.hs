#!/usr/bin/env runhaskell

import Control.Applicative
import Data.List
import System.Environment
import System.Exit
import System.IO
import System.Process

-- rebuild

rebuild :: IO ExitCode
rebuild = rawSystem "./site" ["rebuild"]

-- check

watch :: IO ProcessHandle
watch = do dn <- devnull
           p  <- runProcess "./site" ["watch"] Nothing
                                               Nothing
                                               Nothing
                                               (Just dn)
                                               Nothing
           rawSystem "sleep" ["2"]
           return p

devnull :: IO Handle
devnull = openFile "/dev/null" WriteMode

whileWatching :: IO ExitCode -> IO ExitCode
whileWatching x = do p    <- watch
                     code <- x
                     terminateProcess p
                     return code

split404s :: [String] -> [String]
split404s = let start x = "Found "       `isPrefixOf` x &&
                          "broken links" `isInfixOf`  x
                end   x = "FINISHED"     `isPrefixOf` x
             in takeWhile (not . end) . dropWhile (not . start)
{-
split404s [] = []
split404s xs = let last n = unlines . reverse . take n . reverse
                in case span (not . ("404 Not Found" `isInfixOf`)) xs of
                        (pre, post) -> last 2 pre : split404s (drop 1 post)
-}

crawl :: IO String
crawl = let f (ExitSuccess, _, _) = ""
            f (_,           _, e) = e
         in f <$> readProcessWithExitCode "wget"
                                          ["-nd", "--spider", "-e",
                                           "robots=off", "-L", "-r", "-p",
                                           "http://localhost:8000"]
                                          ""

check' :: IO [String]
check' = split404s . lines <$> crawl


check :: IO ExitCode
check = whileWatching $ do fails <- check'
                           putStr $ unlines fails
                           return $ if null fails
                                       then ExitSuccess
                                       else ExitFailure 1

-- scp

scp :: IO ExitCode
scp = rawSystem "scp" ["-r", "_site", "chriswarbo.net:~/"]

-- main

exitOnFailure :: IO ExitCode -> IO ()
exitOnFailure = let f ExitSuccess = return ()
                    f code        = exitWith code
                 in (>>= f)

ops :: [(String, IO ExitCode)]
ops = [("rebuild", print "Rebuilding"     >> rebuild),
       ("check",   print "Checking links" >> check  ),
       ("copy",    print "Copying"        >> scp    )]

op :: String -> IO ()
op n = case lookup n ops of
            Just x  -> exitOnFailure x
            Nothing -> return ()

main = getArgs >>= sequence . map op >> return ()
