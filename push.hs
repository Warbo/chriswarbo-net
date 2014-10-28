#!/usr/bin/env runhaskell

import System.Environment
import System.Exit
import System.Process

rebuild :: IO ()
rebuild = exitOnFailure $ rawSystem "./site" ["rebuild"]

watch :: IO ProcessHandle
watch = runProcess "./site" ["watch"] Nothing Nothing Nothing Nothing Nothing

whileWatching x = do p <- watch
                     x
                     terminateProcess p

check = exitOnFailure $ runCommand "./404s.sh" >>= waitForProcess

runCheck :: IO ()
runCheck = whileWatching check

exitOnFailure x = do code <- x
                     case code of
                          ExitFailure _ -> exitWith code
                          ExitSuccess   -> return ()

scp = rawSystem "scp" ["-r", "_site", "chriswarbo.net:~/"]

optional = [("rebuild", rebuild), ("check", runCheck)]

toDo args = let opt = map snd $ filter ((`elem` args) . fst) optional
             in opt ++ [exitOnFailure scp]

main = do fmap toDo getArgs >>= sequence
