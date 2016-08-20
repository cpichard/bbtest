{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import MayaTests
import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
    then do -- load environment here
      exe <- getExecutablePath
      -- FIXME: this should go in an env file
      let command = ["env", "skMaya", "--", exe, "run"]
      (_, _, _, p) <- createProcess (proc "rez" command) 
      putStrLn "loading environment"
      waitForProcess p
      putStrLn "leaving environment"
      return ()
    else do -- run tests in parallel
      mayaTests <- buildMayaTests "./sk_tests" -- FIXME: sk_tests should be a command line argument 
      -- FIXME : select the number of thread, 10 is good on my machine right now
      htfMainWithArgs ["-j10"] mayaTests
