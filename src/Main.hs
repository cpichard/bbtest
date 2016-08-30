{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import BBTests 
import System.Environment
import System.Process
import Data.Yaml
import Data.Aeson.Types
import Control.Applicative

--
main :: IO ()
main = do
  --writeExampleFile
  args <- getArgs
  let argsLength = length args
  case argsLength of
    1 -> do
      maybeConfig <- decodeFileEither (head args) :: IO (Either ParseException [BBTestSuite])
      case maybeConfig of
          -- Try to read a singleBBTestUnit
          Right suites -> do 
            runSuites suites
            print suites
          Left errTestSuite -> do
                print $ prettyPrintParseException errTestSuite
    2 -> do 
      maybeTestUnit <- decodeFileEither (last args) :: IO (Either ParseException [BBTestUnit])
      case maybeTestUnit of
          Right bbtu -> do
             testSuite <- buildTestSuite (head args) bbtu []
             htfMainWithArgs ["-j10"] testSuite
          Left errTestUnit -> do 
                print $ prettyPrintParseException errTestUnit
    _ -> error "wrong number of arguments on the command line"



