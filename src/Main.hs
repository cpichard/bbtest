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
import Test.Framework.TestManager
import Test.Framework.CmdlineOptions
--
main :: IO ()
main = do
  --writeExampleFile
  args <- getArgs
  -- We use the number of arguments to determine which test to run.
  -- 1 => list of suites
  -- 2 => list of unit tests
  let argsLength = length args
  case argsLength of
    1 -> do -- Read a yaml file containing a list of suites
      maybeConfig <- decodeFileEither (head args) :: IO (Either ParseException [BBTestSuite])
      case maybeConfig of
          Right suites -> do 
            runSuites suites
          Left errTestSuite -> do
                print $ prettyPrintParseException errTestSuite
    2 -> do -- Read a yaml file containing a list of units tests
      maybeTestUnit <- decodeFileEither (last args) :: IO (Either ParseException [BBTestUnit])
      case maybeTestUnit of
          Right bbtu -> do
             testSuite <- buildTestSuite (head args) bbtu []
             defaultConfig <- testConfigFromCmdlineOptions defaultCmdlineOptions
             let config = defaultConfig {tc_shuffle = False, tc_threads= Just 5 }
             code <- testSuite `seq` runTestWithConfig config testSuite 
             putStrLn $ "return code is " ++ show code
          Left errTestUnit -> do 
                print $ prettyPrintParseException errTestUnit
    _ -> error "wrong number of arguments on the command line"



