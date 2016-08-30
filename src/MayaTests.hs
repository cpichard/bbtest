{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MayaTests where

import Test.Framework
import Test.Framework.TestTypes
import Test.Framework.BlackBoxTest
import Data.Yaml
import Data.Aeson.Types
import Control.Applicative
import System.Process
import System.Environment

data BBTestUnit =
  BBTestUnit
    { progName :: String -- name of the program under test
    , path :: String     -- ???
    , suffix :: String -- filename suffix for input file
    , verbose :: Bool -- verbosity
    } deriving Show

data BBTestSuite = 
  BBTestSuite
    { suiteName :: String
    , envCmd :: [String] -- Command to start the environment
    , tests :: [BBTestUnit] -- list of tests to build
    } deriving Show

instance FromJSON BBTestUnit where
    parseJSON (Object m) = 
            BBTestUnit <$> m .: "progName" 
                       <*> m .: "path" 
                       <*> m .: "suffix"
                       <*> m .: "verbose"
    parseJSON invalid    = typeMismatch "BBTestUnit" invalid

instance ToJSON BBTestUnit where
    toJSON (BBTestUnit n p s v) = object ["progName" .= n, "path" .= p, "suffix" .= s, "verbose" .= v]

instance FromJSON BBTestSuite where
    parseJSON (Object m) = BBTestSuite <$> m .: "suiteName" <*> m .: "envCmd" <*> m .: "tests"
    parseJSON invalid    = typeMismatch "BBTestSuite" invalid

instance ToJSON BBTestSuite where
    toJSON (BBTestSuite p e t)  = object [ "suiteName" .= p, "envCmd" .= e, "tests" .= t] 


writeExampleFile = do
  let bbtu = BBTestUnit "bbt1" "bbt2" "bbt3" True
      bbts = BBTestSuite "bbts1" ["res", "env"] [bbtu]
  encodeFile "example.yaml" [bbts, bbts]


buildTestSuite :: String -> [BBTestUnit] -> [Test.Framework.TestTypes.Test] -> IO TestSuite
buildTestSuite name (x:xs) tests = do
    newTest <- blackBoxTests (path x) (progName x) (suffix x) defaultBBTArgs
    buildTestSuite name xs (tests ++ newTest)
buildTestSuite name [] tests = return $ makeTestSuite name tests
    

runSuites :: [BBTestSuite] -> IO ()
runSuites (x:xs) = do
  thisExePath <- getExecutablePath
  let testUnit = "/tmp/suite1.yaml" -- FIXME create a tmp file
  encodeFile testUnit (tests x)
  let command = (envCmd x) ++ [thisExePath] ++ [suiteName x] ++ [testUnit]
  (_, _, _, p) <- createProcess (proc (head command) (tail command)) 
  putStrLn $ "loading environment" 
  waitForProcess p
  putStrLn $ "leaving environment" 
  return ()

