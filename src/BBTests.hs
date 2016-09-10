{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module BBTests where

import Test.Framework
import Test.Framework.TestTypes
import Test.Framework.BlackBoxTest
import Data.Yaml
import Data.List
import Data.Aeson.Types
import Control.Applicative
import System.Process
import System.Environment
import System.IO
import System.Directory

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

-- Write a simple example file
writeExampleFile = do
  let bbtu1 = BBTestUnit "examples/vfx_tests/maya/gui/launch_maya" "examples/vfx_tests/maya/gui" ".sh" True
      bbtu2 = BBTestUnit "examples/vfx_tests/maya/prompt/launch_maya" "examples/vfx_tests/maya/prompt" ".sh" True
      bbtu3 = BBTestUnit "python" "examples/vfx_tests/maya/python" ".py" True
      bbtu4 = BBTestUnit "examples/vfx_tests/maya/script/launch_maya" "examples/vfx_tests/maya/script" ".mel" True
      bbts = BBTestSuite "maya" ["bash"] [bbtu1, bbtu2, bbtu3, bbtu4]
  encodeFile "examples/example1.yaml" [bbts, bbts]


buildTestSuite :: String -> [BBTestUnit] -> [Test] -> IO TestSuite
buildTestSuite name (x:xs) tests = do
    newTest <- blackBoxTests (path x) (progName x) (suffix x) (defaultBBTArgs {bbtArgs_verbose = (BBTests.verbose x)})
    buildTestSuite name xs (tests ++ newTest)
buildTestSuite name [] tests = return $ makeTestSuite name tests
    

runSuites :: [BBTestSuite] -> IO ()
runSuites (x:xs) = do
  putStrLn "running suites"
  thisExePath <- getExecutablePath
  (testUnit, hTestUnit) <- openTempFile "/tmp" "testunit.yaml" -- FIXME delete temp file 
  encodeFile testUnit (tests x)
  hClose hTestUnit
  let command = envCmd x ++ [thisExePath, suiteName x, testUnit]
  --let env = (showCommandForUser (head (envCmd x)) (tail (envCmd x))) ++ " " ++(showCommandForUser (head command) (tail command)) 
  let env = showCommandForUser (head command) (tail command)
  
  --(Just hin, _, _, p) <- createProcess (shell env){std_in = CreatePipe}
  (_, _, _, p) <- createProcess (shell env)
  putStrLn $ "loading environment " ++ (show env)
  putStrLn $ "running " ++ (show command)
  -- TODO send command to process
  --hPutStrLn hin (showCommandForUser (head command) (tail command))
  code <- waitForProcess p
  
  putStrLn $ "leaving environment" 
  removeFile testUnit
  runSuites xs
runSuites [] = putStrLn "all suite processed"

