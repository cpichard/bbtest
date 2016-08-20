{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MayaTests where

import Test.Framework
import Test.Framework.BlackBoxTest

--
-- TODO: have a yaml file describing the test and the different actions to take
-- We shouldn't have hardcoded command in here
--
buildMayaTests :: String -> IO TestSuite
buildMayaTests root = do
    test1 <- blackBoxTests (root ++ "/maya/gui") (root ++ "/maya/gui/launch_maya") "" defaultBBTArgs
    test2 <- blackBoxTests (root ++ "/maya/python") "mayapy" "py" defaultBBTArgs
    test3 <- blackBoxTests (root ++ "/maya/script") (root ++ "/maya/script/launch_maya") "mel" defaultBBTArgs
    test4 <- blackBoxTests (root ++ "/maya/prompt") (root ++ "/maya/prompt/launch_maya") "mel" defaultBBTArgs
    return $ makeTestSuite "maya" (test1 ++ test2 ++ test3 ++ test4)
