module Main where

import Data.TimeMapSpec

import Test.Tasty


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Testing..."
  [spec]
