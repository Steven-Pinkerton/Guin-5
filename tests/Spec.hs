module Main where

import Test.Hspec (hspec)
import NewsFeedSpec (newsFeedspec)

main :: IO ()
main = hspec newsFeedspec