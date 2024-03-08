{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

-- The testing library
import Test.QuickCheck
import Test.Tasty.QuickCheck
import Test.Tasty

import qualified TEXT1.Data.Text as TEXT1
import qualified TEXT2.Data.Text as TEXT2

equal :: Eq a => (TEXT1.Text -> a) -> (TEXT2.Text -> a) -> String -> Bool
equal f1 f2 x = f1 (TEXT1.pack x) == f2 (TEXT2.pack x)

--------------------------------------------------------------------------
-- example 2

prop_reverse :: String -> Bool
prop_reverse =
  equal (TEXT1.unpack . TEXT1.reverse) (TEXT2.unpack . TEXT2.reverse)


prop_length :: String -> Bool
prop_length = equal TEXT1.length TEXT2.length

prop_drop :: Int -> String -> Bool
prop_drop n =
  equal (TEXT1.unpack . TEXT1.drop n) (TEXT2.unpack . TEXT2.drop n)

return []

main =
  defaultMain (testProperties "Properties" $allProperties)

