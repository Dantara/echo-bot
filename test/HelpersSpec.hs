module HelpersSpec where

import           Data.Char
import           Data.List
import           Hedgehog            (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import           Helpers
import           Test.Tasty          (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)


test_helpers :: TestTree
test_helpers = testGroup "Helpers tests"
  [ testProperty "CamelTo_snake_case" camelToSnakeCheck
  ]
  where
    camelToSnakeCheck :: Property
    camelToSnakeCheck = property $ do
      words <- forAll
        $ Gen.list (Range.linear 1 20)
                (Gen.string (Range.linear 10 20) Gen.lower)
      let camel = foldMap(\(x:xs) -> toUpper x : xs) words
      snakeToCamel (camelToSnakeCase camel) === camel

    snakeToCamel :: String -> String
    snakeToCamel = foldMap (\(x:xs) -> toUpper x : xs)
      . filter (/= "_")
      . groupBy (\y x -> x /= '_' && y /= '_')
