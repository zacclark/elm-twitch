module Test.Games exposing (tests)


import ElmTest exposing (..)
import Games


tests : Test
tests =
  suite "Games"
    [ matchingSelectedTest
    , uniqueSelectedTest
    ]


matchingSelectedTest : Test
matchingSelectedTest =
  test "all games - in list" <|
    assertEqual
      ["one", "two", "three"]
      (Games.allGames (Games.Model "TWO" ["one", "two", "three"]))


uniqueSelectedTest : Test
uniqueSelectedTest =
  test "all games - not in list" <|
    assertEqual
      ["special", "one", "two"]
      (Games.allGames (Games.Model "special" ["one", "two"]))
