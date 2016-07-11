module Tests exposing (tests)


import ElmTest exposing (..)
import Test.Games as Games


tests : Test
tests =
    suite "Twitch tests"
        [ Games.tests
        ]

main =
    runSuite tests
