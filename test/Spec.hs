{-# LANGUAGE RecordWildCards #-}


import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Robot

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "robot" $ for_ cases test
  where
    test Case{..} = it description assertion
      where
        assertion = robot input startPosition `shouldBe` expected

data Case = Case { description   :: String
                 , input         :: String
                 , startPosition :: Maybe Position
                 , expected      :: Maybe Position
                 }

cases :: [Case]
cases = [ Case { description    = "Do nothing"
                , input         = ""
                , startPosition = Just (Position 0 0 North)
                , expected      = Just (Position 0 0 North)
                }
        , Case { description    = "Move two steps forward"
                , input         = "MM"
                , startPosition = Just (Position 0 0 North)
                , expected      = Just (Position 0 2 North)
                }
        , Case { description    = "Turn right"
                , input         = "R"
                , startPosition = Just (Position 0 0 North)
                , expected      = Just (Position 0 0 East)
                }
        , Case { description    = "Turn left"
                , input         = "L"
                , startPosition = Just (Position 0 0 North)
                , expected      = Just (Position 0 0 West)
                }
        , Case { description    = "Turn around 360 degrees clockwise"
                , input         = "RRRR"
                , startPosition = Just (Position 0 0 North)
                , expected      = Just (Position 0 0 North)
                }
        , Case { description    = "Turn around 360 degrees counter clockwise"
                , input         = "LLLL"
                , startPosition = Just (Position 0 0 North)
                , expected      = Just (Position 0 0 North)
                }
        , Case { description    = "Walk around and return to Start Position"
                , input         = "MMRMMRMMRMMR"
                , startPosition = Just (Position 0 0 North)
                , expected      = Just (Position 0 0 North)
                }
        , Case { description    = "5 Steps forward, turn left and 10 steps forward"
                , input         = "MMMMMLMMMMMMMMMM"
                , startPosition = Just (Position 0 0 North)
                , expected      = Just (Position (-10) 5 West)
                }
        , Case { description    = "Incorrect directions"
                , input         = "MMMRGGG"
                , startPosition = Just (Position 0 0 North)
                , expected      = Nothing
                }
        , Case { description    = "Incorrect Start Position"
                , input         = "LLLL"
                , startPosition = Nothing
                , expected      = Nothing
                }
        ]