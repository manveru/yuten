module Goban.Msgs exposing (..)

import List


type alias Point =
    ( Int, Int )


type Msg
    = StoneClick Point
    | StoneHover Point
    | StoneJitter Int
    | Replay (List Point)
