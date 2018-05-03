module Goban.Models exposing (..)

import Array
import Dict exposing (Dict)
import Goban.Msgs exposing (..)
import Set


type StoneColor
    = BlackStone
    | WhiteStone
    | NeutralStone


type alias Model =
    { hoveredStone : Point
    , clickedStone : Point
    , moves : Array.Array Stone
    , removed : Array.Array (Set.Set Point)
    , board : Board
    , previousBoard : Board
    , currentPlayer : StoneColor
    }


type alias Board =
    { stones : Dict Point Stone
    , size : Int
    }


type alias Stone =
    { color : StoneColor
    , point : Point
    , jitter : ( Int, Int )
    }


emptyBoard : Int -> Board
emptyBoard size =
    let
        ys =
            List.range 1 size

        xs =
            List.range 1 size

        stones =
            Dict.fromList <|
                List.concat
                    (List.map
                        (\x ->
                            List.map
                                (\y ->
                                    ( ( x, y ), { color = NeutralStone, point = ( x, y ), jitter = ( 0, 0 ) } )
                                )
                                ys
                        )
                        xs
                    )
    in
    { stones = stones, size = size }


invertColor : StoneColor -> StoneColor
invertColor color =
    case color of
        BlackStone ->
            WhiteStone

        WhiteStone ->
            BlackStone

        NeutralStone ->
            NeutralStone
