module Goban.Board exposing (view)

import Array
import Dict
import Goban.Models exposing (Model, Stone, StoneColor(..))
import Goban.Msgs exposing (..)
import Html
import Set
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, onMouseOut, onMouseOver)


border =
    150


boardSize =
    1000


innerSize =
    boardSize - border


boardLabels : Array.Array String
boardLabels =
    Array.fromList [ "A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" ]


calcOffsetInt : Int -> Int -> Int
calcOffsetInt gridSize count =
    let
        offset =
            round <| border / 2

        cell =
            round <| (innerSize / toFloat (gridSize - 1))
    in
    offset + (cell * (count - 1))


calcOffset : Int -> Int -> String
calcOffset gridSize count =
    toString <| calcOffsetInt gridSize count


stoneRadiusInt : Int -> Int
stoneRadiusInt gridSize =
    let
        radius =
            round <| innerSize / toFloat gridSize
    in
    round <| toFloat radius / 2.2


stoneRadius : Int -> String
stoneRadius gridSize =
    toString <| stoneRadiusInt gridSize


hoshiRadius : Int -> String
hoshiRadius gridSize =
    let
        radius =
            round <| innerSize / toFloat gridSize
    in
    toString <| round <| toFloat radius / 13


stoneColor2Hex : StoneColor -> Bool -> StoneColor -> ( String, String, String, String )
stoneColor2Hex currentPlayer hovered stoneColor =
    case stoneColor of
        BlackStone ->
            ( "url(#blackGradient)", "url(#shineGradient)", "url(#unshineGradient)", "url(#shadowGradient)" )

        WhiteStone ->
            ( "url(#whiteGradient)", "url(#shineGradient)", "url(#unshineGradient)", "url(#shadowGradient)" )

        NeutralStone ->
            if hovered then
                stoneColor2Hex NeutralStone hovered currentPlayer
            else
                ( "#00000000", "#00000000", "#00000000", "#00000000" )


stone : Model -> Stone -> Svg Msg
stone model stone =
    let
        gridSize =
            model.board.size

        ( x, y ) =
            stone.point

        ( jx, jy ) =
            stone.jitter

        wasRemoved =
            case Array.get (Array.length model.removed - 1) model.removed of
                Nothing ->
                    False

                Just removedStones ->
                    Set.member stone.point removedStones

        wasPlaced =
            case Array.get (Array.length model.moves - 1) model.moves of
                Nothing ->
                    False

                Just placedStone ->
                    placedStone.point == stone.point

        xPos =
            calcOffsetInt gridSize x + jx

        yPos =
            calcOffsetInt gridSize y + jy

        base =
            [ cx <| toString xPos
            , cy <| toString yPos
            , r <| stoneRadius gridSize
            ]

        ( stoneGradient, shineGradient, unshineGradient, shadowGradient ) =
            stoneColor2Hex model.currentPlayer
                (model.hoveredStone == ( x, y ))
                stone.color
    in
    if stone.color == NeutralStone then
        g
            [ onMouseOver (StoneHover ( x, y ))
            , onMouseOut (StoneHover ( -1, -1 ))
            , onClick (StoneClick ( x, y ))
            ]
            [ circle (base ++ [ fill stoneGradient ]) []
            , removedMarker model wasRemoved stone
            ]
    else
        g
            []
            [ ellipse
                [ cx <| toString <| xPos - 7
                , cy <| toString <| yPos + 1
                , rx <| toString <| (toFloat <| stoneRadiusInt gridSize) * 1.25
                , ry <| toString <| (toFloat <| stoneRadiusInt gridSize) * 1.1
                , fill shadowGradient
                , transform <| "rotate(45 " ++ calcOffset gridSize x ++ "," ++ calcOffset gridSize y ++ ")"
                ]
                []
            , circle (base ++ [ fill stoneGradient ]) []
            , ellipse
                [ cx <| toString <| xPos - 15
                , cy <| toString <| yPos + 10
                , rx <| toString <| (toFloat <| stoneRadiusInt gridSize) * 1.0
                , ry <| toString <| (toFloat <| stoneRadiusInt gridSize) * 1.0
                , fill shineGradient
                ]
                []
            , placedMarker model wasPlaced stone
            ]


view : Model -> Html.Html Msg
view model =
    svg
        [ viewBox "0 0 1000 1000"
        , preserveAspectRatio "xMinYMin meet"
        , width "100%"
        , height "100%"
        ]
        ([ boardBackground
         , defs []
            [ radialGradient [ id "unshineGradient", cx "0.9", cy "0.25" ]
                [ stop [ offset "0%", stopColor "#00000066" ] []
                , stop [ offset "100%", stopColor "#00000000" ] []
                ]
            , radialGradient [ id "shadowGradient", cx "0.5", cy "0.5" ]
                [ stop [ offset "0%", stopColor "#000000ff" ] []
                , stop [ offset "40%", stopColor "#000000ff" ] []
                , stop [ offset "100%", stopColor "#00000011" ] []
                ]
            , radialGradient [ id "whiteGradient", fx "0.75", fy "0.75" ]
                [ stop [ offset "0%", stopColor "#ffffff" ] []
                , stop [ offset "100%", stopColor "#a0a0a0", stopOpacity "1" ] []
                ]
            , radialGradient [ id "blackGradient", fx "0.75", fy "0.75" ]
                [ stop [ offset "0%", stopColor "#a0a0a0" ] []
                , stop [ offset "100%", stopColor "#000000", stopOpacity "0.9" ] []
                ]
            , linearGradient [ id "boardGradient", fx "0.5", fy "1", gradientTransform "rotate(45 0,0)" ]
                [ stop [ offset "0%", stopColor "#df9d25", stopOpacity "0.5" ] []
                , stop [ offset "90%", stopColor "#dfc28d", stopOpacity "0.5" ] []
                , stop [ offset "100%", stopColor "#dfd0b3", stopOpacity "0.5" ] []
                ]
            ]
         ]
            ++ horizontalLines model
            ++ horizontalLabels model
            ++ verticalLines model
            ++ verticalLabels model
            ++ hoshis model
            ++ boardShine
            ++ stones model
        )


placedMarker model show stone =
    let
        gridSize =
            model.board.size

        ( px, py ) =
            stone.point

        radius =
            stoneRadiusInt gridSize

        strokeColor =
            if stone.color == BlackStone then
                "#fff"
            else
                "#000"
    in
    if show then
        circle
            [ cx <| calcOffset gridSize px
            , cy <| calcOffset gridSize py
            , r <| toString (toFloat radius * 0.5)
            , stroke strokeColor
            , strokeWidth "2px"
            , fill "#00000000"
            ]
            []
    else
        rect [] []


removedMarker model show stone =
    let
        gridSize =
            model.board.size

        ( px, py ) =
            stone.point

        radius =
            stoneRadiusInt gridSize
    in
    if show then
        rect
            [ x <| toString <| calcOffsetInt gridSize px - round (toFloat radius * 0.5)
            , y <| toString <| calcOffsetInt gridSize py - round (toFloat radius * 0.5)
            , width <| toString radius
            , height <| toString radius
            , stroke "#000"
            , strokeWidth "2px"
            , fill "#00000000"
            ]
            []
    else
        rect [] []


hoshis : Model -> List (Svg msg)
hoshis model =
    let
        gridSize =
            model.board.size

        points =
            case gridSize of
                9 ->
                    [ ( 2, 2 ), ( 2, 6 ), ( 6, 2 ), ( 6, 6 ), ( 4, 4 ) ]

                13 ->
                    [ ( 3, 3 ), ( 3, 9 ), ( 9, 3 ), ( 9, 9 ), ( 6, 6 ) ]

                19 ->
                    [ ( 3, 3 ), ( 3, 15 ), ( 15, 3 ), ( 15, 15 ), ( 9, 9 ), ( 3, 9 ), ( 9, 3 ), ( 15, 9 ), ( 9, 15 ) ]

                _ ->
                    []
    in
    List.map
        (\point ->
            let
                ( x, y ) =
                    point
            in
            circle
                [ cx <| calcOffset gridSize <| (x + 1)
                , cy <| calcOffset gridSize <| (y + 1)
                , r <| hoshiRadius gridSize
                ]
                []
        )
        points


boardShine : List (Svg msg)
boardShine =
    [ rect
        [ x "0"
        , y "0"
        , width <| toString boardSize
        , height <| toString boardSize
        , rx "0"
        , ry "0"
        , fill "url(#boardGradient)"
        ]
        []
    ]


boardBackground : Svg msg
boardBackground =
    rect
        [ x "0"
        , y "0"
        , width <| toString boardSize
        , height <| toString boardSize
        , rx "0"
        , ry "0"
        , fill "#dfa335"
        ]
        []


verticalLines : Model -> List (Svg msg)
verticalLines model =
    let
        gridSize =
            model.board.size
    in
    List.range 1 gridSize
        |> List.map
            (\n ->
                line
                    [ x1 <| calcOffset gridSize 1
                    , y1 <| calcOffset gridSize n
                    , x2 <| calcOffset gridSize gridSize
                    , y2 <| calcOffset gridSize n
                    , stroke "#000"
                    ]
                    []
            )


verticalLabels : Model -> List (Svg msg)
verticalLabels model =
    let
        gridSize =
            model.board.size
    in
    List.range 1 gridSize
        |> List.map
            (\n ->
                g []
                    [ text_
                        [ x <| toString <| calcOffsetInt gridSize 1 - 60
                        , y <| toString <| calcOffsetInt gridSize (gridSize - (n - 1))
                        , stroke "#000"
                        ]
                        [ text <| toString n ]
                    , text_
                        [ x <| toString <| calcOffsetInt gridSize gridSize + 60
                        , y <| toString <| calcOffsetInt gridSize (gridSize - (n - 1))
                        , stroke "#000"
                        ]
                        [ text <| toString n ]
                    ]
            )


horizontalLines model =
    let
        gridSize =
            model.board.size
    in
    List.range 1 gridSize
        |> List.map
            (\n ->
                line
                    [ x1 <| calcOffset gridSize n
                    , y1 <| calcOffset gridSize 1
                    , x2 <| calcOffset gridSize n
                    , y2 <| calcOffset gridSize gridSize
                    , stroke "#000"
                    ]
                    []
            )


horizontalLabels model =
    let
        gridSize =
            model.board.size
    in
    List.range 1 gridSize
        |> List.indexedMap
            (\i ->
                \n ->
                    g []
                        [ text_
                            [ x <| toString <| calcOffsetInt gridSize n
                            , y <| toString <| calcOffsetInt gridSize 1 - 60
                            , stroke "#000"
                            ]
                            [ text <| Maybe.withDefault "" <| Array.get i boardLabels ]
                        , text_
                            [ x <| toString <| calcOffsetInt gridSize n
                            , y <| toString <| calcOffsetInt gridSize gridSize + 60
                            , stroke "#000"
                            ]
                            [ text <| Maybe.withDefault "" <| Array.get i boardLabels ]
                        ]
            )


stones model =
    let
        values =
            Dict.values model.board.stones
    in
    List.map (\v -> stone model v) values
