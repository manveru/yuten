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
import Svg.Lazy exposing (lazy)


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

        factor =
            case gridSize of
                9 ->
                    1.9

                _ ->
                    2.0
    in
    round <| toFloat radius / factor


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


stoneColor2Hex : StoneColor -> Bool -> StoneColor -> String
stoneColor2Hex currentPlayer hovered stoneColor =
    case stoneColor of
        BlackStone ->
            "#000"

        WhiteStone ->
            "#aaa"

        NeutralStone ->
            if hovered then
                stoneColor2Hex NeutralStone hovered currentPlayer
            else
                "#00000000"


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

        stoneGradient =
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
            [ circle
                (base
                    ++ [ fill stoneGradient
                       , Svg.Attributes.filter "url(#lighting1)"
                       ]
                )
                []
            , removedMarker model wasRemoved stone
            ]
    else
        g []
            [ circle
                (base
                    ++ [ fill stoneGradient
                       , Svg.Attributes.filter "url(#lighting1)"
                       ]
                )
                []
            , placedMarker model wasPlaced stone
            ]


view : Model -> Html.Html Msg
view model =
    lazy
        (\model ->
            svg
                [ viewBox "0 0 1000 1000"
                , preserveAspectRatio "xMinYMin meet"
                , width "100%"
                , height "100%"
                ]
                ([ boardBackground
                 , defs []
                    [ linearGradient [ id "boardGradient", fx "0.5", fy "1", gradientTransform "rotate(45 0,0)" ]
                        [ stop [ offset "0%", stopColor "#df9d25", stopOpacity "0.5" ] []
                        ]
                    , Svg.filter
                        [ id "lighting1", filterUnits "userSpaceOnUse", x "0%", y "0%", width "110%", height "110%" ]
                        [ feGaussianBlur [ in_ "SourceAlpha", result "shadowBlur", stdDeviation "5" ] []
                        , feOffset [ in_ "shadowBlur", result "shadow", dx "9", dy "9" ] []
                        , feGaussianBlur [ in_ "SourceAlpha", result "lightningBlur", stdDeviation "10" ] []
                        , feSpecularLighting [ in_ "lightningBlur", result "specOut", surfaceScale "10", specularConstant "1", specularExponent "50", lightingColor "#aaa" ]
                            [ fePointLight [ y "500", x "500", z "250" ] []
                            ]
                        , feComposite
                            [ in_ "specOut", in2 "SourceAlpha", result "specOut", operator "in" ]
                            []
                        , feGaussianBlur [ in_ "specOut", result "specOut", stdDeviation "2" ] []
                        , feComposite
                            [ in_ "SourceGraphic", in2 "specOut", result "litPaint", operator "arithmetic", k1 "1", k2 "1", k3 "1", k4 "0" ]
                            []
                        , feMerge []
                            [ feMergeNode [ in_ "shadow" ] []
                            , feMergeNode [ in_ "litPaint" ] []
                            ]
                        ]
                    , Svg.filter [ id "lighting" ]
                        [ feDiffuseLighting [ in_ "SourceGraphic", result "light", lightingColor "#fff" ]
                            [ fePointLight [ x "10000", y "10000", z "20000" ] []
                            ]
                        , feComposite
                            [ in_ "SourceGraphic"
                            , in2 "light"
                            , operator "arithmetic"
                            , k1 "1"
                            , k2 "0"
                            , k3 "0"
                            , k4 "0"
                            ]
                            []
                        ]
                    , Svg.filter [ id "sun" ]
                        [ feSpecularLighting
                            [ result "sun"
                            , specularConstant "1.0"
                            , specularExponent "100"
                            , lightingColor "#fff"
                            ]
                            [ fePointLight
                                [ x "1000"
                                , y "1000"
                                , z "1000"
                                , pointsAtX "0"
                                , pointsAtY "0"
                                , pointsAtZ "0"
                                ]
                                []
                            ]
                        , feComposite
                            [ in_ "SourceGraphic"
                            , in2 "sun"
                            , operator "atop"
                            , k1 "1"
                            , k2 "1"
                            , k3 "1"
                            , k4 "1"
                            ]
                            []
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
        )
        model


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

        -- , Svg.Attributes.style "filter:url(#sun)"
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
    [ g
        []
        (List.map
            (\v -> stone model v)
            values
        )
    ]
