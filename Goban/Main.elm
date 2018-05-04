module Main exposing (..)

import Array
import Dict
import Goban.Board
import Goban.Models exposing (Model, Stone, StoneColor(..), emptyBoard, invertColor)
import Goban.Msgs exposing (..)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Set
import Task


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , subscriptions = subscriptions
        , update = update
        }


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( { hoveredStone = ( -1, -1 )
      , clickedStone = ( -1, -1 )
      , board = emptyBoard 19
      , moves = Array.fromList []
      , removed = Array.fromList []
      , previousBoard = emptyBoard 9
      , currentPlayer = BlackStone
      }
    , Task.succeed (Replay initialMoves) |> Task.perform identity
    )


initialMoves : List Point
initialMoves =
    [-- ( 5, 5 )
     -- , ( 6, 5 )
     -- , ( 6, 6 )
     -- , ( 7, 6 )
     -- , ( 7, 7 )
     -- , ( 8, 7 )
     -- , ( 8, 8 )
     -- , ( 9, 8 )
     -- , ( 9, 9 )
     -- , ( 8, 6 )
     -- , ( 9, 6 )
     -- , ( 9, 5 )
     -- , ( 7, 5 )
     -- , ( 6, 4 )
     -- , ( 7, 4 )
     -- , ( 7, 3 )
     -- , ( 8, 4 )
     -- , ( 8, 3 )
     -- , ( 9, 4 )
     -- , ( 9, 3 )
     -- , ( 8, 5 )
     -- , ( 9, 6 )
     -- , ( 8, 5 )
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoneClick pos ->
            -- Random.generate StoneJitter (Random.int -2 2)
            ( setStone model pos, Cmd.none )

        StoneHover pos ->
            ( { model | hoveredStone = pos }, Cmd.none )

        StoneJitter jitter ->
            ( causeDisturbance model ( jitter, jitter ), Cmd.none )

        Replay remaining ->
            case List.head remaining of
                Nothing ->
                    ( model, Cmd.none )

                Just point ->
                    case List.tail remaining of
                        Nothing ->
                            ( model, Cmd.none )

                        Just tail ->
                            ( setStone model point, Task.succeed (Replay tail) |> Task.perform identity )


causeDisturbance : Model -> Point -> Model
causeDisturbance model jitter =
    case Array.get (Array.length model.moves - 1) model.moves of
        Nothing ->
            model

        Just placedStone ->
            let
                movedStones =
                    List.foldl
                        (\fun ->
                            \sum ->
                                case fun model placedStone.point of
                                    Nothing ->
                                        sum

                                    Just stone ->
                                        let
                                            ( jx, jy ) =
                                                jitter

                                            ( sx, sy ) =
                                                stone.jitter
                                        in
                                        Dict.insert stone.point { stone | jitter = ( jx + sx, jy + sy ) } sum
                        )
                        Dict.empty
                        [ nPos, ePos, sPos, wPos ]

                newStones =
                    Dict.union movedStones model.board.stones

                newBoard =
                    { stones = newStones, size = model.board.size }
            in
            { model | board = newBoard }


setStone : Model -> Point -> Model
setStone model pos =
    let
        newStone =
            { color = model.currentPlayer, point = pos, jitter = ( 0, 0 ) }

        willHaveFreedom =
            hasFreedom model newStone

        newBoardStones =
            Dict.insert pos newStone model.board.stones

        capturePositions =
            captureGroups model newStone

        newBoardStonesMinusCaptures =
            Dict.map
                (\_ ->
                    \stone ->
                        if Set.member stone.point capturePositions then
                            { color = NeutralStone, point = stone.point, jitter = ( 0, 0 ) }
                        else
                            stone
                )
                newBoardStones

        newBoard =
            { stones = newBoardStonesMinusCaptures, size = model.board.size }

        newPlayer =
            if model.currentPlayer == BlackStone then
                WhiteStone
            else
                BlackStone

        newBoardIsOldBoard =
            Array.length model.moves /= 0 && (model.previousBoard.stones == newBoard.stones)
    in
    if newBoardIsOldBoard then
        model
    else if willHaveFreedom || Set.size capturePositions > 0 then
        { model
            | clickedStone = pos
            , moves = Array.push newStone model.moves
            , removed = Array.push capturePositions model.removed
            , previousBoard = model.board
            , board = newBoard
            , currentPlayer = newPlayer
        }
    else
        model



-- doesn't consider capturing, only whether we'll have a freedom left if we place a stone here.


hasFreedom : Model -> Stone -> Bool
hasFreedom model stoneToBePlaced =
    case Dict.get stoneToBePlaced.point model.board.stones of
        Nothing ->
            -- out of board
            False

        Just stone ->
            if stone.color == NeutralStone then
                let
                    ( freedoms, _ ) =
                        freedomCount model stoneToBePlaced.color (Set.fromList [ stone.point ]) stone.point
                in
                freedoms >= 1
            else
                False


freedomCount : Model -> StoneColor -> Set.Set Point -> Point -> ( Int, Set.Set Point )
freedomCount model color seen point =
    List.foldl
        (\fun ->
            \sum ->
                let
                    ( count, allSeen ) =
                        sum
                in
                case fun model point of
                    Nothing ->
                        ( count, allSeen )

                    Just stone ->
                        let
                            newSeen =
                                Set.insert stone.point allSeen
                        in
                        case stone.color of
                            NeutralStone ->
                                if Set.member stone.point allSeen then
                                    ( count, allSeen )
                                else
                                    ( count + 1, allSeen )

                            _ ->
                                if stone.color == color && Set.member stone.point allSeen /= True then
                                    let
                                        ( subCount, subSeen ) =
                                            freedomCount model color newSeen stone.point
                                    in
                                    ( count + subCount, Set.union allSeen subSeen )
                                else
                                    ( count, allSeen )
        )
        ( 0, seen )
        [ nPos, ePos, sPos, wPos ]



{-
   if we place a stone:
   can it capture enemy stones?


   Case 1:
     Black places a stone at X, checking n,e,s,w each returns a group of 1 stone and 1 freedom.
     Since the freedom is the same as the stone we want to place, the groups get captured

        12345
      1 ++#++
      2 +#O#+
      3 #OXO#
      4 +#O#+
      5 ++#++

   Result:
     [((3,2),(3,3))
     ,((2,3),(3,3))
     ,((4,3),(3,3))
     ,((3,4),(3,3))] -> [(3,2),(2,3),(4,3),(3,4)]
-}


captureGroups : Model -> Stone -> Set.Set Point
captureGroups model stoneToBePlaced =
    List.foldl
        (\fun ->
            \sum ->
                case fun model stoneToBePlaced.point of
                    Nothing ->
                        sum

                    Just stone ->
                        let
                            ( group, freedoms ) =
                                groupAndFreedoms model stone

                            hasOnlyOneFreedom =
                                Set.size freedoms == 1

                            willOccupyFreedom =
                                Set.size (Set.filter (\s -> s == stoneToBePlaced.point) freedoms) == 1
                        in
                        if stone.color /= stoneToBePlaced.color && hasOnlyOneFreedom && willOccupyFreedom then
                            Set.union group sum
                        else
                            sum
        )
        Set.empty
        [ nPos, ePos, sPos, wPos ]



-- finds the points neighbours of the same color and collects all the freedom points they have


groupAndFreedoms : Model -> Stone -> ( Set.Set Point, Set.Set Point )
groupAndFreedoms model stone =
    case stone.color of
        NeutralStone ->
            ( Set.empty, Set.fromList [ stone.point ] )

        _ ->
            groupAndFreedomsInner model stone (Set.fromList [ stone.point ]) Set.empty


groupAndFreedomsInner : Model -> Stone -> Set.Set Point -> Set.Set Point -> ( Set.Set Point, Set.Set Point )
groupAndFreedomsInner model stone parentNeighbours parentFreedoms =
    List.foldl
        (\fun ->
            \sum ->
                let
                    ( neighbours, freedoms ) =
                        sum
                in
                case fun model stone.point of
                    Nothing ->
                        ( neighbours, freedoms )

                    Just neighbour ->
                        if Set.member neighbour.point neighbours || Set.member neighbour.point freedoms then
                            -- we already saw that one
                            ( neighbours, freedoms )
                        else if neighbour.color == NeutralStone then
                            -- no need to look for neighbours of a freedom
                            ( neighbours, Set.insert neighbour.point freedoms )
                        else if neighbour.color == stone.color then
                            -- go deeper
                            groupAndFreedomsInner model neighbour (Set.insert neighbour.point neighbours) freedoms
                        else
                            -- spotted the enemy
                            ( neighbours, freedoms )
        )
        ( parentNeighbours, parentFreedoms )
        [ nPos, ePos, sPos, wPos ]



-- Find all the adjacent groups that can be capture it a stone is placed at the given point
-- first go through all neighbours, and try to find an enemy stone
-- if there is an enemy stone, check that all the neighbours have only one freedom in total
-- if only one freedom exists, get the whole enemy group associated with it


upForCapture : Model -> StoneColor -> Point -> Set.Set Point
upForCapture model captorColor point =
    List.foldl
        (\fun ->
            \captured ->
                case fun model point of
                    Nothing ->
                        captured

                    Just stone ->
                        case stone.color of
                            NeutralStone ->
                                captured

                            _ ->
                                if stone.color == captorColor then
                                    captured
                                else if
                                    let
                                        ( count, _ ) =
                                            freedomCount
                                                model
                                                (invertColor captorColor)
                                                (Set.fromList [ stone.point ])
                                                stone.point
                                    in
                                    count
                                        == 1
                                then
                                    Set.union captured
                                        (getGroup model
                                            (invertColor captorColor)
                                            Set.empty
                                            stone.point
                                        )
                                else
                                    captured
        )
        Set.empty
        [ nPos, ePos, sPos, wPos ]


getGroup : Model -> StoneColor -> Set.Set Point -> Point -> Set.Set Point
getGroup model groupColor seen point =
    List.foldl
        (\fun ->
            \group ->
                case fun model point of
                    Nothing ->
                        group

                    Just stone ->
                        let
                            newSeen =
                                Set.insert stone.point seen
                        in
                        case stone.color of
                            NeutralStone ->
                                group

                            _ ->
                                if stone.color == groupColor && Set.member stone.point seen == False then
                                    Set.union group (getGroup model groupColor newSeen stone.point)
                                else
                                    group
        )
        (Set.fromList [ point ])
        [ nPos, ePos, sPos, wPos ]


nPos : Model -> Point -> Maybe Stone
nPos model point =
    relPos model point ( 0, -1 )


ePos : Model -> Point -> Maybe Stone
ePos model point =
    relPos model point ( 1, 0 )


sPos : Model -> Point -> Maybe Stone
sPos model point =
    relPos model point ( 0, 1 )


wPos : Model -> Point -> Maybe Stone
wPos model point =
    relPos model point ( -1, 0 )


atPos : Model -> Point -> Maybe Stone
atPos model point =
    Dict.get point model.board.stones


relPos : Model -> Point -> Point -> Maybe Stone
relPos model point modPoint =
    let
        ( x, y ) =
            point

        ( x1, y1 ) =
            modPoint
    in
    atPos model ( x + x1, y + y1 )


boardStyle : List ( String, String )
boardStyle =
    [ ( "display", "flex" )
    , ( "justify-content", "center" )
    , ( "align-items", "center" )
    , ( "width", "90vw" )
    , ( "max-width", "90vw" )
    , ( "height", "90vh" )
    , ( "max-height", "90vh" )
    ]


wrapStyle : List ( String, String )
wrapStyle =
    [ ( "display", "flex" )
    , ( "justify-content", "center" )
    , ( "align-items", "center" )
    , ( "width", "100vw" )
    , ( "height", "100vh" )
    ]


view : Model -> Html Msg
view model =
    div [ style wrapStyle ]
        [ div [ style boardStyle ]
            [ Goban.Board.view model
            ]
        ]
