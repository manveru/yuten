module Goban.History exposing (Tree)

import Goban.Msgs exposing (Point)


type Tree a
    = Tree
        { position : Point
        , children : Maybe (Node a)
        }


type alias Node a =
    { current : Tree a
    , prev : List (Tree a)
    , next : List (Tree a)
    }
