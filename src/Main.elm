module Main exposing (main)

import Playground exposing (..)


main =
    game view update ( 0, 0 )


view computer ( x, y ) =
    [ circle blue 40
        |> move x y
    ]


update computer ( x, y ) =
    ( x + toX computer.keyboard
    , y + toY computer.keyboard
    )
