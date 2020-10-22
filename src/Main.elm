module Main exposing (main)

import Dot exposing (Dot)
import Playground exposing (..)


main =
    game view update initialModel


constants =
    { gameWidth = 12
    }


gameScale computer =
    computer.screen.width / constants.gameWidth


toGameCoordinates : Computer -> Mouse -> ( Float, Float )
toGameCoordinates computer { x, y } =
    let
        k =
            gameScale computer
    in
    ( 1 / k * x
    , 1 / k * y
    )



-- MODEL


type alias Model =
    { level : List Shape
    , nearestDotToPointer : Dot
    }


initialModel : Model
initialModel =
    { level = []
    , nearestDotToPointer = ( 0, 0 )
    }



-- UPDATE


update : Computer -> Model -> Model
update computer model =
    { model
        | nearestDotToPointer =
            computer.mouse
                |> toGameCoordinates computer
                |> Tuple.mapBoth round round
    }



-- VIEW


view : Computer -> Model -> List Shape
view computer model =
    [ group (viewGame computer model)
        |> scale (gameScale computer)
    , viewHud computer model
    ]


viewHud : Computer -> Model -> Shape
viewHud computer model =
    let
        ( x_, y_ ) =
            model.nearestDotToPointer
    in
    group
        [ words black (String.fromInt x_ ++ ", " ++ String.fromInt y_)
        ]
        |> scale 2
        |> moveY (computer.screen.height / 2)
        |> moveY -30


viewGame : Computer -> Model -> List Shape
viewGame computer model =
    [ drawDots
    , drawNearestDotToPointer model
    , shapes model
    ]


shapes : Model -> Shape
shapes model =
    group
        []


drawDots : Shape
drawDots =
    let
        dot ( i, j ) =
            group
                [ circle blue 0.05

                --, words black (String.fromInt i ++ "," ++ String.fromInt j)
                --    |> scale 0.02
                --    |> moveY 0.25
                ]
                |> move (toFloat i) (toFloat j)
    in
    group
        (cartesianProduct
            (List.range -5 5)
            (List.range -5 5)
            |> List.map dot
        )


drawNearestDotToPointer : Model -> Shape
drawNearestDotToPointer model =
    let
        ( x, y ) =
            model.nearestDotToPointer
    in
    circle red 0.3
        |> moveX (toFloat x)
        |> moveY (toFloat y)



-- HELPERS


cartesianProduct : List a -> List b -> List ( a, b )
cartesianProduct list1 list2 =
    let
        column j =
            list1 |> List.map (\i -> ( i, j ))
    in
    list2 |> List.concatMap column
