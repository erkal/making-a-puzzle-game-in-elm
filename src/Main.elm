module Main exposing (main)

import Dot exposing (Dot)
import Playground exposing (..)


main =
    game view update initialModel


constants =
    { gameWidth = 13
    }



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
            let
                toGameCoordinates { x, y } =
                    ( x / computer.screen.width * constants.gameWidth
                    , y / computer.screen.width * constants.gameWidth
                    )
            in
            computer.mouse
                |> toGameCoordinates
                |> Tuple.mapBoth round round
    }



-- VIEW


view : Computer -> Model -> List Shape
view computer model =
    [ viewGame computer model
    ]


viewGame : Computer -> Model -> Shape
viewGame computer model =
    scale (computer.screen.width / constants.gameWidth)
        (group
            [ drawNearestDotToPointer model
            , drawDots
            , drawShapes model
            ]
        )


drawShapes : Model -> Shape
drawShapes model =
    group
        []


drawDots : Shape
drawDots =
    let
        cartesianProduct : List a -> List b -> List ( a, b )
        cartesianProduct list1 list2 =
            let
                column j =
                    list1 |> List.map (\i -> ( i, j ))
            in
            list2 |> List.concatMap column

        allCoordinates =
            cartesianProduct
                (List.range -5 5)
                (List.range -5 5)

        drawDot ( i, j ) =
            group
                [ circle blue 0.05
                , words black (String.fromInt i ++ "/" ++ String.fromInt j)
                    |> scale 0.02
                    |> moveY 0.2
                ]
                |> move (toFloat i) (toFloat j)
    in
    group (List.map drawDot allCoordinates)


drawNearestDotToPointer : Model -> Shape
drawNearestDotToPointer model =
    let
        ( x, y ) =
            model.nearestDotToPointer
    in
    circle red 0.3
        |> moveX (toFloat x)
        |> moveY (toFloat y)
