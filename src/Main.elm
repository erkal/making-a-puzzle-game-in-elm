module Main exposing (main)

import Playground exposing (..)
import World exposing (World)
import World.Dot as Dot exposing (Dot)


main =
    game view update initialModel


constants =
    { gameWidth = 13
    }



-- MODEL


type alias Model =
    { level : World
    , nearestDotToPointer : Dot
    }


initialModel : Model
initialModel =
    { level = World.init
    , nearestDotToPointer = ( 0, 0 )
    }



-- UPDATE


update : Computer -> Model -> Model
update computer model =
    { model
        | nearestDotToPointer =
            Dot.nearestTo
                { x = computer.mouse.x / computer.screen.width * constants.gameWidth
                , y = computer.mouse.y / computer.screen.width * constants.gameWidth
                }
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

        allDots =
            cartesianProduct
                (List.range -5 5)
                (List.range -5 5)

        drawDot ( i, j ) =
            let
                { x, y } =
                    Dot.position ( i, j )
            in
            group
                [ circle blue 0.05
                , words black (String.fromInt i ++ "/" ++ String.fromInt j)
                    |> scale 0.02
                    |> moveY 0.2
                ]
                |> move x y
    in
    group (List.map drawDot allDots)


drawNearestDotToPointer : Model -> Shape
drawNearestDotToPointer model =
    let
        { x, y } =
            Dot.position model.nearestDotToPointer
    in
    circle red 0.3
        |> move x y
