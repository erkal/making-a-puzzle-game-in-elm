module Main exposing (main)

import Playground exposing (..)


main =
    game view update initialModel


constants =
    { gameWidth = 12
    }


gameScale computer =
    computer.screen.width / constants.gameWidth


toGameCoordinates : Computer -> Mouse -> { x : Float, y : Float }
toGameCoordinates computer { x, y } =
    let
        k =
            gameScale computer
    in
    { x = 1 / k * x
    , y = 1 / k * y
    }



-- MODEL


type alias Model =
    List Square


type alias Square =
    ( Int, Int )


initialModel : Model
initialModel =
    [ ( 0, 0 )
    , ( 1, 1 )
    , ( 2, 2 )
    , ( 3, 3 )
    , ( 4, 4 )
    ]



-- UPDATE


update : Computer -> Model -> Model
update computer model =
    if computer.mouse.click then
        let
            m =
                computer.mouse |> toGameCoordinates computer

            newSquare =
                ( floor m.x, floor m.y )
        in
        newSquare :: model

    else
        model



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
        mouse =
            computer.mouse |> toGameCoordinates computer

        x_ =
            mouse.x |> String.fromFloat |> String.left 4

        y_ =
            mouse.y |> String.fromFloat |> String.left 4
    in
    group
        [ words black (x_ ++ ", " ++ y_)
        ]
        |> scale 4
        |> moveY (computer.screen.height / 2)
        |> moveY -30


viewGame : Computer -> Model -> List Shape
viewGame computer model =
    [ gridDots
    , squares model
    ]


squares : Model -> Shape
squares model =
    let
        square ( i, j ) =
            rectangle blue 1 1
                |> move (toFloat i) (toFloat j)
                |> move 0.5 0.5
    in
    group
        (model |> List.map square)


gridDots : Shape
gridDots =
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



-- HELPERS


cartesianProduct : List a -> List b -> List ( a, b )
cartesianProduct list1 list2 =
    let
        column j =
            list1 |> List.map (\i -> ( i, j ))
    in
    list2 |> List.concatMap column
