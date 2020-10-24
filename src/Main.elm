module Main exposing (main)

import Playground exposing (..)
import World exposing (World)
import World.Direction exposing (Direction)
import World.Dot as Dot exposing (Dot)
import World.Path as Path exposing (Path)


main =
    game view update initialModel


constants =
    { gameWidth = 13
    }


cameraScale computer =
    constants.gameWidth / computer.screen.width



-- MODEL


type alias Model =
    { world : World
    , nearestDotToPointer : Dot
    , editor : Editor
    }


type Editor
    = Idle
    | DrawingBody Dot (List Direction)


initialModel : Model
initialModel =
    { world = World.init
    , nearestDotToPointer = ( 0, 0 )
    , editor = Idle
    }



-- UPDATE


update : Computer -> Model -> Model
update computer model =
    model
        |> updateNearestDotToPointer computer
        |> maybeStartDrawingBody computer
        |> maybeDrawBody computer
        |> maybeFinishDrawingBody computer


updateNearestDotToPointer : Computer -> Model -> Model
updateNearestDotToPointer computer model =
    { model
        | nearestDotToPointer =
            Dot.nearestTo
                { x = computer.mouse.x * cameraScale computer
                , y = computer.mouse.y * cameraScale computer
                }
    }


maybeStartDrawingBody : Computer -> Model -> Model
maybeStartDrawingBody computer model =
    case ( computer.keyboard.shift, model.editor ) of
        ( True, Idle ) ->
            { model
                | editor = DrawingBody model.nearestDotToPointer []
            }

        _ ->
            model


maybeDrawBody : Computer -> Model -> Model
maybeDrawBody computer model =
    case ( computer.keyboard.shift, computer.mouse.click, model.editor ) of
        ( True, True, DrawingBody startDot reversedOutline ) ->
            { model
                | editor =
                    let
                        lastDot =
                            startDot |> Dot.walk (List.reverse reversedOutline)
                    in
                    case Dot.neighbourIn lastDot model.nearestDotToPointer of
                        Just direction ->
                            DrawingBody startDot ((direction |> Debug.log "") :: reversedOutline)

                        Nothing ->
                            model.editor
            }

        _ ->
            model


maybeFinishDrawingBody : Computer -> Model -> Model
maybeFinishDrawingBody computer model =
    case ( computer.keyboard.shift, model.editor ) of
        ( False, DrawingBody startDot reversedOutline ) ->
            { model
                | editor = Idle
                , world = model.world |> World.add (Path.create startDot (List.reverse reversedOutline))
            }

        _ ->
            model



-- VIEW


view : Computer -> Model -> List Shape
view computer model =
    [ viewGame computer model
    ]


viewGame : Computer -> Model -> Shape
viewGame computer model =
    scale (1 / cameraScale computer)
        (group
            [ drawNearestDotToPointer model
            , drawDots
            , drawBodies model
            , drawDrawnBody model
            ]
        )


drawBodies : Model -> Shape
drawBodies { world } =
    group
        (List.map drawBody
            (world.bodiesBefore ++ [ world.activeBody ] ++ world.bodiesAfter)
        )


drawBody : Path -> Shape
drawBody body =
    let
        toPair { x, y } =
            ( x, y )
    in
    polygon black
        (body
            |> Path.dots
            |> List.map (Dot.position >> toPair)
        )


drawDot : Dot -> Shape
drawDot ( i, j ) =
    let
        { x, y } =
            Dot.position ( i, j )
    in
    group
        [ circle black 0.05

        --, words black (String.fromInt i ++ "/" ++ String.fromInt j)
        --    |> scale 0.02
        --    |> moveY 0.2
        ]
        |> move x y


drawDrawnBody : Model -> Shape
drawDrawnBody model =
    case model.editor of
        DrawingBody startDot directions ->
            drawBody (Path.create startDot (List.reverse directions))

        _ ->
            group []


drawDots : Shape
drawDots =
    group
        (List.map drawDot
            (cartesianProduct
                (List.range -5 5)
                (List.range -5 5)
            )
        )


cartesianProduct : List a -> List b -> List ( a, b )
cartesianProduct list1 list2 =
    let
        column j =
            list1 |> List.map (\i -> ( i, j ))
    in
    list2 |> List.concatMap column


drawNearestDotToPointer : Model -> Shape
drawNearestDotToPointer model =
    let
        { x, y } =
            Dot.position model.nearestDotToPointer
    in
    circle red 0.3
        |> move x y
