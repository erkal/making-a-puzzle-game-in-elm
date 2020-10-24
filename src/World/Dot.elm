module World.Dot exposing
    ( Dot
    , moveTo
    , nearestTo
    , neighbourIn
    , position
    , rotate60DegreesCCW
    , translateBy
    , walk
    )

import Point exposing (Point)
import World.Direction exposing (Direction(..))


type alias Dot =
    ( Int, Int )


nearestTo : Point -> Dot
nearestTo point =
    let
        { x, y } =
            point |> toTrixelCoordinates
    in
    ( round x
    , round y
    )


toTrixelCoordinates : Point -> Point
toTrixelCoordinates { x, y } =
    Point
        (x / cos (degrees 30))
        (y - x * sin (degrees 30))


position : Dot -> Point
position ( x, y ) =
    Point
        (cos (degrees 30) * toFloat x)
        (toFloat y + sin (degrees 30) * toFloat x)


rotate60DegreesCCW : Dot -> Dot
rotate60DegreesCCW ( x, y ) =
    ( x + y
    , -x
    )


translateBy : ( Int, Int ) -> Dot -> Dot
translateBy ( dx, dy ) ( x, y ) =
    ( x + dx
    , y + dy
    )


rotate60DegreesCCWAround : Dot -> Dot -> Dot
rotate60DegreesCCWAround ( rotationCenterX, rotationCenterY ) =
    translateBy ( -rotationCenterX, -rotationCenterY )
        >> rotate60DegreesCCW
        >> translateBy ( rotationCenterX, rotationCenterY )


neighbourIn : Dot -> Dot -> Maybe Direction
neighbourIn ( i, j ) ( i_, j_ ) =
    if i_ == i + 1 && j_ == j then
        Just Deg30

    else if i_ == i && j_ == j + 1 then
        Just Deg90

    else if i_ == i - 1 && j_ == j + 1 then
        Just Deg150

    else if i_ == i - 1 && j_ == j then
        Just Deg210

    else if i_ == i && j_ == j - 1 then
        Just Deg270

    else if i_ == i + 1 && j_ == j - 1 then
        Just Deg330

    else
        Nothing


walk : List Direction -> Dot -> Dot
walk directions dot =
    List.foldl moveTo dot directions


moveTo : Direction -> Dot -> Dot
moveTo direction ( i, j ) =
    case direction of
        Deg30 ->
            ( i + 1, j )

        Deg90 ->
            ( i, j + 1 )

        Deg150 ->
            ( i - 1, j + 1 )

        Deg210 ->
            ( i - 1, j )

        Deg270 ->
            ( i, j - 1 )

        Deg330 ->
            ( i + 1, j - 1 )
