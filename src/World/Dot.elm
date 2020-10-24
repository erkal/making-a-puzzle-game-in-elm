module World.Dot exposing
    ( Dot
    , position
    , rotate60DegreesCCW
    , translateBy
    )

import Point exposing (Point)


type alias Dot =
    ( Int, Int )


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
