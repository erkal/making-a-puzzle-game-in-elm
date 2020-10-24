module Point exposing (Point, rotateAround, translatBy)


type alias Point =
    { x : Float
    , y : Float
    }



--


rotateAround : Point -> Float -> Point -> Point
rotateAround center angle p =
    p
        |> translatBy ( -center.x, -center.y )
        |> rotateAroundOrigin angle
        |> translatBy ( center.x, center.y )


{-| rotates CounterClockwise for left handed coordinate system
-}
rotateAroundOrigin : Float -> Point -> Point
rotateAroundOrigin angle { x, y } =
    Point
        (x * cos -angle - y * sin -angle)
        (x * sin -angle + y * cos -angle)


translatBy : ( Float, Float ) -> Point -> Point
translatBy ( dx, dy ) { x, y } =
    Point
        (x + dx)
        (y + dy)
