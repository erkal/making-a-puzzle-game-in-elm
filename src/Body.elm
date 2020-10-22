module Body exposing (Shape)

import Direction exposing (Direction)
import Dot exposing (Dot)


type Shape
    = Shape Dot (List Direction)


empty : Dot -> Shape
empty startDot =
    Shape startDot []
