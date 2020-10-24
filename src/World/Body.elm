module World.Body exposing (Body, create)

import World.Direction exposing (Direction)
import World.Dot exposing (Dot)


type Body
    = Body Dot (List Direction)


create =
    Body
