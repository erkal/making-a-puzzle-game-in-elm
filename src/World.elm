module World exposing
    ( RollAttemptResult(..)
    , RollDirection(..)
    , World
    , init
    , roll
    )

import Point exposing (Point)
import World.Body as Body exposing (Body)
import World.Direction exposing (Direction(..))


type World
    = World
        { bodiesBefore : List Body
        , activeBody : Body
        , bodiesAfter : List Body
        }


type RollDirection
    = Clockwise
    | Counterclockwise


type RollAttemptResult
    = NoRollBeacuseOfCollisionAt { rotationCenter : Point, collisionAtAngle : Float }
    | Roll { rotationCenter : Point, newWorld : World }


init : World
init =
    World
        { bodiesBefore = []
        , activeBody = Body.create ( 0, 0 ) [ Deg30, Deg210, Deg330 ]
        , bodiesAfter = []
        }


roll : RollDirection -> World -> ( World, RollAttemptResult )
roll rollDirection level =
    ( level
    , Debug.todo ""
    )
