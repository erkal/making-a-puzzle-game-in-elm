module World exposing (..)

import World.Direction exposing (Direction(..))
import World.Path as Body exposing (Path)


type alias World =
    { bodiesBefore : List Path
    , activeBody : Path
    , bodiesAfter : List Path
    }


init : World
init =
    { bodiesBefore = []
    , activeBody = Body.create ( 0, 0 ) [ Deg30, Deg150, Deg270 ]
    , bodiesAfter = []
    }


add : Path -> World -> World
add newBody world =
    { world
        | activeBody = newBody
        , bodiesAfter = world.activeBody :: world.bodiesAfter
    }



--
--type RollDirection
--    = Clockwise
--    | Counterclockwise
--
--
--type RollAttemptResult
--    = NoRollBeacuseOfCollisionAt { rotationCenter : Point, collisionAtAngle : Float }
--    | Roll { rotationCenter : Point, newWorld : World }
--
--roll : RollDirection -> World -> ( World, RollAttemptResult )
--roll rollDirection level =
--    ( level
--    , Debug.todo ""
--    )
