module World.Path exposing
    ( Path
    , create
    , dots
    )

import World.Direction exposing (Direction)
import World.Dot exposing (Dot, moveTo)


type Path
    = Path Dot (List Direction)


create =
    Path


dots : Path -> List Dot
dots (Path startDot outline) =
    let
        helper direction ( lastDot, acc ) =
            let
                newDot =
                    lastDot |> moveTo direction
            in
            ( newDot, acc ++ [ newDot ] )
    in
    startDot
        :: (outline
                |> List.foldl helper ( startDot, [] )
                |> Tuple.second
           )
