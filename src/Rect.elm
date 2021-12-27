module Rect exposing (Corner(..), Rect, fitRectAspect, getAspect, getC, getHeight, getWidth, mkRect, mkRectDim, rectEmpty)

import Canvas exposing (Point)
import Util exposing (sign)


{-| Describes a rectangle via its upper-left corner and dimensions
-}
type alias Rect =
    { x1 : Float, y1 : Float, x2 : Float, y2 : Float }



type Corner
    = UL
    | UR
    | LL
    | LR



origin : Point
origin =
    ( -1, -1 )


{-| Make a rectangle with a corner and dimensions
Since the dimensions can be negative, it normalizes the result
-}
mkRect : Point -> Point -> Rect
mkRect ( x1, y1 ) ( x2, y2 ) =
    { x1 = min x1 x2, y1 = min y1 y2, x2 = max x1 x2, y2 = max y1 y2 }


rectEmpty : Rect
rectEmpty =
    mkRect origin origin


mkRectDim : Point -> Point -> Rect
mkRectDim ( x, y ) ( w, h ) =
    mkRect ( x, y ) ( x + w, y + h )


getWidth : Rect -> Float
getWidth { x1, x2 } =
    abs (x2 - x1)


getHeight : Rect -> Float
getHeight { y1, y2 } =
    abs (y2 - y1)


getAspect : Rect -> Float
getAspect r =
    getWidth r / getHeight r



fitRectAspect : Float -> Rect -> Rect
fitRectAspect aspect { x1, y1, x2, y2 } =
    let
        ( dX, dY ) =
            ( x2 - x1, y2 - y1 )

        dAspect =
            dX / dY

        -- dXRefl is dY translated via the apect ratio to be a difference in X
        -- (then we must also use the sign of dX)
        -- same of dYRefl but we must divide by the aspect ratio since aspect = w/h
        dXRefl =
            sign dX * (abs dY * aspect)

        dYRefl =
            sign dY * (abs dX / aspect)
    in
    -- I drew it on paper but can't explain properly. Depending on if the ration between the deltas
    -- is bigger or smaller than the aspect ratio we have to construct the selection rectangle differently.
    if abs dAspect > abs aspect then
        mkRectDim ( x1, y1 ) ( dX, dYRefl )

    else
        mkRectDim ( x1, y1 ) ( dXRefl, dY )


{-| Get a corner of a rectangle
-}
getC : Corner -> Rect -> Point
getC c { x1, y1, x2, y2 } =
    case c of
        UL ->
            ( x1, y1 )

        UR ->
            ( x2, y1 )

        LL ->
            ( x1, y2 )

        LR ->
            ( x2, y2 )

