module Rect exposing (Corner(..), Rect, Selection, deactivate, fitRectAspect, getC, height, mkRect, mkRectDim, mkSelection, renderSelection, selDown, selMove, selUp, width)

import Canvas exposing (Point, Renderable, group, lineTo, path, rect, shapes, texture)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Advanced exposing (Transform, scale, transform, translate)
import Color exposing (Color)
import Util exposing (sign)


{-| Describes a rectangle via its upper-left corner and dimensions
-}
type alias Rect =
    { x1 : Float, y1 : Float, x2 : Float, y2 : Float }


type alias Selection =
    { rect : Rect, color : Color, isActive : Bool }


type Corner
    = UL
    | UR
    | LL
    | LR


{-| Make a rectangle with a corner and dimensions
Since the dimensions can be negative, it normalizes the result
-}
mkRect : Point -> Point -> Rect
mkRect ( x1, y1 ) ( x2, y2 ) =
    { x1 = min x1 x2, y1 = min y1 y2, x2 = max x1 x2, y2 = max y1 y2 }


mkRectDim : Point -> Point -> Rect
mkRectDim ( x, y ) ( w, h ) =
    mkRect ( x, y ) ( x + w, y + h )


origin : Point
origin =
    ( -1, -1 )


selDown : Point -> Selection -> Selection
selDown ( x, y ) sel =
    { sel | rect = { x1 = x, y1 = y, x2 = x, y2 = y }, isActive = True }


selMove : Point -> Selection -> Selection
selMove ( x2, y2 ) sel =
    let
        -- We cannot use the consturctors mkRect[Dim] because we must preserve the starting point.
        newRect =
            { x1 = sel.rect.x1, y1 = sel.rect.y1, x2 = x2, y2 = y2 }
    in
    { sel | rect = newRect }


selUp : Selection -> Selection
selUp sel =
    sel


{-| Make a selection wihch is inactive by default.
-}
mkSelection : Color -> Selection
mkSelection c =
    { rect = mkRect origin origin, color = c, isActive = False }


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


renderSelection : Float -> Selection -> List Renderable
renderSelection aspect { rect, color, isActive } =
    let
        aspectRect =
            fitRectAspect aspect rect
    in
    if isActive then
        [ shapes [ stroke color ]
            [ path (getC aspectRect UL)
                (List.map (\c -> lineTo (getC aspectRect c)) [ LL, LR, UR, UL ])
            ]
        ]

    else
        []


{-| Get a corner of a rectangle
-}
getC : Rect -> Corner -> Point
getC { x1, y1, x2, y2 } c =
    case c of
        UL ->
            ( x1, y1 )

        UR ->
            ( x2, y1 )

        LL ->
            ( x1, y2 )

        LR ->
            ( x2, y2 )


width { x1, x2 } =
    x2 - x1


height { y1, y2 } =
    y2 - y1


deactivate : Selection -> Selection
deactivate s =
    { s | isActive = False }
