module Rect exposing (Corner(..), Rect, Selection, SelectionMode(..), deactivate, fitRectAspect, getAspect, getC, getHeight, getWidth, mkRect, mkRectDim, mkSelection, rectEmpty, renderSelection, selDown, selIsActive, selIsCleared, selMove, selUp)

import Array exposing (initialize)
import Canvas exposing (Point, Renderable, group, lineTo, path, rect, shapes, texture)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Advanced exposing (Transform, scale, transform, translate)
import Canvas.Settings.Line exposing (LineJoin(..), lineDash, lineDashOffset, lineJoin, lineWidth)
import Color exposing (Color)
import Util exposing (sign)


{-| Describes a rectangle via its upper-left corner and dimensions
-}
type alias Rect =
    { x1 : Float, y1 : Float, x2 : Float, y2 : Float }


type alias Selection =
    { rect : Rect, initialRect : Rect, color : Color, mode : SelectionMode }


type SelectionMode
    = SelectionSource
    | SelectionSink Bool


type Corner
    = UL
    | UR
    | LL
    | LR


setActive : Bool -> SelectionMode -> SelectionMode
setActive b mode =
    case mode of
        SelectionSource ->
            SelectionSource

        SelectionSink _ ->
            SelectionSink b


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


selDown : Point -> Selection -> Selection
selDown p sel =
    { sel | rect = mkRect p p, mode = setActive True sel.mode }


selMove : Point -> Selection -> Selection
selMove ( x2, y2 ) sel =
    let
        -- We cannot use the consturctors mkRect[Dim] because we must preserve the starting point.
        rect =
            sel.rect
    in
    { sel | rect = { rect | x2 = x2, y2 = y2 } }


selUp : Maybe Float -> Selection -> Selection
selUp aspectM sel =
    if getWidth sel.rect < 1 || getHeight sel.rect < 1 then
        deactivate sel

    else
        let
            aspectRect =
                case aspectM of
                    Nothing ->
                        sel.rect

                    Just aspect ->
                        fitRectAspect aspect sel.rect
        in
        { sel | rect = aspectRect }


{-| Make a selection wihch is inactive by default.
-}
mkSelection : Color -> SelectionMode -> Rect -> Selection
mkSelection color mode initialRect =
    { rect = initialRect, initialRect = initialRect, color = color, mode = mode }


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


selIsCleared : Selection -> Bool
selIsCleared { rect, initialRect } =
    rect == initialRect


selIsActive : Selection -> Bool
selIsActive sel =
    case sel.mode of
        SelectionSource ->
            True

        SelectionSink b ->
            b


renderSelection : Maybe Float -> Selection -> List Renderable
renderSelection aspectM sel =
    let
        rect =
            case aspectM of
                Nothing ->
                    sel.rect

                Just aspect ->
                    fitRectAspect aspect sel.rect
    in
    case sel.mode of
        SelectionSource ->
            [ shapes [ stroke sel.color, lineWidth 2, lineJoin RoundJoin ]
                [ path (getC UL rect)
                    -- we draw over the startpoint by ending in LL again so that the lineJoin works correctly
                    -- otherwise the first and last segment are not joined, elm-canvas apparently used to support closePath but doesn't anymore
                    (List.map (\c -> lineTo (getC c rect)) [ LL, LR, UR, UL, LL ])
                ]
            ]

        SelectionSink True ->
            [ shapes [ stroke sel.color, lineDash [ 5 ] ]
                [ path (getC UL rect)
                    -- we draw over the startpoint by ending in LL again so that the lineJoin works correctly
                    -- otherwise the first and last segment are not joined, elm-canvas apparently used to support closePath but doesn't anymore
                    (List.map (\c -> lineTo (getC c rect)) [ LL, LR, UR, UL, LL ])
                ]
            ]

        SelectionSink False ->
            []


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


deactivate : Selection -> Selection
deactivate sel =
    { sel | mode = setActive False sel.mode, rect = sel.initialRect }
