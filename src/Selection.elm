module Selection exposing (Selection, SelectionMode(..), deactivate, isActive, isCleared, isSource, mkSelection, renderSelection, selDown, selMove, selUp)

import Canvas exposing (Point, Renderable, lineTo, path, rect, shapes)
import Canvas.Settings exposing (stroke)
import Canvas.Settings.Line exposing (LineJoin(..), lineDash, lineJoin, lineWidth)
import Color exposing (Color)
import Rect exposing (..)


type alias Selection =
    { rect : Rect, initialRect : Rect, color : Color, mode : SelectionMode }


type SelectionMode
    = SelectionSource
    | SelectionSink Bool


setActive : Bool -> SelectionMode -> SelectionMode
setActive b mode =
    case mode of
        SelectionSource ->
            SelectionSource

        SelectionSink _ ->
            SelectionSink b


isSource : Selection -> Bool
isSource { mode } =
    case mode of
        SelectionSource ->
            True

        _ ->
            False


isSink : Selection -> Bool
isSink { mode } =
    case mode of
        SelectionSink _ ->
            True

        _ ->
            False


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


isCleared : Selection -> Bool
isCleared { rect, initialRect } =
    rect == initialRect


isActive : Selection -> Bool
isActive sel =
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


deactivate : Selection -> Selection
deactivate sel =
    { sel | mode = setActive False sel.mode, rect = sel.initialRect }
