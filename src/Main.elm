module Main exposing (main)

import Browser
import Canvas exposing (Point, Renderable, circle, group, lineTo, path, rect, shapes, texture)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Advanced exposing (Transform, scale, transform, translate)
import Canvas.Settings.Line exposing (lineWidth)
import Canvas.Texture as T exposing (Source, Texture)
import Color exposing (Color)
import Debug exposing (toString)
import File exposing (File)
import Html exposing (Html, button, div, input, label, source, text)
import Html.Attributes exposing (checked, for, id, style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Html.Events.Extra.Drag exposing (startPortData)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as D
import Rect as R exposing (Rect, Selection, SelectionMode(..), fitRectAspect, mkRect, mkRectDim, mkSelection, rectEmpty, selDown, selMove, selUp)
import Task
import Tuple exposing (first, second)
import Util exposing (iter, iterCollect, list_mapi, trunc_tail)
import Zipper as Z exposing (Movement, Zipper, mkZipper, move)


{-| The model of this app. It encapsulates the menu flags as well as the state of the canvas.

  - tex : The texture that we draw at the moment
  - renders : The list of renderables that are drawn on the canvas. This is what the user sees.
  - initialRenders : The initial list of renders that is used to compute `renders` every time the user makes a change. Stamping updates this list.
  - cDim : The dimension of the canvas in pixels.
  - startPos : The start position of the selection rectangle.
  - curPos : The last position of the selection rectangle.
  - rectSet : If the rectangle is "finished", i.e. if the user relased the mouse after dragging.
  - dragging : If the selection rectangle is in the process of being changed.
  - focussed :
  - url : File url of the image in the texture. When this is updated, the canvas will load a new texture.
  - recSteps : How many steps of recursion should be done.

-}
type alias Model =
    { tex : Maybe Texture
    , renders : List Renderable
    , initialRender : Renderable
    , cDim : Point
    , aspectM : Maybe Float
    , selections : Zipper Selection
    , dragging : Bool
    , url : String
    , recSteps : Int
    }


initialModel : Model
initialModel =
    let
        canvasDim =
            ( 400, 400 )

        sourceSel =
            R.mkSelection Color.black R.SelectionSource rectEmpty

        sinkSels =
            List.map (\c -> R.mkSelection c (R.SelectionSink False) rectEmpty) [ Color.red, Color.green, Color.blue, Color.orange ]

        selections =
            mkZipper sourceSel sinkSels
    in
    { tex = Nothing
    , renders = []
    , initialRender = clearCanvas canvasDim Color.lightGrey
    , cDim = canvasDim
    , aspectM = Nothing
    , selections = selections
    , dragging = False
    , url = ""
    , recSteps = 1
    }


getActiveAspect : Model -> Float
getActiveAspect { aspectM, cDim } =
    case aspectM of
        Just asp ->
            asp

        Nothing ->
            let
                ( w, h ) =
                    cDim
            in
            w / h


{-|

  - TextureLoaded : signalled by canvas when new texture is loaded after url was updated.
  - LoadTex : The url should be updated.
  - LoadCustomTex : The given file should become the new texture.
  - DownMsg : The mouse is pressed on the canvas.
  - MoveMsg : The mouse moves over the canvas.
  - UpMsg : The mouse is released on teh canvas.
  - ChangeRecursion : The recursion steps should be updated.
  - Reload : Application should reload.
  - ResetRenders : Application should throw away non-stamped progress.
  - Stamp : initialRenders should be updated to current renders, thus saving progress.

-}
type Msg
    = TextureLoaded (Maybe Texture)
    | LoadTex String
    | LoadCustomTex File
    | DownMsg ( Float, Float )
    | MoveMsg ( Float, Float )
    | UpMsg
    | ChangeRecursion Int
    | Reload
    | ResetRenders
    | Stamp
    | NextSelection
    | PrevSelection
    | ClearSelection ClearAmount


type ClearAmount
    = ClearOne
    | ClearAll


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- recursiveTextures : Texture -> Model -> List Renderable
-- recursiveTextures t model =
--     let (startX, startY) = model.startPos in
--     let rect = getRect model.cDim model.startPos model.curPos in
--     let s = (rectWidth rect) / (first model.cDim) in
--     let mkTrans : Int -> List Transform
--         mkTrans i = if i > 0
--                     then
--                       let i2 = toFloat (i-1) in
--                       (translate (s^i2 * startX) (s^i2 * startY)) :: mkTrans (i-1)
--                     else []
--     in
--     let sTotal = model.initialScale in
--     let _ = Debug.log "scale:" s in
--     -- since the scaling also affects the anchor position I set it to (0, 0) and then translate after the scaling.
--     -- to do multiple levels at one I would still need to compute a new anchor each time by using the scaling factor
--     List.map (\i -> texture [transform (mkTrans i ++ [scale (s^(toFloat i) * sTotal) (s^(toFloat i) * sTotal)])] (0, 0) t) (range model.recSteps)
--     -- let newtex2 = texture [transform [translate (s * startX) (s * startY), translate startX startY, scale (s * sTotal) (s * sTotal)]] (0, 0) t in
--     -- doesn't really work like this. I think it goes wrong because I already scale the original texture and I always need to apply this scaling first
--     -- let newtex2 = texture [transform [translate (startX * 0.5) (startY * 0.5), translate startX startY, scale 0.25 0.25]] (0, 0) t in
--     -- [ newtex, newtex2 ]
-- Take the renders from the model and place them into the selection rectangle again.
-- addRecursion : List Renderable -> Point -> Rect -> Int -> List Renderable
-- addRecursion initialRenders ( cWidth, cHeight ) rect recSteps =
--     let
--         aspectRect =
--             fitRectAspect (cWidth / cHeight) rect
--         s =
--             R.width aspectRect / cWidth
--         rec rs =
--             [ group [ transform [ translate aspectRect.x1 aspectRect.y1, scale s s ] ] rs ]
--         newRenders =
--             trunc_tail (iterCollect recSteps rec initialRenders)
--     in
--     List.concat newRenders


{-| Fill in recursive images in all active selections.
-}



-- addRecursions : Model -> Model
-- addRecursions ({ initialRenders, renders, cDim, selections, recSteps } as model) =
--     let
--         processSelection sel =
--             if sel.isActive then
--                 Just (addRecursion initialRenders cDim sel.rect recSteps)
--             else
--                 Nothing
--         newRenders =
--             List.concat (List.filterMap processSelection (Z.toList selections))
--     in
--     { model | renders = renders ++ newRenders }


addRecursion2 : Float -> Point -> List Rect -> List Renderable -> List Renderable
addRecursion2 width ( x1, y1 ) rects renders =
    let
        copy r =
            let
                s =
                    R.getWidth r / width

                _ =
                    Debug.log "(x1, y1) = " ( x1, y1 )

                _ =
                    Debug.log "r.(x1, y1) = " ( r.x1, r.y1 )
            in
            group [ transform [ translate (r.x1 - x1) (r.y1 - y1), scale s s ] ] renders

        newRenders =
            List.map copy rects
    in
    newRenders


{-| Fill in recursive images in all active selections.
This is done iteratively:

1.  in step 1 we copy the initialRenders to the selections
2.  in step n we copy the renders of n-1 to the selections

-}
addRecursions2 : Model -> Model
addRecursions2 ({ initialRender, renders, cDim, selections, recSteps } as model) =
    let
        ( cWidth, cHeight ) =
            cDim

        sourceRect =
            (Z.getFirst selections).rect

        sinkRects =
            List.filterMap
                (\sel ->
                    case sel.mode of
                        SelectionSink True ->
                            Just sel.rect

                        _ ->
                            Nothing
                )
                (Z.toList selections)

        sourceRender =
            -- this does not work since the aspect ratio is changed so after 2 recursion steps nothing lines up anymore
            -- group [ transform [ translate sourceRect.x1 sourceRect.y1, scale (R.getWidth sourceRect / cWidth) (R.getHeight sourceRect / cHeight) ] ] [ initialRender ]
            -- If I can get a cutout from initialRender then the scaling login in addRecursion2 should just work
            initialRender

        -- trunc_tail to throw away the initialRenders
        newRenders =
            trunc_tail (iterCollect recSteps (addRecursion2 (R.getWidth sourceRect) (R.getC R.UL sourceRect) sinkRects) [ sourceRender ])
    in
    { model | renders = renders ++ List.concat newRenders }


updateRenders : Model -> Model
updateRenders model =
    resetRenders model |> addRecursions2


{-| Lift a model to a mode, command pair.
-}
lm : Model -> ( Model, Cmd Msg )
lm m =
    ( m, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextureLoaded tex ->
            initRenders tex model |> lm

        DownMsg p ->
            { model | dragging = True, selections = Z.modify (selDown p) model.selections } |> lm

        -- fitToAspect here instead of in recursion + view
        MoveMsg p ->
            if model.dragging then
                { model | selections = Z.modify (selMove p) model.selections } |> lm

            else
                model |> lm

        UpMsg ->
            upSelection model |> lm

        LoadTex url ->
            let
                _ =
                    Debug.log "url:" url
            in
            { model | url = url } |> lm

        LoadCustomTex file ->
            let
                _ =
                    Debug.log "files" file
            in
            let
                cmdLoad =
                    Task.perform LoadTex (File.toUrl file)
            in
            ( model, cmdLoad )

        ChangeRecursion i ->
            let
                _ =
                    Debug.log "recursion:" i
            in
            { model | recSteps = i } |> updateRenders |> lm

        Reload ->
            let
                _ =
                    Debug.log "reload" ()
            in
            lm initialModel

        ResetRenders ->
            resetRenders model |> lm

        Stamp ->
            let
                selections =
                    Z.map R.deactivate model.selections
            in
            { model | initialRender = group [] model.renders, selections = selections } |> updateRenders |> lm

        PrevSelection ->
            let
                ( selections, _ ) =
                    move Z.Prev model.selections
            in
            { model | selections = selections } |> lm

        NextSelection ->
            let
                ( selections, _ ) =
                    move Z.Next model.selections
            in
            { model | selections = selections } |> lm

        ClearSelection ca ->
            let
                selections =
                    case ca of
                        ClearOne ->
                            Z.modify R.deactivate model.selections

                        ClearAll ->
                            Z.map R.deactivate model.selections

                -- reset the aspect ratio if all selections are cleared
                aspectM =
                    if List.all R.selIsCleared (Z.toList selections) then
                        let
                            _ =
                                Debug.log "all clear" ()
                        in
                        Nothing

                    else
                        model.aspectM
            in
            { model | selections = selections, aspectM = aspectM } |> updateRenders |> lm


{-| Initialize the renders of a model from a texture.
-}
initRenders : Maybe Texture -> Model -> Model
initRenders tex model =
    case tex of
        Nothing ->
            initialModel

        Just t ->
            let
                { width, height } =
                    T.dimensions t

                -- compute the scaling factor so that the new image fits into the canvas width
                s =
                    first initialModel.cDim / width

                newtex =
                    texture [ transform [ scale s s ] ] ( 0, 0 ) t

                cDim =
                    ( s * width, s * height )

                canvasRect =
                    mkRect ( 0, 0 ) cDim

                selections =
                    Z.modify (\sel -> { sel | rect = canvasRect, initialRect = canvasRect }) initialModel.selections
            in
            { initialModel | tex = tex, url = model.url, cDim = cDim, initialRender = newtex, renders = [ newtex ], selections = selections }


resetRenders : Model -> Model
resetRenders model =
    { model | renders = [ model.initialRender ] }


upSelection : Model -> Model
upSelection model =
    let
        -- we pass in aspectM so that the rectangle can be scaled in case we have an aspect
        selections =
            Z.modify (selUp model.aspectM) model.selections

        focusSel =
            Z.getFocus selections

        -- if aspectM was used in selUp then it was a Just,
        aspectM =
            case model.aspectM of
                Just _ ->
                    model.aspectM

                Nothing ->
                    if R.selIsActive focusSel then
                        let
                            _ =
                                Debug.log "set new aspect" ()
                        in
                        Just (R.getAspect focusSel.rect)

                    else
                        Nothing
    in
    { model | dragging = False, selections = selections, aspectM = aspectM } |> updateRenders


print : String -> Renderable
print str =
    Canvas.text [] ( 10, 10 ) str


renderSelections : Maybe Float -> Zipper Selection -> List Renderable
renderSelections aspectM selections =
    List.concatMap (R.renderSelection aspectM) (Z.toList selections)


view : Model -> Html Msg
view ({ tex, renders, cDim, selections, url, aspectM, recSteps } as model) =
    let
        contents =
            case tex of
                Nothing ->
                    [ print "Could not load texture." ]

                Just _ ->
                    renders
                        -- ++ [ print (toString startPos ++ toString curPos) ]
                        ++ renderSelections aspectM selections

        selectionsView =
            let
                renderDot n ( mode, sel ) =
                    let
                        pos =
                            ( 10 + 20 * toFloat n, 10 )

                        radius =
                            5
                    in
                    case mode of
                        Z.Other ->
                            [ shapes [ fill sel.color ] [ circle pos radius ] ]

                        Z.Focus ->
                            [ shapes [ fill sel.color ] [ circle pos radius ]
                            , shapes [ stroke Color.black ] [ circle pos (radius + 1) ]
                            ]

                dots =
                    List.concat (list_mapi renderDot (Z.toListMode model.selections))

                w =
                    20 * toFloat (Z.length model.selections)

                h =
                    20
            in
            Canvas.toHtml ( floor w, floor h ) [] (clearCanvas ( w, h ) Color.white :: dots)

        selectionMenu =
            div []
                [ selectionsView
                , button [ onClick PrevSelection ] [ text "Prev" ]

                -- , label [ style "background-color" (Color.toCssString selections.focus.color) ] [ text "Current" ]
                , button [ onClick NextSelection ] [ text "Next" ]
                ]
    in
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtmlWith
            { width = floor (first cDim)
            , height = floor (second cDim)
            , textures = [ T.loadFromImageUrl url TextureLoaded ]
            }
            [ Mouse.onDown (\event -> DownMsg event.offsetPos)
            , Mouse.onMove (\event -> MoveMsg event.offsetPos)
            , Mouse.onUp (\_ -> UpMsg)
            ]
            (clearCanvas cDim Color.lightGrey :: contents)
        , div []
            [ button [ onClick (LoadTex "./assets/image.jpg") ] [ text "Load example" ]
            , button [ onClick Stamp ] [ text "Stamp" ]

            -- , div []
            --     [ label [ for "chkFocus" ] [ text "Focus" ]
            --     , input
            --         [ type_ "checkbox"
            --         , id "chkFocus"
            --         , on "change" (D.map ChangeFocus checkboxDecoder)
            --         , checked model.focussed
            --         ]
            --         []
            --     ]
            , input
                [ type_ "file"
                , on "change" (D.map processFile filesDecoder)
                ]
                []
            , input
                [ type_ "number"
                , onInput processRecSteps

                -- somehow the on "input" does not work, no events are fired
                -- a.d. I think the decoder gets the whole even so I need something like checkboxDecoder
                --   , on "input" (D.map ChangeRecursion D.int)
                , value (toString recSteps)
                ]
                []
            , selectionMenu
            , button [ onClick (ClearSelection ClearOne) ] [ text "Clear" ]
            , button [ onClick (ClearSelection ClearAll) ] [ text "Clear All" ]
            ]
        ]


clearCanvas : Point -> Color -> Renderable
clearCanvas ( w, h ) color =
    shapes [ fill color ] [ rect ( 0, 0 ) w h ]


processFile : List File -> Msg
processFile l =
    case l of
        [ file ] ->
            LoadCustomTex file

        _ ->
            Reload


processRecSteps : String -> Msg
processRecSteps s =
    case String.toInt s of
        Nothing ->
            ChangeRecursion initialModel.recSteps

        Just i ->
            ChangeRecursion i


filesDecoder : D.Decoder (List File)
filesDecoder =
    D.at [ "target", "files" ] (D.list File.decoder)


checkboxDecoder : D.Decoder Bool
checkboxDecoder =
    D.at [ "target", "checked" ] D.bool



-- relativePos : Pointer.Event -> ( Float, Float )
-- relativePos event =
--     event.pointer.offsetPos
