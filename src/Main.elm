module Main exposing (main)

import Browser
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Texture exposing (..)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse as Mouse
import Debug exposing (toString)
import Canvas.Settings.Line exposing (lineWidth)

type alias Model =
    { tex : Maybe Texture
    , textures : List Renderable
    , startPos : (Float, Float)
    , curPos : (Float, Float)
    , dragging : Bool }

initialModel : Model
initialModel = { tex = Nothing
               , textures = [ ]
               , startPos = (0.0, 0.0)
               , curPos = (0.0, 0.0)
               , dragging = False }


type Msg
    = TextureLoaded (Maybe Texture)
    | DownMsg (Float, Float)
    | MoveMsg (Float, Float)
    | UpMsg


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

update : Msg -> Model -> Model
update msg model =
    case msg of
        TextureLoaded tex -> 
            case tex of 
                Nothing -> { model | tex = tex }
                Just t ->
                    let sX = toFloat width / (dimensions t).width in
                    let sY = toFloat height / (dimensions t).height in
                    let newtex = texture [transform [scale sX sY]] (0, 0) t in
                    { model | tex = tex, textures = [ newtex ] }
        DownMsg p -> { model | startPos = p, dragging = True }
        MoveMsg p -> { model | curPos = p }
        UpMsg -> 
            case model.tex of
                Nothing -> { model | dragging = False }
                Just t -> 
                    let (startX, startY) = model.startPos in
                    let (curX, curY) = model.curPos in
                    let sX = (abs (startX - curX)) / (dimensions t).width in
                    let sY = (abs (startY - curY)) / (dimensions t).height in

                    -- since the scaling also affects the anchor position I set it to (0, 0) and then translate after the scaling.
                    -- to do multiple levels at one I would still need to compute a new anchor each time by using the scaling factor
                    let newtex = texture [transform [translate startX startY, scale sX sY]] (0, 0) t in
                    { model | dragging = False, textures = model.textures ++ [ newtex ] }

width =
    400


height =
    400


centerX =
    width / 2


centerY =
    height / 2


view : Model -> Html Msg
view { tex, textures, startPos, curPos, dragging } =
    let contents = case tex of
                    Nothing -> [ text [] (centerX, centerY) "Could not load texture." ]
                    Just t -> 
                        let (startX, startY) = startPos in
                        let (curX, curY) = curPos in
                        let edge1 = (startX, curY) in
                        let edge2 = (curX, startY) in
                        let sel = if dragging
                                    then [ shapes [stroke Color.red] [ path startPos [ lineTo edge1, lineTo curPos, lineTo edge2, lineTo startPos ] ] ]
                                    else []  
                        in
                        textures ++  
                        [ text [] (centerX, centerY) (toString startPos ++ toString curPos) ]
                        ++ sel
    in
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtmlWith
            { width = width
            , height = height
            , textures = [ loadFromImageUrl "../assets/image.jpg" TextureLoaded ]
            }
            [ Mouse.onDown (\event -> DownMsg event.offsetPos)
            , Mouse.onMove (\event -> MoveMsg event.offsetPos)
            , Mouse.onUp (\_ -> UpMsg) ]
            (clearScreen :: contents)
        ]


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.grey ] [ rect ( 0, 0 ) width height ]


-- relativePos : Pointer.Event -> ( Float, Float )
-- relativePos event =
--     event.pointer.offsetPos
