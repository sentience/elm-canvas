module Main exposing (..)

import Canvas exposing (..)
import Color exposing (Color)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random exposing (Seed)
import Task
import Time exposing (Time)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Tick Time
    | Refresh


type alias Model =
    { seed : Maybe Seed
    , canvas : Canvas
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let
                initialSeed =
                    Random.initialSeed (round time)

                ( tiles, finalSeed ) =
                    drawTiles initialSeed
            in
            ( { model
                | seed = Just finalSeed
                , canvas =
                    initCanvas
                        |> Canvas.draw tiles
              }
            , Cmd.none
            )

        Refresh ->
            ( model, Task.perform Tick Time.now )


view : Model -> Html Msg
view model =
    viewArt model.canvas
        |> viewWithBackground


viewArt : Canvas -> Html Msg
viewArt canvas =
    Canvas.toHtml
        [ onClick Refresh
        , style
            [ ( "background-color", "#888" )
            , ( "border", "10px solid #fff" )
            , ( "box-shadow", "0 10px 20px rgba(0,0,0,0.5)" )
            , ( "cursor", "pointer" )
            , ( "padding", "40px" )
            ]
        ]
        canvas


viewWithBackground : Html msg -> Html msg
viewWithBackground content =
    div
        [ style
            [ ( "display", "flex" )
            , ( "align-items", "center" )
            , ( "justify-content", "center" )
            , ( "background-color", "#333" )
            , ( "height", "100vh" )
            ]
        ]
        [ content ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( { canvas = initCanvas
      , seed = Nothing
      }
    , Task.perform Tick Time.now
    )


initCanvas : Canvas
initCanvas =
    let
        size =
            canvasTiles * round tileSize
    in
    blankCanvas size


tileSize : Float
tileSize =
    20


canvasTiles : Int
canvasTiles =
    20


blankCanvas : Int -> Canvas
blankCanvas size =
    Canvas.initialize (Size size size)
        |> Canvas.draw
            (Canvas.batch
                [ BeginPath
                , FillStyle <| Color Color.white
                , Rect
                    (Point 0 0)
                    (Size size size)
                , Fill
                ]
            )


drawTiles : Seed -> ( DrawOp, Seed )
drawTiles seed =
    let
        rows =
            List.range 0 canvasTiles

        columns =
            List.range 0 canvasTiles

        ( tiles, newSeed ) =
            generate seed

        draw =
            List.indexedMap drawRow tiles
                |> List.concat
                |> Canvas.batch
    in
    ( draw, newSeed )


generate : Seed -> ( List (List Bool), Seed )
generate seed =
    let
        rowGenerator =
            Random.list canvasTiles Random.bool

        generator =
            Random.list canvasTiles rowGenerator
    in
    Random.step generator seed


drawRow : Int -> List Bool -> List DrawOp
drawRow index row =
    List.indexedMap (drawTile index) row


drawTile : Int -> Int -> Bool -> DrawOp
drawTile row col bool =
    if bool then
        drawVerticalTile row col
    else
        drawHorizontalTile row col


drawVerticalTile : Int -> Int -> DrawOp
drawVerticalTile row col =
    let
        offsetX =
            tileSize * (toFloat col + 0.5)

        offsetY =
            tileSize * toFloat row
    in
    [ BeginPath
    , LineWidth <| tileSize / 2
    , MoveTo <| Point offsetX offsetY
    , LineTo <| Point offsetX (offsetY + tileSize)
    , Stroke
    ]
        |> Canvas.batch


drawHorizontalTile : Int -> Int -> DrawOp
drawHorizontalTile row col =
    let
        offsetX =
            tileSize * toFloat col

        offsetY =
            tileSize * (toFloat row + 0.5)
    in
    [ BeginPath
    , LineWidth <| tileSize / 2
    , MoveTo <| Point offsetX offsetY
    , LineTo <| Point (offsetX + tileSize) offsetY
    , Stroke
    ]
        |> Canvas.batch
