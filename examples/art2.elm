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
    = Seed Float
    | Refresh


type alias Model =
    { canvas : Canvas
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Seed seed ->
            let
                initialSeed =
                    Random.initialSeed (round seed)

                ( tiles, finalSeed ) =
                    drawTiles initialSeed
            in
            ( { model
                | canvas = Canvas.draw tiles initCanvas
              }
            , Cmd.none
            )

        Refresh ->
            ( model, Task.perform Seed Time.now )


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
      }
    , Task.perform Seed Time.now
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
        ( tiles, newSeed ) =
            generate seed

        draw =
            List.indexedMap drawRow tiles
                |> List.concat
                |> Canvas.batch
    in
    ( draw, newSeed )


type Tile
    = Straight Int
    | Swap Int Int


generate : Seed -> ( List (List Tile), Seed )
generate seed =
    let
        -- a non-rendered row that establishes the initial color positions
        setupRow =
            List.range 0 (canvasTiles - 1)
                |> List.map Straight

        recurse rowsLeft generatedRows seed =
            if rowsLeft == 0 then
                ( List.reverse generatedRows, seed )
            else
                let
                    ( row, nextSeed ) =
                        case generatedRows of
                            [] ->
                                generateRow setupRow seed

                            previousRow :: _ ->
                                generateRow previousRow seed
                in
                recurse (rowsLeft - 1) (row :: generatedRows) nextSeed
    in
    recurse canvasTiles [] seed


generateRow : List Tile -> Seed -> ( List Tile, Seed )
generateRow previousRow seed =
    let
        colorIndexes accumulator tiles =
            case tiles of
                [] ->
                    accumulator

                (Straight index) :: moreTiles ->
                    colorIndexes (index :: accumulator) moreTiles

                (Swap index1 index2) :: moreTiles ->
                    colorIndexes (index1 :: index2 :: accumulator) moreTiles

        recurse colorIndexes generatedTiles seed =
            case colorIndexes of
                [] ->
                    ( generatedTiles, seed )

                index :: [] ->
                    ( Straight index :: generatedTiles, seed )

                index1 :: index2 :: moreIndexes ->
                    case Random.step Random.bool seed of
                        ( True, nextSeed ) ->
                            -- swap
                            recurse moreIndexes (Swap index2 index1 :: generatedTiles) nextSeed

                        ( False, nextSeed ) ->
                            -- straight
                            recurse (index2 :: moreIndexes) (Straight index1 :: generatedTiles) nextSeed
    in
    recurse (colorIndexes [] previousRow) [] seed


drawRow : Int -> List Tile -> List DrawOp
drawRow rowIndex row =
    let
        recurse tilesLeft tilesDrawn columnIndex =
            case tilesLeft of
                [] ->
                    tilesDrawn

                tile :: moreTiles ->
                    let
                        ( drawnTile, columnSpan ) =
                            drawTile rowIndex columnIndex tile
                    in
                    recurse moreTiles (drawnTile :: tilesDrawn) (columnIndex + columnSpan)
    in
    recurse row [] 0


drawTile : Int -> Int -> Tile -> ( DrawOp, Int )
drawTile row col tile =
    case tile of
        Straight colorIndex ->
            ( drawStraight row col colorIndex, 1 )

        Swap colorIndex1 colorIndex2 ->
            ( drawSwap row col colorIndex1 colorIndex2, 2 )


drawStraight : Int -> Int -> Int -> DrawOp
drawStraight row col colorIndex =
    let
        offsetX =
            tileSize * (toFloat col + 0.5)

        offsetY =
            tileSize * toFloat row
    in
    [ BeginPath
    , StrokeStyle <| Color (colorForIndex colorIndex)
    , LineWidth <| tileSize / 2
    , MoveTo <| Point offsetX offsetY
    , LineTo <| Point offsetX (offsetY + tileSize)
    , Stroke
    ]
        |> Canvas.batch


drawSwap : Int -> Int -> Int -> Int -> DrawOp
drawSwap row col colorIndex1 colorIndex2 =
    let
        offsetX1 =
            tileSize * (toFloat col + 0.5)

        offsetX2 =
            offsetX1 + tileSize

        offsetY1 =
            tileSize * toFloat row

        offsetY2 =
            offsetY1 + tileSize
    in
    [ LineWidth <| tileSize / 2
    , BeginPath
    , StrokeStyle <| Color (colorForIndex colorIndex1)
    , MoveTo <| Point offsetX1 offsetY1
    , BezierCurveTo
        (Point offsetX1 ((offsetY1 + offsetY2) / 2))
        (Point offsetX2 ((offsetY1 + offsetY2) / 2))
        (Point offsetX2 offsetY2)
    , Stroke
    , BeginPath
    , StrokeStyle <| Color (colorForIndex colorIndex2)
    , MoveTo <| Point offsetX2 offsetY1
    , BezierCurveTo
        (Point offsetX2 ((offsetY1 + offsetY2) / 2))
        (Point offsetX1 ((offsetY1 + offsetY2) / 2))
        (Point offsetX1 offsetY2)
    , Stroke
    ]
        |> Canvas.batch


colorForIndex : Int -> Color
colorForIndex index =
    Color.hsl (degrees <| toFloat index * 36) 0.8 0.6
