module Main exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode exposing (int, string, Decoder)
import Json.Decode.Pipeline exposing (decode, optional)
import MouseEvents exposing (onMouseMove, onMouseOver, onMouseOut, relPos, MouseEvent, Position)


{-| Because Flags can be optional, we want to update the signature to use Decode.Value vs. Flags so it can be decoded
-}
main : Program Decode.Value Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { photo : Photo
    , lastPos : MouseEvent
    , zoom : Zoom
    }


type alias Photo =
    { imgSrc : String
    , imgWidth : Int
    , imgHeight : Int
    , viewerWidth : Int
    , viewerHeight : Int
    }


flagDecoder : Decoder Photo
flagDecoder =
    decode Photo
        |> optional "imgSrc" string ""
        |> optional "imgWidth" int 1920
        |> optional "imgHeight" int 1200
        |> optional "viewerWidth" int 640
        |> optional "viewerHeight" int 400


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        decodedValue : Result String Photo
        decodedValue =
            Decode.decodeValue flagDecoder flags

        lastPos : MouseEvent
        lastPos =
            { clientPos = Position 0 0, targetPos = Position 0 0 }

        initialPhoto : Photo
        initialPhoto =
            Photo "" 1920 1200 640 400

        initialModel : Model
        initialModel =
            case decodedValue of
                Ok model ->
                    { photo = model, lastPos = lastPos, zoom = ZoomOut }

                Err _ ->
                    Model initialPhoto lastPos ZoomOut
    in
        ( initialModel, Cmd.none )



-- VIEW


createTransformOrigin : MouseEvent -> Photo -> String
createTransformOrigin evt photo =
    let
        percentage : Float
        percentage =
            photo.imgWidth // photo.viewerWidth * 100 |> toFloat

        offsetX : Float
        offsetX =
            evt.clientPos.x - evt.targetPos.x |> toFloat

        offsetY : Float
        offsetY =
            evt.clientPos.y - evt.targetPos.y |> toFloat

        x : String
        x =
            offsetX / toFloat photo.imgWidth * percentage |> toString

        y : String
        y =
            offsetY / toFloat photo.imgHeight * percentage |> toString
    in
        x ++ "% " ++ y ++ "%"


createBackgroundImage : Photo -> String
createBackgroundImage { imgSrc } =
    "url(" ++ imgSrc ++ ")"


createViewerProp : Int -> String
createViewerProp int =
    (toString int) ++ "px"


createBaseStyles : Photo -> MouseEvent -> List ( String, String )
createBaseStyles photo evt =
    [ ( "backgroundImage", createBackgroundImage photo )
    , ( "backgroundPosition", "center" )
    , ( "backgroundRepeat", "no-repeat" )
    , ( "backgroundSize", "cover" )
    , ( "height", "100%" )
    , ( "left", "0" )
    , ( "position", "absolute" )
    , ( "top", "0" )
    , ( "transformOrigin", createTransformOrigin evt photo )
    , ( "transition", "transform .3s ease-in-out" )
    , ( "width", "100%" )
    ]


transformScale : Photo -> String
transformScale { imgWidth, viewerWidth } =
    imgWidth // viewerWidth |> toString


createZoomStyles : Zoom -> Photo -> List ( String, String )
createZoomStyles zoom photo =
    case zoom of
        ZoomIn ->
            [ ( "cursor", "zoom-in" )
            , ( "transform", "scale(" ++ transformScale photo ++ ".0)" )
            ]

        ZoomOut ->
            [ ( "cursor", "pointer" )
            , ( "transform", "scale(1.0)" )
            ]


createPhoto : Model -> Html Msg
createPhoto { photo, lastPos, zoom } =
    let
        baseCss =
            createBaseStyles photo lastPos

        zoomCss =
            createZoomStyles zoom photo
    in
        div
            [ class "photo"
            , onMouseMove MouseMove
            , onMouseOver MouseOver
            , onMouseOut MouseOut
            , style (List.append baseCss zoomCss)
            ]
            []


view : Model -> Html Msg
view model =
    div
        [ class "photo-wrapper"
        , style
            [ ( "box-sizing", "border-box" )
            , ( "height", createViewerProp model.photo.viewerHeight )
            , ( "overflow", "hidden" )
            , ( "position", "relative" )
            , ( "width", createViewerProp model.photo.viewerWidth )
            ]
        ]
        [ createPhoto model ]



-- UPDATE


type Zoom
    = ZoomIn
    | ZoomOut


type Msg
    = MouseMove MouseEvent
    | MouseOver MouseEvent
    | MouseOut MouseEvent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove evt ->
            ( { model | lastPos = evt }, Cmd.none )

        MouseOver evt ->
            ( { model | zoom = ZoomIn }, Cmd.none )

        MouseOut evt ->
            ( { model | zoom = ZoomOut }, Cmd.none )
