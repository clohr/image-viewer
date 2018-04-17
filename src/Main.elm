module Main exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode exposing (int, string, Decoder)
import Json.Decode.Pipeline exposing (decode, optional)
import MouseEvents exposing (onMouseMove, relPos, MouseEvent, Position)


{-| Because Flags can be optional, we want to update the signature to use Decode.Value vs. Flags so it can be decoded
-}
main : Program Decode.Value Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { photo : Photo
    , lastPos : MouseEvent
    }


type alias Flags =
    Photo


type alias Photo =
    { imgSrc : String
    , imgWidth : Int
    , imgHeight : Int
    , imgAlt : String
    , viewerWidth : Int
    , viewerHeight : Int
    }


flagDecoder : Decoder Photo
flagDecoder =
    decode Photo
        |> optional "imgSrc" string ""
        |> optional "imgWidth" int 1920
        |> optional "imgHeight" int 1200
        |> optional "imgAlt" string ""
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

        initialModel : Model
        initialModel =
            case decodedValue of
                Ok model ->
                    { photo = model, lastPos = lastPos }

                Err _ ->
                    Model (Photo "" 1920 1200 "" 640 400) lastPos
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
createBackgroundImage photo =
    "url(" ++ photo.imgSrc ++ ")"


createViewerProp : Int -> String
createViewerProp int =
    (toString int) ++ "px"


view : Model -> Html Msg
view model =
    div
        [ class "photo-wrapper"
        , style
            [ ( "width", createViewerProp model.photo.viewerWidth )
            , ( "height", createViewerProp model.photo.viewerHeight )
            ]
        ]
        [ div
            [ class "photo"
            , onMouseMove MouseMove
            , style
                [ ( "transformOrigin", createTransformOrigin model.lastPos model.photo )
                , ( "backgroundImage", createBackgroundImage model.photo )
                ]
            ]
            []
        ]



-- UPDATE


type Msg
    = MouseMove MouseEvent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove evt ->
            ( { model | lastPos = evt }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
