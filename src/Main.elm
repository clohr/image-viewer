module Main exposing (..)

import Html exposing (Html, div, img)
import Html.Attributes exposing (class, src, height, width, alt)


-- import Html.Events exposing (on)
-- import Mouse exposing (Position)

import Json.Decode as Decode exposing (int, string, Decoder)
import Json.Decode.Pipeline exposing (decode, optional)


{-| Because Flags can be optional, we want to update the signature to use Decode.Value vs. Flags so it can be decoded
-}
main : Program Decode.Value Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }



-- MODEL


type alias Model =
    { imgSrc : String
    , imgWidth : Int
    , imgHeight : Int
    , imgAlt : String
    , viewerWidth : Int
    , viewerHeight : Int
    , zoomIncr : Int
    , maxZoom : Int
    }


type alias Flags =
    Model


flagDecoder : Decoder Model
flagDecoder =
    decode Model
        |> optional "imgSrc" string ""
        |> optional "imgWidth" int 1920
        |> optional "imgHeight" int 1200
        |> optional "imgAlt" string ""
        |> optional "viewerWidth" int 640
        |> optional "viewerHeight" int 400
        |> optional "zoomInc" int 10
        |> optional "maxZoom" int 3


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        decodedValue =
            Decode.decodeValue flagDecoder flags

        initialModel =
            case decodedValue of
                Ok model ->
                    model

                Err _ ->
                    Model "" 1920 1200 "" 640 400 10 3
    in
        ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
        [ img
            [ src model.imgSrc
            , height model.imgHeight
            , width model.imgWidth
            , alt model.imgAlt
            ]
            []
        ]
