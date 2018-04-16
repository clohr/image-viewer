module Main exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
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
        decodedValue =
            Decode.decodeValue flagDecoder flags

        lastPos =
            { clientPos = Position 0 0, targetPos = Position 0 0 }

        initialModel =
            case decodedValue of
                Ok model ->
                    { photo = model, lastPos = lastPos }

                Err _ ->
                    Model (Photo "" 1920 1200 "" 640 400) lastPos
    in
        ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = MouseMove MouseEvent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove ev ->
            ( { model | lastPos = ev }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "photo-wrapper" ]
        [ div
            [ class "photo", onMouseMove MouseMove ]
            []
        ]
