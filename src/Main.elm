module Main exposing (..)

import Html exposing (Html, div, img)
import Html.Attributes exposing (class, src, height, width, alt)


-- import Html.Events exposing (on)
-- import Json.Decode as Decode
-- import Mouse exposing (Position)


main : Program Flags Model Msg
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


defaultTo : a -> Maybe a -> a
defaultTo defaultValue valueToTest =
    case valueToTest of
        Just val ->
            val

        Nothing ->
            defaultValue


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        imgUrl =
            defaultTo "" (Just flags.imgSrc)

        imgWidth =
            defaultTo 1920 (Just flags.imgWidth)

        imgHeight =
            defaultTo 1200 (Just flags.imgHeight)

        imgAlt =
            defaultTo "" (Just flags.imgAlt)

        viewerWidth =
            defaultTo 640 (Just flags.viewerWidth)

        viewerHeight =
            defaultTo 400 (Just flags.viewerHeight)

        zoomIncr =
            defaultTo 10 (Just flags.zoomIncr)

        maxZoom =
            defaultTo 3 (Just flags.maxZoom)

        initialModel =
            Model imgUrl imgWidth imgHeight imgAlt viewerWidth viewerHeight zoomIncr maxZoom
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
