module Filter.DateComponent.Calendar exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Translations exposing (Language, translate, translations)


type alias Model =
    { placeholder : String }


type Msg
    = NoOp


init : Model
init =
    Model ""


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Language -> model -> Html msg
view language _ =
    div [ class "calendar-view" ]
        [ text "Calendar view is not yet implemented." ]


encodedValue : Model -> List ( String, Decode.Value )
encodedValue _ =
    []
