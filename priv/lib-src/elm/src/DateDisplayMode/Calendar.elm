module DateDisplayMode.Calendar exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


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


view : model -> Html msg
view _ =
    div [ class "calendar-view" ]
        [ text "Calendar view is not yet implemented." ]
