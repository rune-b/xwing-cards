module Filters exposing (view, Msg, Model, initialModel, update)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type Msg 
    = NoOp
    | Pilots 
    | Upgrades

type alias Model = 
    { pilots: Bool
    , upgrades: Bool
    }

initialModel : Model
initialModel =
    { pilots = False
    , upgrades = False
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            model ! []
        Pilots ->
            { model | pilots = not model.pilots} ! []
        Upgrades ->
            { model | upgrades = not model.upgrades} ! []

view : Model -> Html Msg
view model =
    div [class "filters"] 
        [ filterCheckbox "Pilots" model.pilots Pilots
        , filterCheckbox "Upgrades" model.upgrades Upgrades
        ]

filterCheckbox : String -> Bool -> Msg -> Html Msg
filterCheckbox caption isChecked msg =
    label [] 
        [ input [type' "checkbox", value "", checked isChecked, onClick msg] []
        , text caption
        ]
    