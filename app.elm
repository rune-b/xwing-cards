import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Dom exposing (focus)
import Task
import Array exposing (..)
import XWingData exposing (..)

main =
    App.program 
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions 
    }


-- MODEL

type alias Model = 
    { searchString : String
    }


init : (Model, Cmd Msg)
init = 
    ( Model 
        "test"
    ! []
    )


-- UPDATE

type Msg = NoOp | Search String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> 
            model ! []
        Search text ->
            { model | searchString = text } ! []

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ node "style" [type' "text/css"]
            [text "@import url(css/styles.css)"]
        , header [] 
            [ h1 [] [text "X-Wing Card Finder"]
            ]
        , section [id "search"]
            [ input [value model.searchString, onInput Search, autofocus True] []
            ]
        , section [id "results"]
            (List.map viewCard cards)
        ]
x = Debug.log "cards" cards
-- SUBSCRIPTIONS

viewCard : Card -> Html Msg
viewCard card =
    p [] [text (card.name ++ ", " ++ card.text)]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none