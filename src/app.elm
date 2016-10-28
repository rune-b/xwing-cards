import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Dom exposing (focus)
import Task
import Array exposing (..)
import XWingData exposing (..)
import Http
import String

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
    , cards: XWingData.Cards
    , searchResults : XWingData.Cards
    }


init : (Model, Cmd Msg)
init = 
    ( Model 
        ""
        []
        []
    , getCardsCmd
    )


-- UPDATE

type Msg 
    = NoOp 
    | Search String
    | GetCardsSuccess Cards
    | GetCardsFailed Http.Error
    | Found Cards

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> 
            model ! []
        Search text ->
            ({ model | searchString = text }, searchCards text model.cards)
        Found cards ->
            let
                x = Debug.log "Found" cards
            in
                ({ model | searchResults = cards }, Cmd.none)            
        GetCardsSuccess cards ->
            ({ model | cards = cards }, Cmd.none)
        GetCardsFailed error ->
            let
                x = Debug.log "GetCardsFailed" error
            in
                (model, Cmd.none)

getCardsCmd: Cmd Msg
getCardsCmd =
    Task.perform GetCardsFailed GetCardsSuccess XWingData.getCards

searchCards : String -> Cards -> Cmd Msg
searchCards text cards =
    let
        found = List.filter (matchCard text) cards
    in
        Task.perform identity identity (Task.succeed (Found found))

matchCard : String -> Card -> Bool
matchCard text card =
    (String.contains (String.toLower text) (String.toLower card.name)) 
    || (String.contains (String.toLower text) (String.toLower card.text)) 

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ node "style" [type' "text/css"]
            [text "@import url(../css/styles.css)"]
        , header [] 
            [ h1 [] [text "X-Wing Card Finder"]
            ]
        , section [id "search"]
            [ input [value model.searchString, onInput Search, autofocus True] []
            ]
        , section [id "results"]
            (List.map viewCard model.searchResults)
        ]


-- SUBSCRIPTIONS

viewCard : Card -> Html Msg
viewCard card =
    p [] [text (card.name ++ ", " ++ card.text)]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none