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
import Json.Encode exposing (string)

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
            ({ model | searchResults = cards }, Cmd.none)            
        GetCardsSuccess cards ->
            ({ model | cards = cards, searchResults = cards }, Cmd.none)
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
    case card of 
        Pilot pilot ->
            (String.contains (String.toLower text) (String.toLower pilot.name)) 
            || (String.contains (String.toLower text) (String.toLower pilot.text)) 
        Upgrade upgrade ->
            (String.contains (String.toLower text) (String.toLower upgrade.name)) 
            || (String.contains (String.toLower text) (String.toLower upgrade.text)) 

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ header [] 
            [ h1 [] [text "X-Wing Card Viewer"]
            ]
        , section [id "search"]
            [ input [value model.searchString, onInput Search, autofocus True] []
            ]
        , section [id "results"]
            (List.map viewCard model.searchResults)
        ]

viewCard : Card -> Html Msg
viewCard card  =
    case card of
        Pilot pilot ->
            if String.isEmpty pilot.imageUrl 
                then cardAsText "pilot" pilot
                else cardAsImage "pilot" pilot            
        Upgrade upgrade -> 
            if String.isEmpty upgrade.imageUrl 
                then cardAsText "upgrade" upgrade
                else cardAsImage "upgrade" upgrade

cardAsImage : String -> CardBase c -> Html Msg
cardAsImage cardType card =
    div [class ("card image " ++ cardType)] 
        [ img [src ("./xwing-data/images/" ++ card.imageUrl), alt card.name, title card.name] []        
        ]

cardAsText: String -> CardBase c -> Html Msg
cardAsText cardType card =
    div [class ("card text " ++ cardType)] 
        [ h1 [] [text card.name]
        , div [class "text", property "innerHTML" (string card.text)] []
        ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none