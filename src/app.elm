import Html exposing (..)
import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Dom exposing (focus)
import Task
import Array exposing (..)
import XWingData exposing (..)
import Http
import String
import Json.Encode exposing (string)
import Filters exposing (..)

main =
   Html.App.program 
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
    , filters: Filters.Model
    }

initialModel : Model
initialModel =
    { searchString = ""
    , cards = []
    , searchResults = []
    , filters = Filters.initialModel
    }

init : (Model, Cmd Msg)
init = 
    (initialModel, getCardsCmd)


-- UPDATE

type Msg 
    = NoOp 
    | Search String
    | GetCardsSuccess Cards
    | GetCardsFailed Http.Error
    | Found Cards
    | FilterMsg Filters.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> 
            model ! []
        Search text ->
            ({ model | searchString = text }, searchCards text model.cards model.filters)
        Found cards ->
            ({ model | searchResults = sortAndFilterCards cards }, Cmd.none)            
        GetCardsSuccess cards ->
            ({ model | cards = cards }, Cmd.none)
        GetCardsFailed error ->
            let
                x = Debug.log "GetCardsFailed" error
            in
                (model, Cmd.none)
        FilterMsg filterMsg ->
            let
                (updatedFilters, filterCmd) = Filters.update filterMsg model.filters
            in
                ({ model | filters = updatedFilters}, searchCards model.searchString model.cards updatedFilters)

sortAndFilterCards: Cards -> Cards
sortAndFilterCards cards =    
    List.sortWith compareCard 
        (List.filter (\card -> not (String.isEmpty (getCommon card).imageUrl)) cards)

getCommon: Card -> { name: String, text: String, imageUrl: String}
getCommon card =
    case card of
        Pilot pilot ->
            { name = pilot.name, text = pilot.text, imageUrl = pilot.imageUrl }
        Upgrade upgrade ->
            { name = upgrade.name, text = upgrade.text, imageUrl = upgrade.imageUrl }

compareCard: Card -> Card -> Order
compareCard a b =
    compare (removeQuotes (getCommon a).name) (removeQuotes (getCommon b).name)


removeQuotes: String -> String
removeQuotes s =
    String.toLower (String.filter (\c -> c /= '"') s)

getCardsCmd: Cmd Msg
getCardsCmd =
    Task.perform GetCardsFailed GetCardsSuccess XWingData.getCards

searchCards : String -> Cards -> Filters.Model -> Cmd Msg
searchCards text cards filters =
    let
        found = List.filter (matchCard text filters) cards
    in
        Task.perform identity identity (Task.succeed (Found found))

matchCard : String -> Filters.Model -> Card -> Bool
matchCard text filters card =
    case card of 
        Pilot pilot ->
            filters.pilots && 
            ((String.contains (String.toLower text) (String.toLower pilot.name)) 
            || (String.contains (String.toLower text) (String.toLower pilot.text)))
        Upgrade upgrade ->
            filters.upgrades && 
            ((String.contains (String.toLower text) (String.toLower upgrade.name)) 
            || (String.contains (String.toLower text) (String.toLower upgrade.text)))

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ header [] 
            [ h1 [] [text "X-Wing Card Viewer"]
            , Html.App.map FilterMsg (Filters.view model.filters)
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