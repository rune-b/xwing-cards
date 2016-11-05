module XWingData exposing (Card (Pilot, Upgrade), Cards, CardBase, getCards)

import Http
import Json.Decode as Decode exposing (..)
import Task exposing (..)

type alias Cards = List Card 
type alias Pilots = List PilotCard
type alias Upgrades = List UpgradeCard
type alias CardBase a = 
    { a | name: String, text: String, imageUrl: String }

type Card = Pilot PilotCard | Upgrade UpgradeCard

type alias PilotCard = 
    { name : String
    , text: String
    , imageUrl: String
    , ship: String 
    --, cardType: CardType
    }

type alias UpgradeCard = 
    { name : String
    , text: String
    , imageUrl: String
    , slot: String
    }


getCards: Task Http.Error Cards
getCards =
    let 
        pilotsTask = Task.map (\pilots -> List.map Pilot pilots) getPilots
        upgradesTask = Task.map (\pilots -> List.map Upgrade pilots) getUpgrades
    in 
        Task.map 
            (\tasks -> List.concat tasks) 
            (Task.sequence [pilotsTask, upgradesTask])

getPilots: Task Http.Error Pilots
getPilots = 
    Http.get (Decode.list pilotDecoder) "./xwing-data/data/pilots.js"

pilotDecoder : Decoder PilotCard
pilotDecoder =
    Decode.object4 PilotCard
        ("name" := Decode.string)
        (oneOf [ "text" := Decode.string, Decode.succeed "" ])
        (oneOf [ "image" := Decode.string, Decode.succeed "" ])
        ("ship" := Decode.string)


getUpgrades: Task Http.Error Upgrades
getUpgrades = 
    Http.get (Decode.list ugradeDecoder) "./xwing-data/data/upgrades.js"

ugradeDecoder : Decoder UpgradeCard
ugradeDecoder =
    Decode.object4 UpgradeCard
        ("name" := Decode.string)
        (oneOf [ "text" := Decode.string, Decode.succeed "" ])
        (oneOf [ "image" := Decode.string, Decode.succeed "" ])
        ("slot" := Decode.string)

