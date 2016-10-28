module XWingData exposing (Card, Cards, getCards)

import Http
import Json.Decode as Decode exposing (..)
import Task exposing (..)

type alias Cards = List Card

type alias Card  = 
    { name : String
    , text: String
    , imageUrl: String
    }

getCards: Task Http.Error Cards
getCards =
    Http.get (Decode.list cardDecoder) "../xwing-data/data/pilots.js"

cardDecoder : Decoder Card
cardDecoder =
  Decode.object3 Card
    ("name" := Decode.string)
    (oneOf [ "text" := Decode.string, Decode.succeed "" ])
    (oneOf [ "image" := Decode.string, Decode.succeed "" ])

