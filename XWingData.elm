module XWingData exposing (..)

import Http
import Json.Decode as Decode exposing (..)
import Task exposing (..)

type alias Cards = List Card

type alias Card  = 
    { name : String
    , text: String
    }

getCards: Task Http.Error Cards
getCards =
    Http.get decoder "xwing-data/data/pilots.js"

decoder = Decode.list cardDecoder

cardDecoder : Decoder Card
cardDecoder =
  Decode.object2 Card
    ("name" := Decode.string)
    (oneOf [ "text" := Decode.string, Decode.succeed "" ] )

