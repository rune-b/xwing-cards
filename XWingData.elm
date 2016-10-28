module XWingData exposing (..)


import Json.Decode as Decode exposing (..)

type alias Cards = List Card

type alias Card  = 
    { name : String
    , text: String
    }

test = """
    [
        {"name": "bob", "text": "blablabla"},
        {"name": "aaa", "text": "xxx"}
    ]
  """


decoder : Decoder Card
decoder =
  Decode.object2 Card
    ("name" := Decode.string)
    ("text" := Decode.string)

cards : List Card
cards = 
    let
        result = decodeString (Decode.list decoder) test
        x = Debug.log "result: " result
    in 
        case result of
            Err msg ->
                []
            Ok cards ->  
                cards
    