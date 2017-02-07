module Parser.Types exposing (..)

--import Array exposing (Array)
import Dict exposing (Dict)


type TagValue
  = Flag Bool
  | Content { texts : Texts, mentions : Mentions }
type alias Tags = Dict String TagValue

type alias Text = String
type alias Texts = List Text

type alias KeyPath = List String
type alias Mention = Maybe KeyPath
type alias Mentions = List Mention

{-| A #lofi element, parsed from a single line
-}
type Element =
  Element
  { texts : Texts
  , mentions : Mentions
  , tags : Tags
  , items : List Element
  }
