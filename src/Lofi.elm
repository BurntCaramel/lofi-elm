module Lofi exposing
  ( Element(..)
  , Text, Mention
  , KeyPath, Tags, TagValue(..)
  )

{-| A library for #lofi parsing

# Types
@docs Element, Text, Mention, KeyPath, Tags, TagValue

-}


import Dict exposing (Dict)


{-| Normal text to be displayed as-is -}
type alias Text = String

{-| A list of keys used in @mentions -}
type alias KeyPath = List String
{-| A reference to an external resource -}
type alias Mention = Maybe KeyPath

{-| The value of a tag, either a boolean, or nested content -}
type TagValue
  = Flag Bool
  | Content { texts : List Text, mentions : List Mention }
{-| A collection of tags -}
type alias Tags = Dict String TagValue

{-| A #lofi element, representing a single line -}
type Element =
  Element
  { texts : List Text
  , mentions : List Mention
  , tags : Tags
  , items : List Element
  }
