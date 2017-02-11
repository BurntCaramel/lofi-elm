module Lofi.Parse exposing
  ( parseElement
  )

{-| Parse #lofi strings into elements

# Run Parsers
@docs parseElement

-}


import Lofi exposing (Element(..), Text, Mention, KeyPath, TagValue(..), Tags)
import String
import Regex
import Dict
import List.Extra


tagsRegex : Regex.Regex
tagsRegex = Regex.regex "\\B#\\w+(:\\s*[^#]*)?"

mentionsRegex : Regex.Regex
mentionsRegex = Regex.regex "\\B@([a-zA-Z0-9-_]+(?:\\.[a-zA-Z0-9-_]+)*)"

reduceMultipleSpaces : String -> String
reduceMultipleSpaces = Regex.replace Regex.All (Regex.regex "\\s+") (\_ -> " ")

nonEmptyRegex : Regex.Regex
nonEmptyRegex = Regex.regex "\\S"

rejectEmptyStrings : List String -> List String
rejectEmptyStrings = List.filter (not << Regex.contains nonEmptyRegex)

-- parseKeyPathItem : String -> 
-- R.map(R.when(
--   R.test(/^\d/), // Starts with a digit
--   (input) => parseInt(input, 10) // Convert to number
-- ))

convertToKeyPath : String -> List String
convertToKeyPath input =
  input
  |> String.trim
  |> String.split "."
  |> rejectEmptyStrings

textsAndMentionSeriesReducer : List String -> (List Text, List Mention) -> (List Text, List Mention)
textsAndMentionSeriesReducer pair (texts, mentions) =
  let
    (inText, inMention) =
      case pair of
        [] -> ("", Maybe.Nothing)
        [text] -> (text, Maybe.Nothing)
        [text, mention] -> (text, Maybe.Just mention)
        text::mention::_ -> (text, Maybe.Just mention)
    
    outText =
      reduceMultipleSpaces inText
    
    outMention =
      Maybe.map convertToKeyPath inMention
  in
    -- @TODO: Use :: instead to prepend, then reverse
    (texts ++ [outText], mentions ++ [outMention])

reduceTextAndMentionSeries : List (List String) -> (List Text, List Mention)
reduceTextAndMentionSeries list =
  List.foldl textsAndMentionSeriesReducer ([], []) list

parseTextsAndMentions : String -> { texts : List Text, mentions: List Mention }
parseTextsAndMentions input =
  input
  |> Regex.split Regex.All mentionsRegex
  |> List.Extra.groupsOf 2
  |> reduceTextAndMentionSeries
  |> \(texts, mentions) -> { texts = texts, mentions = mentions }


parseTagValue : Maybe String -> TagValue
parseTagValue input =
  case input of
    Nothing ->
      Flag True
    
    Just string ->
      Content (parseTextsAndMentions string)

parseTagSubmatches : List (Maybe String) -> Maybe (String, TagValue)
parseTagSubmatches submatches =
  case submatches of
    key::_::value::_ ->
      Maybe.map (\key -> (key, parseTagValue value)) key
    
    _ ->
      Nothing

tagKeyValueRegex : Regex.Regex
tagKeyValueRegex = Regex.regex "\\B#([a-zA-Z0-9-_]+)(:\\s*([^#]*))?"

parseTag : String -> Maybe (String, TagValue)
parseTag input =
  input
  |> Regex.find Regex.All tagKeyValueRegex -- capture tag elements
  --|> \match::_ -> Maybe.map (\match -> parseTagSubmatches match.submatches) match
  |> \matches ->
    case matches of
      match::_ ->
        parseTagSubmatches match.submatches
    
      _ ->
        Nothing

tagKeyAndValueReducer : Maybe (String, TagValue) -> Tags -> Tags
tagKeyAndValueReducer maybeTagTuple tags =
  case maybeTagTuple of
    Nothing ->
      tags
    
    Just (key, value) ->
      Dict.insert key value tags

parseTags : String -> Tags
parseTags input =
  input
  |> Regex.find Regex.All tagsRegex
  |> List.map (\match -> parseTag match.match)
  |> List.foldl tagKeyAndValueReducer Dict.empty

{-| Parses a single #lofi line into an Element

    parseElement "Click me #button #primary"
-}
parseElement : String -> Element
parseElement input =
  let
    { texts, mentions } =
      input
      |> Regex.replace Regex.All tagsRegex (\_ -> "")
      |> String.trim
      |> parseTextsAndMentions
    
    tags =
      parseTags input
  in
    Element
    {
      texts = texts
    , mentions = mentions
    , tags = tags
    , items = []
    }
