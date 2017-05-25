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


tagsRegex : Regex.Regex
tagsRegex = Regex.regex "\\B#[A-Za-z0-9_-]+(:\\s*[^#]*)?"
tagKeyValueRegex : Regex.Regex
tagKeyValueRegex = Regex.regex "\\B#([a-zA-Z0-9-_]+)(:\\s*([^#]*))?"

mentionsRegex : Regex.Regex
mentionsRegex = Regex.regex "@([a-zA-Z0-9_-]+(?:\\.[a-zA-Z0-9-_]+)*)"

introductionsRegex : Regex.Regex
introductionsRegex = Regex.regex "^@([a-zA-Z0-9_-]+):[\\s]*(.*)"

reduceMultipleSpaces : String -> String
reduceMultipleSpaces = Regex.replace Regex.All (Regex.regex "\\s+") (\_ -> " ")

nonEmptyRegex : Regex.Regex
nonEmptyRegex = Regex.regex "\\S"

rejectEmptyStrings : List String -> List String
rejectEmptyStrings = List.filter (Regex.contains nonEmptyRegex)

-- parseKeyPathItem : String -> 
-- R.map(R.when(
--   R.test(/^\d/), // Starts with a digit
--   (input) => parseInt(input, 10) // Convert to number
-- ))

parseIntroduction : String -> { introduction: Maybe String, rest: String }
parseIntroduction input =
  input
  |> Regex.find Regex.All introductionsRegex
  |> \matches ->
    case matches of
      match::_ ->
        case match.submatches of
          introduction::rest::[] ->
            case rest of
              Just rest ->
                { introduction = introduction, rest = rest }
              
              Nothing ->
                { introduction = introduction, rest = "" }
          _ ->
            { introduction = Nothing, rest = input }
    
      _ ->
        { introduction = Nothing, rest = input }

convertToKeyPath : String -> List String
convertToKeyPath input =
  input
  |> String.trim
  |> String.split "."
  |> rejectEmptyStrings

textsAndMentionSeriesReducer : (Int, String) -> (List Text, List Mention) -> (List Text, List Mention)
textsAndMentionSeriesReducer (index, content) (texts, mentions) =
  let
    isMention = index % 2 == 1 -- Odd indexes are mentions
  in
    -- @TODO: Use :: instead to prepend, then reverse
    if isMention then
      ( texts
      , mentions ++ [convertToKeyPath content]
      )
    else
      let
        text = content
          |> reduceMultipleSpaces
      in
        if String.isEmpty text && index > 0 then
          ( texts, mentions)
        else
          ( texts ++ [text]
          , mentions
          )

extractTextAndMentionSeries : List String -> (List Text, List Mention)
extractTextAndMentionSeries list =
  list
  |> List.indexedMap (,)
  |> List.foldl textsAndMentionSeriesReducer ([], [])

parseTextsAndMentions : String -> { texts: List Text, mentions: List Mention }
parseTextsAndMentions input =
  input
  |> String.trim
  |> Regex.split Regex.All mentionsRegex
  |> extractTextAndMentionSeries
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

parseTag : String -> Maybe (String, TagValue)
parseTag input =
  input
  |> Regex.find Regex.All tagKeyValueRegex -- capture tag elements
  |> \matches ->
    case matches of
      match::_ ->
        match.submatches
        |> parseTagSubmatches 
    
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
    { introduction, rest } =
      input
      |> String.trim
      |> parseIntroduction

    { texts, mentions } =
      rest
      |> Regex.replace Regex.All tagsRegex (\_ -> "")
      |> parseTextsAndMentions
    
    tags =
      parseTags input
  in
    Element
    { introduction = introduction
    , texts = texts
    , mentions = mentions
    , tags = tags
    , items = []
    }
