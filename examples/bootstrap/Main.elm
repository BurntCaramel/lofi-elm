module LofiBootstrap4 exposing (main)

import Html exposing (Html, section, div, h1, h2, h3, text, button, textarea, pre)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onClick, onInput)
import Json.Encode
import Json.Helpers
import Dict
import Lofi exposing (Element(Element))
import Lofi.Parse


type alias Model =
  { items : List String
  }


model : Model
model =
  { items = [
      "Click me #button #primary"
    ]
  }


type Msg
    = ChangeItem Int String
    | AddItem
    | RemoveItem Int


replaceInList : Int -> a -> List a -> List a
replaceInList index replacement list =
  list |>
  List.indexedMap (\i original -> if i == index then replacement else original)

removeFromList : Int -> List a -> List a
removeFromList index list =
  list
  |> List.indexedMap (\i item -> if i == index then Nothing else Just item)
  |> List.filterMap identity

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeItem index newContent ->
            ( { model | items = replaceInList index newContent model.items }
            , Cmd.none
            )

        AddItem ->
            ( { model | items = List.append model.items [""] }
            , Cmd.none
            )
            
        RemoveItem index ->
            ( { model | items = removeFromList index model.items }
            , Cmd.none
            )


viewElementBootstrap4 : Element -> Html Msg
viewElementBootstrap4 element =
  case element of
    Element {texts, mentions, tags} ->
      let
        hasTag tag = Dict.member tag tags

        hasTagText tag valueToMatch =
          case Dict.get tag tags of
            Just tagValue ->
              case tagValue of
                Lofi.Content {texts, mentions} ->
                  texts == [valueToMatch]
                _ ->
                  False

            Nothing ->
              False
      in
        if hasTag "button" then
          button [ classList
            [ ("btn", True)
            , ("btn-primary", hasTag "primary" )
            ]
          ]
          (List.map text texts)
        else if hasTag "alert" then
          let
            variationClass = if hasTagText "variation" "success" then
              "alert-success"
            else if hasTagText "variation" "danger" then
              "alert-danger"
            else
              "alert-info"
          in
            div [ classList
              [ ("alert", True)
              , (variationClass, True)
              ]
            ]
            (List.map text texts)
        else if hasTag "primary" then
          h1 [] (List.map text texts)
        else if hasTag "secondary" then
          h2 [] (List.map text texts)
        else if hasTag "tertiary" then
          h3 [] (List.map text texts)
        else
          div [] (List.map text texts)

contentTextareaStyle : Html.Attribute msg
contentTextareaStyle =
  style
    [ ("width", "100%")
    , ("background", "none")
    , ("border", "none")
    ]

viewItem : Int -> String -> Html Msg
viewItem index content =
  let
    element = Lofi.Parse.parseElement content
  in
    div [ class "row" ]
      [ div [ class "col-12 col-sm-6" ]
          [ textarea
            [ onInput (ChangeItem index)
            , contentTextareaStyle
            ]
            [ text content ] ]
      , div [ class "col-12 col-sm-6" ]
          [ viewElementBootstrap4 element ]
      ]

viewLofiRaw : Lofi.Element -> Html Msg
viewLofiRaw element =
  pre []
    [ text (
      Json.Encode.object
        [ --("texts", Json.Encode.list (List.map (Json.Helpers.maybeEncode Json.Encode.string) element.texts))
        ]
      |> Json.Encode.encode 2
    )
    ]

view : Model -> Html Msg
view model =
    section [ class "container" ]
    [ div [] (List.indexedMap viewItem model.items)
    , button [ onClick AddItem ] [ text "+" ] 
    ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( model, Cmd.none )
        }
