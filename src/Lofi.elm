module Lofi exposing (parseElement)

{-| A library for #lofi parsing

# Parsing #lofi
@docs parseElement

-}

import Parser.Types exposing (Element)
import Parser.Model


{-| Turn a #lofi string into a #lofi Element.
-}
parseElement : String -> Element
parseElement = Parser.Model.parseElement
