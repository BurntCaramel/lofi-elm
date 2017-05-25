module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Dict
import Lofi exposing (Element(..), TagValue(..))
import Lofi.Parse exposing (parseElement)


defaultElement = { introduction = Nothing, texts = [], mentions = [], tags = Dict.empty, items = [] }

all : Test
all =
    describe "Parsing"
        [ describe "Parsing text"
            [ test "Text" <|
                \() ->
                    Expect.equal (parseElement "hello") (Element { defaultElement | texts = ["hello"] })
            , test "Text with leading whitespace" <|
                \() ->
                    Expect.equal (parseElement " hello") (Element { defaultElement | texts = ["hello"] })
            , test "Text with trailing whitespace" <|
                \() ->
                    Expect.equal (parseElement "hello ") (Element { defaultElement | texts = ["hello"] })
            , test "Text with surrounding whitespace" <|
                \() ->
                    Expect.equal (parseElement " hello ") (Element { defaultElement | texts = ["hello"] })
            ]
        , describe "Text with tags"
            [ test "Simple flag tag" <|
                \() ->
                    Expect.equal (parseElement "hello #button") (Element { defaultElement | texts = ["hello"], tags = Dict.singleton "button" (Flag True) })
            , test "Key value tag" <|
                \() ->
                    let
                        tags = Dict.singleton "variation" (Content { texts = ["danger"], mentions = []})
                    in
                        Expect.equal (parseElement "hello #variation: danger") (Element { defaultElement | texts = ["hello"], tags = tags })
            , test "Two tags" <|
                \() ->
                    let
                        tags = Dict.fromList [ ("button", Flag True), ("variation", Content { texts = ["danger"], mentions = []}) ]
                    in
                        Expect.equal (parseElement "hello #button #variation: danger") (Element { defaultElement | texts = ["hello"], tags = tags })
            , test "Flag then content tags with trailing whitespace" <|
                \() ->
                    let
                        tags = Dict.fromList [ ("button", Flag True), ("variation", Content { texts = ["danger"], mentions = []}) ]
                    in
                        Expect.equal (parseElement "hello #button #variation: danger ") (Element { defaultElement | texts = ["hello"], tags = tags })
            , test "Content then flag tag with trailing whitespace" <|
                \() ->
                    let
                        tags = Dict.fromList [ ("button", Flag True), ("variation", Content { texts = ["danger"], mentions = []}) ]
                    in
                        Expect.equal (parseElement "hello #variation: danger #button ") (Element { defaultElement | texts = ["hello"], tags = tags })
            ]
        , describe "Text with mentions"
            [ test "Single just mention" <|
                \() ->
                    Expect.equal (parseElement "@first-name") (Element { defaultElement | texts = [""], mentions = [["first-name"]] })
            , test "Single mention" <|
                \() ->
                    Expect.equal (parseElement "hello @first-name") (Element { defaultElement | texts = ["hello "], mentions = [["first-name"]] })
            , test "Two mentions" <|
                \() ->
                    Expect.equal (parseElement "hello @first-name @last-name") (Element { defaultElement | texts = ["hello ", " "], mentions = [["first-name"], ["last-name"]] })
            , test "Two mentions with no space between" <|
                \() ->
                    Expect.equal (parseElement "hello @first-name@last-name") (Element { defaultElement | texts = ["hello "], mentions = [["first-name"], ["last-name"]] })
            , test "Two mentions with interleaved text" <|
                \() ->
                    Expect.equal (parseElement "first: @first-name last: @last-name") (Element { defaultElement | texts = ["first: ", " last: "], mentions = [["first-name"], ["last-name"]] })
            , test "Two mentions with interleaved text and suffix" <|
                \() ->
                    Expect.equal (parseElement "first: @first-name last: @last-name suffix") (Element { defaultElement | texts = ["first: ", " last: ", " suffix"], mentions = [["first-name"], ["last-name"]] })
            , test "Two mentions with interleaved text and full stop" <|
                \() ->
                    Expect.equal (parseElement "first: @first-name last: @last-name.") (Element { defaultElement | texts = ["first: ", " last: ", "."], mentions = [["first-name"], ["last-name"]] })
            ]
        , describe "Text with mentions key path"
            [ test "Single mention (key path)" <|
                \() ->
                    Expect.equal (parseElement "hello @person.name") (Element { defaultElement | texts = ["hello "], mentions = [["person", "name"]] })
            , test "Single mention (key path) with trailing period" <|
                \() ->
                    Expect.equal (parseElement "hello @person.name.") (Element { defaultElement | texts = ["hello ", "."], mentions = [["person", "name"]] })
            , test "Two mentions (key path)" <|
                \() ->
                    Expect.equal (parseElement "hello @person.name @person.last") (Element { defaultElement | texts = ["hello ", " "], mentions = [["person", "name"], ["person", "last"]] })
            ]
        , describe "Text with tags and mentions"
            [ test "Single mention (key path) and tag" <|
                \() ->
                    Expect.equal (parseElement "hello @person.name #button") (Element { defaultElement | texts = ["hello "], mentions = [["person", "name"]], tags = Dict.singleton "button" (Flag True) })
            , test "Single mention (key path) and tag" <|
                \() ->
                    Expect.equal (parseElement "hello @person.name @person.last #button") (Element { defaultElement | texts = ["hello ", " "], mentions = [["person", "name"], ["person", "last"]], tags = Dict.singleton "button" (Flag True) })
            , test "Two mentions (key path) and tag (value)" <|
                \() ->
                    Expect.equal (parseElement "hello @person.name @person.last #key: value") (Element { defaultElement | texts = ["hello ", " "], mentions = [["person", "name"], ["person", "last"]], tags = Dict.singleton "key" (Content { texts = ["value"], mentions = [] }) })
            ]
        , describe "Tag value with mentions"
            [ test "Single mention (key path)" <|
                \() ->
                    Expect.equal (parseElement "#table #title: @person.name") (Element { defaultElement | texts = [""], mentions = [], tags = Dict.fromList [ ("table", Flag True), ("title", Content { texts = [""], mentions = [["person", "name"]] }) ] })
            , test "Two mentions (key path)" <|
                \() ->
                    Expect.equal (parseElement "#table #title: @person.name @person.last") (Element { defaultElement | texts = [""], mentions = [], tags = Dict.fromList [ ("table", Flag True), ("title", Content { texts = ["", " "], mentions = [["person", "name"], ["person", "last"]] }) ] })
            , test "Two mentions (key path) with surrounding text" <|
                \() ->
                    Expect.equal (parseElement "#table #title: a @person.name b @person.last") (Element { defaultElement | texts = [""], mentions = [], tags = Dict.fromList [ ("table", Flag True), ("title", Content { texts = ["a ", " b "], mentions = [["person", "name"], ["person", "last"]] }) ] })
            , test "Single mention (key path) with surrounding whitespace" <|
                \() ->
                    Expect.equal (parseElement " #table #title: @person.name ") (Element { defaultElement | texts = [""], mentions = [], tags = Dict.fromList [ ("table", Flag True), ("title", Content { texts = [""], mentions = [["person", "name"]] }) ] })
            ]
        , describe "Introduction"
            [ test "Text" <|
                \() ->
                    Expect.equal (parseElement "@user: hello") (Element { defaultElement | introduction = Just "user", texts = ["hello"] })
            , test "Empty text" <|
                \() ->
                    Expect.equal (parseElement "@user:") (Element { defaultElement | introduction = Just "user", texts = [""] })
            , test "Space" <|
                \() ->
                    Expect.equal (parseElement "@user: ") (Element { defaultElement | introduction = Just "user", texts = [""] })
            , test "Tag" <|
                \() ->
                    Expect.equal (parseElement "@user: #text") (Element { defaultElement | introduction = Just "user", texts = [""], tags = Dict.fromList [ ("text", Flag True) ] })
            , test "Mention (key path)" <|
                \() ->
                    Expect.equal (parseElement "@user: @person.name") (Element { defaultElement | introduction = Just "user", texts = [""], mentions = [["person", "name"]] })
            , test "Tag (value)" <|
                \() ->
                    Expect.equal (parseElement "@user: hello #key: value") (Element { defaultElement | introduction = Just "user", texts = ["hello"], tags = Dict.fromList [ ("key", Content { texts = ["value"], mentions = [] }) ] })
            , test "Tag (value) with surrounding whitespace" <|
                \() ->
                    Expect.equal (parseElement " @user: hello #key: value ") (Element { defaultElement | introduction = Just "user", texts = ["hello"], tags = Dict.fromList [ ("key", Content { texts = ["value"], mentions = [] }) ] })
            ]
        ]
