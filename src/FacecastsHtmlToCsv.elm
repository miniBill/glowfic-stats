module FacecastsHtmlToCsv exposing (run)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Stream as Stream
import Csv.Encode as Encode exposing (Encoder)
import FatalError exposing (FatalError)
import Html.Parser
import List.Extra
import Maybe.Extra
import Pages.Script as Script exposing (Script)
import Parser exposing ((|.), (|=), Parser)
import Parser.Error
import Parser.Workaround


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Do.allowFatal (Stream.stdin |> Stream.read) <| \{ body } ->
    Do.do
        (removeScriptTags body
            |> Result.mapError FatalError.fromString
            |> BackendTask.fromResult
        )
    <| \cleanHtml ->
    Do.do
        (case Html.Parser.runDocument cleanHtml of
            Ok tree ->
                BackendTask.succeed tree

            Err deadEnds ->
                { title = "Could not parse page"
                , body =
                    Parser.Error.renderError
                        { text = identity
                        , formatContext = Ansi.Color.fontColor Ansi.Color.cyan
                        , formatCaret = Ansi.Color.fontColor Ansi.Color.red
                        , newline = "\n"
                        , linesOfExtraContext = 3
                        }
                        Parser.Error.forParser
                        cleanHtml
                        deadEnds
                        |> String.concat
                }
                    |> FatalError.build
                    |> BackendTask.fail
        )
    <| \{ document } ->
    document
        |> Tuple.second
        |> seek (attributeIs "id" "content")
        |> seek (tag "table")
        |> seek (tag "tbody")
        |> seek (tag "tr")
        |> List.concatMap
            (\tr ->
                let
                    tds : List Html.Parser.Node
                    tds =
                        [ tr ]
                            |> seek (tag "td")
                in
                List.map3 (\rawActors link author -> ( rawActors, link, author ))
                    (tds |> nth 0 |> seek text)
                    (tds |> nth 2 |> seek (tag "a") |> seek text)
                    (tds |> nth 3 |> seek (tag "a") |> seek text)
                    |> List.concatMap
                        (\( rawActors, link, author ) ->
                            rawActors
                                |> String.replace "  " " "
                                |> String.split ","
                                |> List.map String.trim
                                |> List.map (\actor -> ( actor, link, author ))
                        )
            )
        |> Encode.encode { encoder = encoder, fieldSeparator = ',' }
        |> Script.log


encoder : Encoder ( String, String, String )
encoder =
    Encode.withoutFieldNames (\( a, b, c ) -> [ a, b, c ])


nth : Int -> List a -> List a
nth i n =
    case List.Extra.getAt i n of
        Just v ->
            [ v ]

        Nothing ->
            []


text : Html.Parser.Node -> List String
text n =
    case n of
        Html.Parser.Text t ->
            [ t ]

        _ ->
            []


tag : String -> Html.Parser.Node -> List Html.Parser.Node
tag name_ n =
    node
        (\name _ _ ->
            if name == name_ then
                [ n ]

            else
                []
        )
        n


attributeIs : String -> String -> Html.Parser.Node -> List Html.Parser.Node
attributeIs key value n =
    node
        (\_ attrs _ ->
            if List.member ( key, value ) attrs then
                [ n ]

            else
                []
        )
        n


node : (String -> List ( String, String ) -> List Html.Parser.Node -> List a) -> Html.Parser.Node -> List a
node selector n =
    case n of
        Html.Parser.Element name attrs children ->
            selector name attrs children

        _ ->
            []


seek : (Html.Parser.Node -> List b) -> List Html.Parser.Node -> List b
seek selector list =
    list
        |> List.concatMap
            (\n ->
                case selector n of
                    [] ->
                        node (\_ _ children -> seek selector children) n

                    r ->
                        r
            )


removeScriptTags : String -> Result String String
removeScriptTags input =
    Parser.run scriptRemoverParser input
        |> Result.mapError (\_ -> "Failed to remove scripts???")


scriptRemoverParser : Parser String
scriptRemoverParser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = ""
        , item = chunk
        , trailing = Parser.Optional
        , spaces = Parser.succeed ()
        }
        |. Parser.end
        |> Parser.map
            (\l ->
                l
                    |> Maybe.Extra.values
                    |> String.concat
            )


chunk : Parser (Maybe String)
chunk =
    Parser.oneOf
        [ Parser.succeed Nothing
            |. Parser.symbol "<script"
            |. Parser.Workaround.chompUntilEndOrAfter "</script>"
        , Parser.succeed Just
            |= Parser.getChompedString
                (Parser.chompIf (\_ -> True)
                    |. Parser.Workaround.chompUntilEndOrBefore "<script"
                )
        ]
