module AlicornNumber exposing (main)

import Browser
import Codecs
import Dict
import Element exposing (Element, column, height, padding, paragraph, px, shrink, spacing, text, textColumn, wrappedRow)
import Element.Border as Border
import Element.Input as Input
import File.Download
import Graph exposing (Edge, Graph, Node)
import Graph.DOT
import Graph.TGF
import Http
import Json.Decode
import List.Extra
import Types exposing (PostDetails)


type alias Model =
    Maybe
        { graph : Graph String (List PostDetails)
        , first : String
        , second : String
        , result : Maybe String
        }


type Msg
    = First String
    | Second String
    | GotData (Result Http.Error (List PostDetails))
    | CalculateShortestPath
    | CalculateAllPathsFromAlicorn
    | GenerateDOTfile
    | GenerateTGFfile


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = \model -> Element.layout [] (view model)
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Nothing
    , Http.get
        { url = "/alicorn.json"
        , expect = Http.expectJson GotData (Json.Decode.list Codecs.postDetailsDecoder)
        }
    )


view : Model -> Element Msg
view maybeModel =
    case maybeModel of
        Nothing ->
            text "Loading..."

        Just model ->
            column
                [ spacing 8
                , padding 8
                ]
                [ Input.text []
                    { label = Input.labelAbove [] (text "First")
                    , text = model.first
                    , onChange = First
                    , placeholder = Nothing
                    }
                , Input.text []
                    { label = Input.labelAbove [] (text "Second")
                    , text = model.second
                    , onChange = Second
                    , placeholder = Just (Input.placeholder [] (text "Leave empty to use Alicorn"))
                    }
                , wrappedRow
                    [ spacing 8 ]
                    [ Input.button
                        [ padding 8
                        , Border.width 1
                        ]
                        { onPress = Just CalculateShortestPath
                        , label = text "Calculate"
                        }
                    , Input.button
                        [ padding 8
                        , Border.width 1
                        ]
                        { onPress = Just CalculateAllPathsFromAlicorn
                        , label = text "Calculate all paths from Alicorn"
                        }
                    , Input.button
                        [ padding 8
                        , Border.width 1
                        ]
                        { onPress = Just GenerateDOTfile
                        , label = text "Generate .dot file"
                        }
                    , Input.button
                        [ padding 8
                        , Border.width 1
                        ]
                        { onPress = Just GenerateTGFfile
                        , label = text "Generate .tgf file"
                        }
                    ]
                , case model.result of
                    Nothing ->
                        Element.none

                    Just chain ->
                        chain
                            |> String.split "\n"
                            |> List.map
                                (\line ->
                                    paragraph
                                        [ if String.isEmpty line then
                                            height (px 8)

                                          else
                                            height shrink
                                        ]
                                        [ text line ]
                                )
                            |> textColumn []
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg maybeModel =
    case ( msg, maybeModel ) of
        ( GotData (Err _), _ ) ->
            ( maybeModel, Cmd.none )

        ( GotData (Ok posts), _ ) ->
            let
                ( authors, links ) =
                    List.foldl
                        (\post ( outerAuthorsAcc, outerLinksAcc ) ->
                            ( List.foldl
                                (\author innerAuthorsAcc ->
                                    Dict.insert author.id author.username innerAuthorsAcc
                                )
                                outerAuthorsAcc
                                post.authors
                            , if List.length post.authors > 5 then
                                outerLinksAcc

                              else
                                foldl2
                                    (\( _, others ) -> List.Extra.select others)
                                    (\( firstAuthor, _ ) ( secondAuthor, _ ) innerLinksAcc ->
                                        Dict.update
                                            ( firstAuthor.id, secondAuthor.id )
                                            (\existing ->
                                                existing
                                                    |> Maybe.withDefault []
                                                    |> (::) post
                                                    |> Just
                                            )
                                            innerLinksAcc
                                    )
                                    outerLinksAcc
                                    (List.Extra.select post.authors)
                            )
                        )
                        ( Dict.empty, Dict.empty )
                        posts

                nodes : List (Node String)
                nodes =
                    authors
                        |> Dict.toList
                        |> List.map
                            (\( id, name ) ->
                                { id = id
                                , label = name
                                }
                            )

                edges : List (Edge (List PostDetails))
                edges =
                    links
                        |> Dict.toList
                        |> List.map
                            (\( ( from, to ), details ) ->
                                { from = from
                                , to = to
                                , label = details
                                }
                            )
            in
            ( Just
                { graph = Graph.fromNodesAndEdges nodes edges
                , first = ""
                , second = ""
                , result = Nothing
                }
            , Cmd.none
            )

        ( _, Nothing ) ->
            ( Nothing, Cmd.none )

        ( First first, Just model ) ->
            ( Just { model | first = first }, Cmd.none )

        ( Second second, Just model ) ->
            ( Just { model | second = second }, Cmd.none )

        ( CalculateShortestPath, Just model ) ->
            let
                from : List Graph.NodeId
                from =
                    Graph.nodes model.graph
                        |> List.filterMap
                            (\node ->
                                if String.toLower node.label == String.toLower model.first then
                                    Just node.id

                                else
                                    Nothing
                            )

                visitor contextes _ acc =
                    case contextes of
                        [] ->
                            acc

                        last :: _ ->
                            if
                                String.toLower last.node.label
                                    == String.toLower
                                        (if String.isEmpty model.second then
                                            "Alicorn"

                                         else
                                            model.second
                                        )
                            then
                                contextes
                                    |> List.map (\{ node } -> node.label)
                                    |> List.reverse
                                    |> String.join " -> "
                                    |> Just

                            else
                                acc
            in
            ( Just
                { model
                    | result =
                        Graph.guidedBfs
                            Graph.alongOutgoingEdges
                            visitor
                            from
                            Nothing
                            model.graph
                            |> Tuple.first
                }
            , Cmd.none
            )

        ( CalculateAllPathsFromAlicorn, Just model ) ->
            let
                from : List Graph.NodeId
                from =
                    Graph.nodes model.graph
                        |> List.filterMap
                            (\node ->
                                if node.label == "Alicorn" then
                                    Just node.id

                                else
                                    Nothing
                            )

                visitor : Graph.BfsNodeVisitor String e ( List ( String, ( Int, String ) ), Int, Int )
                visitor contexts distance ( acc, oldN, oldC ) =
                    case contexts of
                        [] ->
                            ( acc, oldN, oldC )

                        this :: _ ->
                            ( ( contexts
                                    |> List.map (\{ node } -> node.label)
                                    |> List.reverse
                                    |> String.join " -> "
                              , ( distance, String.toLower this.node.label )
                              )
                                :: acc
                            , oldN + distance
                            , oldC + 1
                            )

                ( ( chains, n, c ), leftovers ) =
                    Graph.guidedBfs
                        Graph.alongOutgoingEdges
                        visitor
                        from
                        ( [], 0, 0 )
                        model.graph

                average =
                    toFloat n / toFloat c
            in
            ( Just
                { model
                    | result =
                        (chains
                            |> List.sortBy Tuple.second
                            |> List.map Tuple.first
                        )
                            ++ ("\nAverage Alicorn number: "
                                    ++ (round (average * 10) |> toFloat |> (\q -> q / 10) |> String.fromFloat)
                                    ++ "\nUnconnected:"
                               )
                            :: (leftovers
                                    |> Graph.nodes
                                    |> List.map .label
                               )
                            |> String.join "\n"
                            |> Just
                }
            , Cmd.none
            )

        ( GenerateDOTfile, Just model ) ->
            ( maybeModel
            , model.graph
                |> Graph.DOT.output Just (\_ -> Nothing)
                |> File.Download.string "glowfic.dot" "text/plain"
            )

        ( GenerateTGFfile, Just model ) ->
            ( maybeModel
            , model.graph
                |> Graph.TGF.output identity (\_ -> "")
                |> File.Download.string "glowfic.tgf" "text/plain"
            )


foldl2 : (a -> List b) -> (a -> b -> c -> c) -> c -> List a -> c
foldl2 toList step list =
    List.foldl (\o oacc -> List.foldl (\i iacc -> step o i iacc) oacc (toList o)) list


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
