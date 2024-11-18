module AlicornNumber exposing (main)

import Browser
import Dict
import Element exposing (Element, column, height, padding, paragraph, px, shrink, spacing, text, textColumn, wrappedRow)
import Element.Border as Border
import Element.Input as Input
import GetCoauthorshipData exposing (PostDetails)
import Graph exposing (Edge, Graph, Node)
import Http
import Json.Decode
import List.Extra


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
    | GotData (Result Http.Error (List GetCoauthorshipData.PostDetails))
    | CalculateShortestPath
    | CalculateAllPathsFromAlicorn


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
        , expect = Http.expectJson GotData (Json.Decode.list GetCoauthorshipData.postDetailsDecoder)
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
    case msg of
        GotData (Err _) ->
            ( maybeModel, Cmd.none )

        GotData (Ok posts) ->
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
                            , foldl2
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

        First first ->
            case maybeModel of
                Nothing ->
                    ( Nothing, Cmd.none )

                Just model ->
                    ( Just { model | first = first }, Cmd.none )

        Second second ->
            case maybeModel of
                Nothing ->
                    ( Nothing, Cmd.none )

                Just model ->
                    ( Just { model | second = second }, Cmd.none )

        CalculateShortestPath ->
            case maybeModel of
                Nothing ->
                    ( Nothing, Cmd.none )

                Just model ->
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

                        visitor contextes {- distance -} _ acc =
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

        CalculateAllPathsFromAlicorn ->
            case maybeModel of
                Nothing ->
                    ( Nothing, Cmd.none )

                Just model ->
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

                        visitor : Graph.BfsNodeVisitor String e (List ( String, ( Int, String ) ))
                        visitor contexts distance acc =
                            case contexts of
                                [] ->
                                    acc

                                this :: _ ->
                                    ( contexts
                                        |> List.map (\{ node } -> node.label)
                                        |> List.reverse
                                        |> String.join " -> "
                                    , ( distance, this.node.label )
                                    )
                                        :: acc

                        ( chains, leftovers ) =
                            Graph.guidedBfs
                                Graph.alongOutgoingEdges
                                visitor
                                from
                                []
                                model.graph
                    in
                    ( Just
                        { model
                            | result =
                                (chains
                                    |> List.sortBy Tuple.second
                                    |> List.map Tuple.first
                                )
                                    ++ "\nUnconnected:"
                                    :: (leftovers
                                            |> Graph.nodes
                                            |> List.map .label
                                       )
                                    |> String.join "\n"
                                    |> Just
                        }
                    , Cmd.none
                    )


foldl2 : (a -> List b) -> (a -> b -> c -> c) -> c -> List a -> c
foldl2 toList step list =
    List.foldl (\o oacc -> List.foldl (\i iacc -> step o i iacc) oacc (toList o)) list


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
