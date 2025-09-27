module FacecastsCsvToDot exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Stream as Stream
import Csv.Decode as Decode exposing (Decoder)
import Dict.Extra
import FastDict as Dict exposing (Dict)
import FatalError exposing (FatalError)
import Graph
import Graph.DOT
import Pages.Script as Script exposing (Script)
import Set
import Triple.Extra


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Do.allowFatal (Stream.stdin |> Stream.read) <| \{ body } ->
    Do.do
        (Decode.decodeCsv Decode.NoFieldNames rowDecoder body
            |> Result.mapError (\_ -> FatalError.fromString "Failed to parse CSV")
            |> BackendTask.fromResult
        )
    <| \csv ->
    let
        links : List ( String, String, String )
        links =
            csv
                |> List.foldl
                    (\( actor, link, author ) acc ->
                        case Dict.get ( actor, author ) acc of
                            Nothing ->
                                Dict.insert ( actor, author ) ( actor, link, author ) acc

                            Just ( _, ex, _ ) ->
                                Dict.insert ( actor, author ) ( actor, ex ++ ", " ++ link, author ) acc
                    )
                    Dict.empty
                |> Dict.values
                |> Dict.Extra.groupBy Triple.Extra.first
                |> Dict.fromCoreDict
                |> Dict.filter (\_ v -> List.length v > 1)
                |> Dict.values
                |> List.concat

        authors : Dict String Int
        authors =
            links
                |> List.map Triple.Extra.third
                |> Set.fromList
                |> Set.toList
                |> List.indexedMap (\i author -> ( author, i ))
                |> Dict.fromList

        actors : Dict String Int
        actors =
            links
                |> List.map Triple.Extra.first
                |> Set.fromList
                |> Set.toList
                |> List.indexedMap (\i actor -> ( actor, i + Dict.size authors ))
                |> Dict.fromList

        nodes : List (Graph.Node String)
        nodes =
            (Dict.toList authors ++ Dict.toList actors)
                |> List.map (\( label, i ) -> { id = i, label = label })

        edges : List (Graph.Edge String)
        edges =
            List.filterMap
                (\( actor, link, author ) ->
                    Maybe.map2
                        (\authorId actorId ->
                            { from = authorId
                            , to = actorId
                            , label = link
                            }
                        )
                        (Dict.get author authors)
                        (Dict.get actor actors)
                )
                links
    in
    Graph.fromNodesAndEdges nodes edges
        |> Graph.DOT.output Just (always Nothing {- Just -})
        |> Script.log


rowDecoder : Decoder ( String, String, String )
rowDecoder =
    Decode.map3 Triple.Extra.triple
        (Decode.column 0 Decode.string)
        (Decode.column 1 Decode.string)
        (Decode.column 2 Decode.string)
