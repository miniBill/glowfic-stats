module FacecastsCsvToGrid exposing (..)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Stream as Stream
import Csv.Decode as Decode exposing (Decoder)
import Csv.Encode as Encode
import Dict as CoreDict
import Dict.Extra
import FatalError exposing (FatalError)
import List.Extra
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
        links : CoreDict.Dict ( String, String ) Int
        links =
            csv
                |> Dict.Extra.groupBy Triple.Extra.first
                |> CoreDict.toList
                |> List.concatMap
                    (\( _, list ) ->
                        list
                            |> List.map Triple.Extra.third
                            |> Set.fromList
                            |> Set.toList
                            |> List.Extra.uniquePairs
                            |> List.concatMap (\( a, b ) -> [ ( a, b ), ( b, a ) ])
                    )
                |> Dict.Extra.groupBy identity
                |> CoreDict.map (\_ l -> List.length l)

        weight : CoreDict.Dict String ( Int, Int )
        weight =
            links
                |> CoreDict.toList
                |> List.map
                    (\( ( a, _ ), n ) ->
                        ( a, n )
                    )
                |> Dict.Extra.groupBy Tuple.first
                |> CoreDict.map
                    (\_ v ->
                        let
                            n : List Int
                            n =
                                List.map Tuple.second v
                        in
                        ( List.maximum n |> Maybe.withDefault 0, List.sum n )
                    )

        authors : List String
        authors =
            csv
                |> List.map Triple.Extra.third
                |> Set.fromList
                |> Set.toList
                |> List.filterMap
                    (\author ->
                        CoreDict.get author weight
                            |> Maybe.map (\( w1, w2 ) -> ( author, ( -w1, -w2 ) ))
                    )
                |> List.sortBy Tuple.second
                |> List.map Tuple.first
    in
    Encode.encode
        { encoder =
            Encode.withFieldNames
                (\author ->
                    authors
                        |> List.map
                            (\other ->
                                ( other
                                , if author == other then
                                    ""

                                  else
                                    CoreDict.get ( author, other ) links
                                        |> Maybe.withDefault 0
                                        |> String.fromInt
                                )
                            )
                        |> (::) ( "", author )
                )
        , fieldSeparator = ';'
        }
        authors
        |> Script.log


rowDecoder : Decoder ( String, String, String )
rowDecoder =
    Decode.map3 Triple.Extra.triple
        (Decode.column 0 Decode.string)
        (Decode.column 1 Decode.string)
        (Decode.column 2 Decode.string)
