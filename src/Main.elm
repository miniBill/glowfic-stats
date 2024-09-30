module Main exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import FatalError exposing (FatalError)
import Json.Decode
import Pages.Script as Script exposing (Script)
import Url.Builder
import Utils


run : Script
run =
    Script.withoutCliOptions task


type alias Template =
    { id : Int
    , name : String
    }


templateDecoder : Json.Decode.Decoder Template
templateDecoder =
    Json.Decode.map2 Template
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)


task : BackendTask FatalError ()
task =
    Do.do (Utils.getAllPages "templates" [] templateDecoder) <| \templates ->
    Do.each (List.indexedMap Tuple.pair templates) (getCharactersCount (List.length templates)) <| \counts ->
    let
        sliced =
            counts
                |> List.sortBy (\( _, count ) -> -count)
                |> List.take 3

        msg =
            sliced
                |> List.map (\( name, count ) -> "\n  " ++ name ++ ": " ++ String.fromInt count)
                |> String.concat
    in
    Script.log ("Done:" ++ msg)


getCharactersCount : Int -> ( Int, Template ) -> BackendTask FatalError ( String, Int )
getCharactersCount count ( index, template ) =
    Do.log (String.fromInt ((index + 1) * 100 // count) ++ "% - " ++ template.name) <| \_ ->
    Utils.getAllPages "characters" [ Url.Builder.int "template_id" template.id ] Json.Decode.value
        |> BackendTask.quiet
        |> BackendTask.map (\characters -> ( template.name, List.length characters ))
