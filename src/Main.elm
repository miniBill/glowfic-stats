module Main exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import FatalError exposing (FatalError)
import Json.Decode
import Pages.Script as Script exposing (Script)
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
    Do.do (Utils.getAllPages "templates" templateDecoder) <| \_ ->
    Script.log "Done"
