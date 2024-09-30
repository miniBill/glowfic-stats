module CachedHttp exposing (getJson)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import BackendTask.Http as Http
import FatalError exposing (FatalError)
import Json.Decode exposing (Decoder)
import Pages.Script as Script
import Sha256


getJson : String -> Decoder data -> BackendTask FatalError data
getJson url decoder =
    let
        filename =
            ".cache/" ++ Sha256.sha256 url
    in
    File.jsonFile decoder filename
        |> BackendTask.allowFatal
        |> BackendTask.onError
            (\_ ->
                Do.allowFatal (Http.get url Http.expectString) <| \raw ->
                Do.do (Script.sleep 300) <| \_ ->
                Do.allowFatal (Script.writeFile { path = filename, body = raw }) <| \_ ->
                BackendTask.allowFatal (File.jsonFile decoder filename)
            )
