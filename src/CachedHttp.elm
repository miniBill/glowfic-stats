module CachedHttp exposing (getJson, isCached)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import BackendTask.Glob as Glob
import BackendTask.Http as Http
import FatalError exposing (FatalError)
import Json.Decode exposing (Decoder)
import Pages.Script as Script
import Result.Extra
import Sha256


getJson : String -> Decoder data -> BackendTask FatalError data
getJson url decoder =
    let
        filename =
            toFilename url
    in
    File.jsonFile decoder filename
        |> BackendTask.allowFatal
        |> BackendTask.onError
            (\_ ->
                Do.allowFatal (Http.get url Http.expectString) <| \raw ->
                Do.do (Script.sleep 200) <| \_ ->
                Do.allowFatal (Script.writeFile { path = filename, body = raw }) <| \_ ->
                BackendTask.allowFatal (File.jsonFile decoder filename)
            )


toFilename : String -> String
toFilename url =
    ".cache/" ++ Sha256.sha256 url


isCached : String -> BackendTask error Bool
isCached url =
    let
        filename =
            toFilename url
    in
    Glob.literal filename
        |> Glob.expectUniqueMatch
        |> BackendTask.toResult
        |> BackendTask.map Result.Extra.isOk
