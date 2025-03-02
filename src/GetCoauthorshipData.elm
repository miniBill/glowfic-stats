module GetCoauthorshipData exposing (run)

import BackendTask exposing (BackendTask)
import CachedHttp
import Codecs
import FatalError exposing (FatalError)
import Json.Encode
import Pages.Script as Script exposing (Script)
import Pages.Script.Spinner as Spinner
import Utils


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Spinner.steps
        |> Spinner.withStepWithOptions
            (spinnerWithLabel "posts")
            (\_ -> Utils.getAllPages [ "posts" ] [] Codecs.postDecoder)
        |> Spinner.withStepWithOptions
            (spinnerWithLabel "posts' details")
            (\posts ->
                posts
                    |> List.sortBy .id
                    |> List.map
                        (\post ->
                            CachedHttp.getJson
                                (Utils.toUrl [ "posts", String.fromInt post.id ] [])
                                Codecs.postDetailsDecoder
                        )
                    |> BackendTask.sequence
            )
        |> Spinner.withStepWithOptions
            (Spinner.options "Writing output"
                |> Spinner.withOnCompletion
                    (\res ->
                        case res of
                            Err _ ->
                                ( Spinner.Fail, Nothing )

                            Ok _ ->
                                ( Spinner.Succeed, Just "Written output" )
                    )
            )
            (\posts ->
                Script.writeFile
                    { path = "alicorn.json"
                    , body =
                        posts
                            |> Json.Encode.list Codecs.postDetailsEncoder
                            |> Json.Encode.encode 0
                    }
                    |> BackendTask.allowFatal
            )
        |> Spinner.runSteps


spinnerWithLabel : String -> Spinner.Options FatalError (List a)
spinnerWithLabel label =
    Spinner.options ("Getting " ++ label)
        |> Spinner.withOnCompletion
            (\res ->
                case res of
                    Err _ ->
                        ( Spinner.Fail, Nothing )

                    Ok list ->
                        ( Spinner.Succeed, Just ("Got " ++ String.fromInt (List.length list) ++ " " ++ label) )
            )
