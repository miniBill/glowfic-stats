module GetCoauthorshipData exposing (run)

import BackendTask exposing (BackendTask)
import CachedHttp
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode
import Pages.Script as Script exposing (Script)
import Pages.Script.Spinner as Spinner
import Utils


run : Script
run =
    Script.withoutCliOptions task


type alias Post =
    { id : Int
    , subject : String
    }


postDecoder : Json.Decode.Decoder Post
postDecoder =
    Json.Decode.map2 Post
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "subject" Json.Decode.string)


type alias PostDetails =
    { id : Int
    , authors : List User
    }


postDetailsEncoder : PostDetails -> Json.Encode.Value
postDetailsEncoder postDetails =
    Json.Encode.object
        [ ( "id", Json.Encode.int postDetails.id )
        , ( "authors", Json.Encode.list userEncoder postDetails.authors )
        ]


postDetailsDecoder : Json.Decode.Decoder PostDetails
postDetailsDecoder =
    Json.Decode.map2 PostDetails
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "authors" (Json.Decode.list userDecoder))


type alias User =
    { id : Int
    , username : String
    }


userDecoder : Json.Decode.Decoder User
userDecoder =
    Json.Decode.map2 User
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "username" Json.Decode.string)


userEncoder : User -> Json.Encode.Value
userEncoder user =
    Json.Encode.object
        [ ( "id", Json.Encode.int user.id )
        , ( "username", Json.Encode.string user.username )
        ]


task : BackendTask FatalError ()
task =
    Spinner.steps
        |> Spinner.withStepWithOptions
            (spinnerWithLabel "posts")
            (\_ -> Utils.getAllPages [ "posts" ] [] postDecoder)
        |> Spinner.withStepWithOptions
            (spinnerWithLabel "posts' details")
            (\posts ->
                posts
                    |> List.sortBy .id
                    |> List.map
                        (\post ->
                            CachedHttp.getJson
                                (Utils.toUrl [ "posts", String.fromInt post.id ] [])
                                postDetailsDecoder
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
                            |> Json.Encode.list postDetailsEncoder
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
