module Main exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import FatalError exposing (FatalError)
import Json.Decode
import Pages.Script as Script exposing (Script)
import Pages.Script.Spinner as Spinner
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


type alias User =
    { id : Int
    , username : String
    }


userDecoder : Json.Decode.Decoder User
userDecoder =
    Json.Decode.map2 User
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "username" Json.Decode.string)


task : BackendTask FatalError ()
task =
    Spinner.steps
        |> Spinner.withStep "Getting users" (\_ -> Utils.getAllPages "users" [] userDecoder)
        |> Spinner.withStep "Getting templates"
            (\users ->
                users
                    |> List.sortBy .id
                    |> List.map
                        (\user ->
                            Do.do (Utils.getAllPages "templates" [ Url.Builder.int "user_id" user.id ] templateDecoder) <| \templates ->
                            BackendTask.succeed (List.map (Tuple.pair user) templates)
                        )
                    |> BackendTask.sequence
                    |> BackendTask.map List.concat
            )
        |> Spinner.withStep "Getting characters"
            (\pairs ->
                pairs
                    |> List.sortBy (\( user, template ) -> ( user.id, template.id ))
                    |> List.map
                        (\(( user, template ) as pair) ->
                            Do.do (getCharactersCount user template) <| \count ->
                            BackendTask.succeed ( pair, count )
                        )
                    |> BackendTask.sequence
            )
        |> Spinner.runSteps
        |> BackendTask.andThen
            (\result ->
                let
                    sliced : List ( ( User, Template ), Int )
                    sliced =
                        result
                            |> List.sortBy (\( _, count ) -> -count)
                            |> List.take 10

                    msg : String
                    msg =
                        sliced
                            |> List.map
                                (\( ( user, template ), count ) ->
                                    "\n  "
                                        ++ user.username
                                        ++ " => "
                                        ++ template.name
                                        ++ ": "
                                        ++ String.fromInt count
                                )
                            |> String.concat
                in
                Script.log ("Done:" ++ msg)
            )


getCharactersCount : User -> Template -> BackendTask FatalError Int
getCharactersCount user template =
    Utils.getAllPages "characters"
        [ Url.Builder.int "user_id" user.id
        , Url.Builder.int "template_id" template.id
        ]
        Json.Decode.value
        |> BackendTask.quiet
        |> BackendTask.map List.length
