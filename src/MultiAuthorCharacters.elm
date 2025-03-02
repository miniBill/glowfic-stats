module MultiAuthorCharacters exposing (run)

import BackendTask exposing (BackendTask)
import Codecs
import Dict
import Dict.Extra
import FatalError exposing (FatalError)
import List.Extra
import Pages.Script as Script exposing (Script)
import Pages.Script.Spinner as Spinner
import Url.Builder
import Utils


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Spinner.steps
        |> Spinner.withStepWithOptions
            (Spinner.options "Getting users"
                |> Spinner.withOnCompletion
                    (\res ->
                        case res of
                            Err _ ->
                                ( Spinner.Fail, Nothing )

                            Ok users ->
                                ( Spinner.Succeed, Just ("Got " ++ String.fromInt (List.length users) ++ " users") )
                    )
            )
            (\_ -> Utils.getAllPages [ "users" ] [] Codecs.userDecoder)
        |> Spinner.withStepWithOptions
            (Spinner.options "Getting characters"
                |> Spinner.withOnCompletion
                    (\res ->
                        case res of
                            Err _ ->
                                ( Spinner.Fail, Nothing )

                            Ok characters ->
                                ( Spinner.Succeed, Just ("Got " ++ String.fromInt (List.length characters) ++ " characters") )
                    )
            )
            (\users ->
                users
                    |> List.map
                        (\user ->
                            Utils.getAllPages [ "characters" ] [ Url.Builder.int "user_id" user.id ] Codecs.characterDecoder
                                |> BackendTask.map
                                    (List.map
                                        (\character ->
                                            ( user
                                            , character
                                            )
                                        )
                                    )
                        )
                    |> BackendTask.sequence
                    |> BackendTask.map List.concat
            )
        |> Spinner.withStepWithOptions
            (Spinner.options "Calculating overlap"
                |> Spinner.withOnCompletion
                    (\res ->
                        case res of
                            Err _ ->
                                ( Spinner.Fail, Nothing )

                            Ok _ ->
                                ( Spinner.Succeed, Just "Calculated overlap" )
                    )
            )
            (\characters ->
                characters
                    |> Dict.Extra.groupBy
                        (\( _, { name } ) ->
                            name
                        )
                    |> BackendTask.succeed
            )
        |> Spinner.runSteps
        |> BackendTask.andThen
            (\overlaps ->
                overlaps
                    |> Dict.map
                        (\_ group ->
                            List.map (\( { username }, _ ) -> username) group
                                |> List.Extra.unique
                        )
                    |> Dict.toList
                    |> List.sortBy
                        (\( _, group ) ->
                            -(List.length group)
                        )
                    |> List.take 10
                    |> List.map
                        (\( name, group ) ->
                            name
                                ++ ": "
                                ++ String.join ", " group
                        )
                    |> List.map (\line -> Script.log line)
                    |> BackendTask.sequence
                    |> BackendTask.map (\_ -> ())
            )
