module MultiAuthorCharacters exposing (run)

import BackendTask exposing (BackendTask)
import Codecs
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
                    |> List.Extra.greedyGroupsOf 1
                    |> List.map BackendTask.combine
                    |> BackendTask.sequence
                    |> BackendTask.map List.concat
            )
        |> Spinner.runSteps
        |> BackendTask.map (\_ -> ())
