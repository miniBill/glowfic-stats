module MultiAuthorCharacters exposing (run)

import BackendTask exposing (BackendTask)
import Codecs
import FatalError exposing (FatalError)
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
        |> Spinner.withStep "Got users" (\_ -> BackendTask.succeed ())
        |> Spinner.runSteps
