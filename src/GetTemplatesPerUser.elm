module GetTemplatesPerUser exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import Dict
import Dict.Extra
import FatalError exposing (FatalError)
import Json.Decode
import Pages.Script as Script exposing (Script)
import Pages.Script.Spinner as Spinner
import Triple.Extra
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
            (\_ -> Utils.getAllPages [ "users" ] [] userDecoder)
        |> Spinner.withStepWithOptions
            (Spinner.options "Getting templates"
                |> Spinner.withOnCompletion
                    (\res ->
                        case res of
                            Err _ ->
                                ( Spinner.Fail, Nothing )

                            Ok templates ->
                                ( Spinner.Succeed, Just ("Got " ++ String.fromInt (List.length templates) ++ " templates") )
                    )
            )
            (\users ->
                users
                    |> List.sortBy .id
                    |> List.map
                        (\user ->
                            Do.do (Utils.getAllPages [ "templates" ] [ Url.Builder.int "user_id" user.id ] templateDecoder) <| \templates ->
                            BackendTask.succeed (List.map (Tuple.pair user) templates)
                        )
                    |> BackendTask.sequence
                    |> BackendTask.map List.concat
            )
        |> Spinner.withStepWithOptions
            (Spinner.options "Getting characters"
                |> Spinner.withOnCompletion
                    (\res ->
                        case res of
                            Err _ ->
                                ( Spinner.Fail, Nothing )

                            Ok characters ->
                                ( Spinner.Succeed, Just ("Got " ++ String.fromInt (characters |> List.map Triple.Extra.third |> List.sum) ++ " characters") )
                    )
            )
            (\pairs ->
                pairs
                    |> List.sortBy (\( user, template ) -> ( user.id, template.id ))
                    |> List.map
                        (\( user, template ) ->
                            Do.do (getCharactersCount user template) <| \count ->
                            BackendTask.succeed ( user, template, count )
                        )
                    |> BackendTask.sequence
            )
        |> Spinner.runSteps
        |> BackendTask.andThen
            (\result ->
                let
                    sliced : List ( User, Template, Int )
                    sliced =
                        result
                            |> Dict.Extra.groupBy
                                (\( user, template, _ ) ->
                                    if user.username == "Anya" && (String.startsWith "β " template.name || String.startsWith "γ " template.name) then
                                        ( user.id, String.left 2 template.name )

                                    else
                                        ( user.id, String.fromInt template.id )
                                )
                            |> Dict.values
                            |> List.filterMap
                                (\list ->
                                    case list of
                                        [] ->
                                            Nothing

                                        ( user, _, _ ) :: _ ->
                                            ( user
                                            , { id =
                                                    list
                                                        |> List.map (\( _, { id }, _ ) -> id)
                                                        |> List.minimum
                                                        |> Maybe.withDefault -1
                                              , name =
                                                    list
                                                        |> List.map (\( _, { name }, _ ) -> name)
                                                        |> String.join ", "
                                              }
                                            , list
                                                |> List.map (\( _, _, count ) -> count)
                                                |> List.sum
                                            )
                                                |> Just
                                )
                            |> List.sortBy (\( _, _, count ) -> -count)
                            |> List.take 100

                    msg : String
                    msg =
                        sliced
                            |> List.map
                                (\( user, template, count ) ->
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
    Utils.getAllPages [ "characters" ]
        [ Url.Builder.int "user_id" user.id
        , Url.Builder.int "template_id" template.id
        ]
        Json.Decode.value
        |> BackendTask.map List.length
