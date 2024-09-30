module Utils exposing (getAllPages)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Http as Http
import CachedHttp
import Dict
import FatalError exposing (FatalError)
import Json.Decode exposing (Decoder)
import List.Extra
import Pages.Script as Script


getAllPages : String -> Decoder a -> BackendTask FatalError (List a)
getAllPages api decoder =
    getLastPage api |> BackendTask.andThen (getAllPagesWithLast api decoder)


getLastPage : String -> BackendTask FatalError Int
getLastPage api =
    Http.get ("https://glowfic.com/api/v1/" ++ api)
        (Http.expectWhatever ()
            |> Http.withMetadata always
        )
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\{ headers } ->
                extractLastPageFromHeaders api headers
                    |> Result.mapError FatalError.fromString
                    |> BackendTask.fromResult
            )


extractLastPageFromHeaders : String -> Dict.Dict String String -> Result String Int
extractLastPageFromHeaders api headers =
    let
        lastLinkPrefix : String
        lastLinkPrefix =
            "<https://glowfic.com/api/v1/" ++ api ++ "?page="

        lastLinkSuffix : String
        lastLinkSuffix =
            ">; rel=\"last\""
    in
    case Dict.get "link" headers of
        Nothing ->
            Err "Links not found"

        Just link ->
            case
                List.filter
                    (\fragment ->
                        String.endsWith lastLinkSuffix fragment
                            && String.startsWith lastLinkPrefix fragment
                    )
                    (String.split "," link)
            of
                [ last ] ->
                    let
                        sliced =
                            String.slice
                                (String.length lastLinkPrefix)
                                -(String.length lastLinkSuffix)
                                last
                    in
                    case String.toInt sliced of
                        Just i ->
                            Ok i

                        Nothing ->
                            Err <| "Last page is supposed to be \"" ++ sliced ++ "\", but that's not an int"

                [] ->
                    Err "Could not find link to the last page"

                _ ->
                    Err "Ambiguous link to the last page"


getAllPagesWithLast : String -> Decoder a -> Int -> BackendTask FatalError (List a)
getAllPagesWithLast api decoder lastPage =
    List.range 1 lastPage
        |> List.map
            (\pageId ->
                Do.do
                    (CachedHttp.getJson
                        ("https://glowfic.com/api/v1/" ++ api ++ "?page=" ++ String.fromInt pageId)
                        (Json.Decode.field "results" <| Json.Decode.list decoder)
                    )
                <| \decoded ->
                Do.do (Script.sleep 300) <| \_ ->
                BackendTask.succeed decoded
            )
        |> List.Extra.greedyGroupsOf 10
        |> List.map BackendTask.sequence
        |> BackendTask.sequence
        |> BackendTask.map (List.concatMap List.concat)
