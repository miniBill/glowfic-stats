module Utils exposing (getAllPages, toUrl)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Http as Http
import CachedHttp
import Dict
import FatalError exposing (FatalError)
import Json.Decode exposing (Decoder)
import List.Extra
import Url
import Url.Builder


getAllPages : List String -> List Url.Builder.QueryParameter -> Decoder a -> BackendTask FatalError (List a)
getAllPages api params decoder =
    getLastPageIndex api params
        |> BackendTask.andThen (getAllPagesWithLast api params decoder)


getLastPageIndex : List String -> List Url.Builder.QueryParameter -> BackendTask FatalError (Maybe Int)
getLastPageIndex api params =
    let
        url : String
        url =
            toUrl api params
    in
    CachedHttp.isCached url
        |> BackendTask.andThen
            (\isCached ->
                if isCached then
                    BackendTask.succeed Nothing

                else
                    Http.get
                        url
                        (Http.expectWhatever ()
                            |> Http.withMetadata always
                        )
                        |> BackendTask.allowFatal
                        |> BackendTask.andThen
                            (\{ headers } ->
                                extractLastPageFromHeaders headers
                                    |> Result.mapError FatalError.fromString
                                    |> BackendTask.fromResult
                            )
            )


toUrl : List String -> List Url.Builder.QueryParameter -> String
toUrl api params =
    Url.Builder.crossOrigin "https://glowfic.com"
        ([ "api", "v1" ] ++ api)
        params


extractLastPageFromHeaders : Dict.Dict String String -> Result String (Maybe Int)
extractLastPageFromHeaders headers =
    let
        lastLinkPrefix : String
        lastLinkPrefix =
            "<"

        lastLinkSuffix : String
        lastLinkSuffix =
            ">; rel=\"last\""
    in
    case Dict.get "link" headers of
        Nothing ->
            Ok Nothing

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
                    case
                        sliced
                            |> Url.fromString
                            |> Maybe.andThen
                                (\{ query } ->
                                    query
                                        |> Maybe.andThen
                                            (\q ->
                                                q
                                                    |> String.split "&"
                                                    |> List.Extra.findMap
                                                        (\fragment ->
                                                            case String.split "=" fragment of
                                                                [ key, value ] ->
                                                                    if key == "page" then
                                                                        String.toInt value

                                                                    else
                                                                        Nothing

                                                                _ ->
                                                                    Nothing
                                                        )
                                            )
                                )
                    of
                        Just i ->
                            Ok (Just i)

                        Nothing ->
                            Err <| "Last page is supposed to be \"" ++ sliced ++ "\", but I couldn't parse that"

                [] ->
                    Err "Could not find link to the last page"

                _ ->
                    Err "Ambiguous link to the last page"


getAllPagesWithLast : List String -> List Url.Builder.QueryParameter -> Decoder a -> Maybe Int -> BackendTask FatalError (List a)
getAllPagesWithLast api params decoder maybeLastPage =
    case maybeLastPage of
        Nothing ->
            Do.do
                (CachedHttp.getJson
                    (toUrl api params)
                    (Json.Decode.field "results" <| Json.Decode.list decoder)
                )
            <| \decoded ->
            BackendTask.succeed decoded

        Just lastPage ->
            List.range 1 lastPage
                |> List.map
                    (\pageId ->
                        Do.do
                            (CachedHttp.getJson
                                (toUrl api (Url.Builder.int "page" pageId :: params))
                                (Json.Decode.field "results" <| Json.Decode.list decoder)
                            )
                        <| \decoded ->
                        BackendTask.succeed decoded
                    )
                |> List.Extra.greedyGroupsOf 10
                |> List.map BackendTask.sequence
                |> BackendTask.sequence
                |> BackendTask.map (List.concatMap List.concat)
