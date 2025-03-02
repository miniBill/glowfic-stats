module Codecs exposing (characterDecoder, postDecoder, postDetailsDecoder, postDetailsEncoder, templateDecoder, userDecoder, userEncoder)

import Json.Decode
import Json.Encode
import Types exposing (Character, Post, PostDetails, Template, User)


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


templateDecoder : Json.Decode.Decoder Template
templateDecoder =
    Json.Decode.map2 Template
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)


postDecoder : Json.Decode.Decoder Post
postDecoder =
    Json.Decode.map2 Post
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "subject" Json.Decode.string)


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


characterDecoder : Json.Decode.Decoder Character
characterDecoder =
    Json.Decode.map2 Character
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)
