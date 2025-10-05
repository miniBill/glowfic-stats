module Galaxy exposing (..)

import Angle exposing (Angle)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Codecs
import Color
import Dict
import Direction3d
import Graph exposing (Edge, Graph, Node)
import Html exposing (Html)
import Html.Attributes
import Http
import Illuminance
import Json.Decode
import Length
import List.Extra
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Light as Light
import Scene3d.Material as Material
import Sphere3d
import Task
import Types exposing (PostDetails)
import Viewpoint3d


type alias Model =
    { graph : Maybe (Graph String (List PostDetails))
    , size : ( Quantity Int Pixels, Quantity Int Pixels )
    }


type Msg
    = GotData (Result Http.Error (List PostDetails))
    | Resize (Quantity Int Pixels) (Quantity Int Pixels)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { graph = Nothing
      , size = ( Pixels.pixels 800, Pixels.pixels 600 )
      }
    , Cmd.batch
        [ Http.get
            { url = "/alicorn.json"
            , expect = Http.expectJson GotData (Json.Decode.list Codecs.postDetailsDecoder)
            }
        , Browser.Dom.getViewport
            |> Task.perform
                (\{ viewport } ->
                    Resize
                        (Pixels.pixels (floor viewport.width))
                        (Pixels.pixels (floor viewport.height))
                )
        ]
    )


view : Model -> Html Msg
view model =
    case model.graph of
        Nothing ->
            Html.div
                [ Html.Attributes.style "padding" "8px"
                ]
                [ Html.text "Loading..." ]

        Just graph ->
            Scene3d.custom
                { dimensions = model.size
                , camera =
                    Camera3d.perspective
                        { viewpoint =
                            Viewpoint3d.lookAt
                                { eyePoint = Point3d.meters 4 0 3
                                , focalPoint = Point3d.origin
                                , upDirection = Direction3d.positiveZ
                                }
                        , verticalFieldOfView = Angle.degrees 30
                        }
                , entities = spheres graph
                , clipDepth = Length.meter
                , background = Scene3d.backgroundColor Color.black
                , lights =
                    Scene3d.oneLight
                        (Light.soft
                            { upDirection = Direction3d.positiveZ
                            , chromaticity = Light.fluorescent
                            , intensityAbove = Illuminance.lux 400
                            , intensityBelow = Illuminance.lux 100
                            }
                        )
                , exposure = Scene3d.exposureValue 7
                , toneMapping = Scene3d.noToneMapping
                , whiteBalance = Light.daylight
                , antialiasing = Scene3d.multisampling
                }


spheres : Graph n e -> List (Entity coordinates)
spheres graph =
    let
        nodeCount : Int
        nodeCount =
            min 20 (Graph.size graph)
    in
    Graph.nodes graph
        |> List.take nodeCount
        |> List.map
            (\node ->
                let
                    angle : Angle
                    angle =
                        Angle.degrees (toFloat node.id * 360 / toFloat nodeCount)
                in
                Scene3d.sphere
                    (Material.metal
                        { baseColor = Color.blue
                        , roughness = 0
                        }
                    )
                    (Sphere3d.atPoint
                        (Point3d.meters
                            (Angle.sin angle)
                            (Angle.cos angle)
                            (0.0000005
                                * (angle
                                    |> Quantity.multiplyBy 10
                                    |> Angle.sin
                                  )
                            )
                        )
                        (Length.centimeters 10)
                    )
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize w h ->
            ( { model | size = ( w, h ) }, Cmd.none )

        GotData (Err e) ->
            let
                _ =
                    Debug.log "Error" e
            in
            ( model, Cmd.none )

        GotData (Ok posts) ->
            let
                ( authors, links ) =
                    List.foldl
                        (\post ( outerAuthorsAcc, outerLinksAcc ) ->
                            ( List.foldl
                                (\author innerAuthorsAcc ->
                                    Dict.insert author.id author.username innerAuthorsAcc
                                )
                                outerAuthorsAcc
                                post.authors
                            , if List.length post.authors > 5 then
                                outerLinksAcc

                              else
                                foldl2
                                    (\( _, others ) -> List.Extra.select others)
                                    (\( firstAuthor, _ ) ( secondAuthor, _ ) innerLinksAcc ->
                                        Dict.update
                                            ( firstAuthor.id, secondAuthor.id )
                                            (\existing ->
                                                existing
                                                    |> Maybe.withDefault []
                                                    |> (::) post
                                                    |> Just
                                            )
                                            innerLinksAcc
                                    )
                                    outerLinksAcc
                                    (List.Extra.select post.authors)
                            )
                        )
                        ( Dict.empty, Dict.empty )
                        posts

                nodes : List (Node String)
                nodes =
                    authors
                        |> Dict.toList
                        |> List.map
                            (\( id, name ) ->
                                { id = id
                                , label = name
                                }
                            )

                edges : List (Edge (List PostDetails))
                edges =
                    links
                        |> Dict.toList
                        |> List.map
                            (\( ( from, to ), details ) ->
                                { from = from
                                , to = to
                                , label = details
                                }
                            )
            in
            ( { model | graph = Just (Graph.fromNodesAndEdges nodes edges) }
            , Cmd.none
            )


foldl2 : (a -> List b) -> (a -> b -> c -> c) -> c -> List a -> c
foldl2 toList step list =
    List.foldl (\o oacc -> List.foldl (\i iacc -> step o i iacc) oacc (toList o)) list


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> Resize (Pixels.pixels w) (Pixels.pixels h))
