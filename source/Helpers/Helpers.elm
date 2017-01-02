module Helpers.Helpers exposing (..)

import Ui.Helpers.Env
import Json.Decode as Decode
import Json.Encode as Encode
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import HttpBuilder exposing (..)
import Helpers.Models exposing (..)
import RemoteData exposing (..)


apiUrl : String
apiUrl =
    let
        endpoint =
            Ui.Helpers.Env.get "endpoint" Decode.string
                |> Result.withDefault "http://localhost"
    in
        endpoint ++ "api/"


fetcher : String -> Decode.Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
fetcher url decoder msg =
    HttpBuilder.get url
        |> withExpect (Http.expectJson decoder)
        |> withCredentials
        |> send msg


poster : String -> Encode.Value -> Decode.Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
poster url value decoder msg =
    HttpBuilder.post url
        |> withJsonBody value
        |> withExpect (Http.expectJson decoder)
        |> withCredentials
        |> send msg


putter : String -> Encode.Value -> Decode.Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
putter url value decoder msg =
    HttpBuilder.put url
        |> withJsonBody value
        |> withExpect (Http.expectJson decoder)
        |> withCredentials
        |> send msg


type HttpMethod
    = Post
    | Put


requester : String -> HttpMethod -> Encode.Value -> Decode.Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
requester url method value decoder msg =
    let
        requestBuilder =
            case method of
                Post ->
                    HttpBuilder.post

                Put ->
                    HttpBuilder.put
    in
        requestBuilder url
            |> withJsonBody value
            |> withExpect (Http.expectJson decoder)
            |> withCredentials
            |> send msg


fullAddress : Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String
fullAddress maybeAddress1 maybeAddress2 maybeAddress3 maybeAddress4 maybePostcode =
    [ maybeAddress1, maybeAddress2, maybeAddress3, maybeAddress4, maybePostcode ]
        |> List.map (\mb -> Maybe.withDefault "" mb)
        |> List.filter (\a -> String.length (a) > 0)
        |> String.join ", "
        |> Just


nodeTypeToPath : NodeType -> String
nodeTypeToPath nodeType =
    case nodeType of
        RootType ->
            "Root"

        CustomerType ->
            "Customer"

        ClientType ->
            "Client"

        SiteType ->
            "Site"

        StaffType ->
            "Staff"

        FolderType ->
            "Folder"


viewWebData : WebData a -> (a -> List (Html msg)) -> (String -> List (Html msg)) -> List (Html msg)
viewWebData webdata viewSuccess viewPending =
    case webdata of
        NotAsked ->
            (viewPending "fa fa-spin fa-spinner")

        Loading ->
            (viewPending "fa fa-spin fa-refresh")

        Failure err ->
            (viewPending "fa fa-exclamation-triangle")

        Success data ->
            (viewSuccess data)


viewPendingDefault : String -> List (Html msg)
viewPendingDefault iconClass =
    [ div [ class "header-loading" ]
        [ i [ class iconClass ] [] ]
    ]


maybeUpdate : (a -> ( b, Cmd c )) -> Maybe a -> ( Maybe b, Cmd c )
maybeUpdate f maybe =
    case maybe of
        Just data ->
            let
                ( first, second ) =
                    f data
            in
                ( Just first, second )

        Nothing ->
            ( Nothing, Cmd.none )
