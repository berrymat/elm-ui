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
import Http exposing (..)
import Navigation


apiUrl : String
apiUrl =
    let
        endpoint =
            Ui.Helpers.Env.get "endpoint" Decode.string
                |> Result.withDefault "http://localhost"
    in
        endpoint ++ "api/"


fetcher : String -> String -> Decode.Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
fetcher collection id decoder msg =
    HttpBuilder.get (apiUrl ++ collection ++ "/" ++ id)
        |> withExpect (Http.expectJson decoder)
        |> withCredentials
        |> HttpBuilder.send msg


poster : AuthToken -> String -> Encode.Value -> Decode.Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
poster token collection value decoder msg =
    requester token collection "" Post value decoder msg


putter : AuthToken -> String -> String -> Encode.Value -> Decode.Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
putter token collection id value decoder msg =
    requester token collection id Put value decoder msg


type HttpMethod
    = Post
    | Put
    | Delete


request : AuthToken -> String -> String -> HttpMethod -> Encode.Value -> Decode.Decoder a -> ( String, Http.Request a )
request token collection id method value decoder =
    let
        ( requestBuilder, url ) =
            case method of
                Post ->
                    ( HttpBuilder.post, apiUrl ++ collection )

                Put ->
                    ( HttpBuilder.put, apiUrl ++ collection ++ "/" ++ id )

                Delete ->
                    ( HttpBuilder.delete, apiUrl ++ collection ++ "/" ++ id )
    in
        ( url
        , requestBuilder url
            |> withHeaders [ ( "X-CSRF-Token", token ) ]
            |> withJsonBody value
            |> withExpect (Http.expectJson decoder)
            |> withCredentials
            |> toRequest
        )


multipartRequest : AuthToken -> String -> List Http.Part -> Decode.Decoder a -> ( String, Http.Request a )
multipartRequest token collection parts decoder =
    let
        url =
            apiUrl ++ collection

        request =
            Http.request
                { method = "POST"
                , url = apiUrl ++ collection
                , headers = [ Http.header "X-CSRF-Token" token ]
                , body = Http.multipartBody <| parts
                , expect = (Http.expectJson decoder)
                , timeout = Nothing
                , withCredentials = True
                }
    in
        ( url, request )


requester : AuthToken -> String -> String -> HttpMethod -> Encode.Value -> Decode.Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
requester token baseurl urlid method value decoder msg =
    let
        ( _, req ) =
            request token baseurl urlid method value decoder
    in
        req
            |> Http.send msg


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


errorCmd : WebData a -> Cmd msg
errorCmd webdata =
    let
        codeToCmd code =
            if code == 401 then
                Navigation.newUrl "#Login"
            else
                Cmd.none

        handleError err =
            case err of
                BadStatus response ->
                    codeToCmd response.status.code

                _ ->
                    Cmd.none
    in
        case webdata of
            Failure err ->
                handleError err

            _ ->
                Cmd.none
