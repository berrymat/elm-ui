module Login.Update exposing (..)

import Login.Models exposing (..)
import Return exposing (..)
import Ui.Modal
import Components.Form as Form
import HttpBuilder exposing (..)
import Http
import RemoteData exposing (..)
import Erl
import Helpers.Helpers exposing (..)
import Json.Decode as Decode exposing (field)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Helpers.Models exposing (..)
import Navigation


update : Msg -> Login -> Return Msg Login
update msg login =
    let
        errorCmd =
            case msg of
                AuthenticateResponse response ->
                    Helpers.Helpers.errorCmd response

                _ ->
                    Cmd.none

        return =
            updateInner msg login
    in
        (return |> Return.command errorCmd)


updateInner : Msg -> Login -> Return Msg Login
updateInner msg login =
    case msg of
        LoginFormMsg formMsg ->
            Form.update formMsg login.loginForm
                |> Return.map (\nf -> { login | loginForm = nf })
                |> mapCmd LoginFormMsg

        LoginModalMsg modalMsg ->
            ( { login | loginModal = Ui.Modal.update modalMsg login.loginModal }
            , Cmd.none
            )

        OpenLoginModal ->
            updateOpenLoginModal login

        SaveLoginModal ->
            updateSaveLoginModal login

        CancelLoginModal ->
            singleton login

        AuthenticateResponse response ->
            updateAuthenticateResponse login response

        TokenResponse parentType nodeId childType response ->
            updateTokenResponse login parentType nodeId childType response

        GotoHome ->
            updateGotoHome login

        LoadToken parentType nodeId childType ->
            updateLoadToken login parentType nodeId childType


updateOpenLoginModal : Login -> Return Msg Login
updateOpenLoginModal login =
    let
        newLoginModal =
            Ui.Modal.open login.loginModal
    in
        ( { login | loginModal = newLoginModal }, Cmd.none )


updateSaveLoginModal : Login -> Return Msg Login
updateSaveLoginModal login =
    let
        email =
            Form.valueOfInput "Email" "" login.loginForm

        password =
            Form.valueOfInput "Password" "" login.loginForm

        remember =
            Form.valueOfCheckbox rememberMe False login.loginForm
    in
        ( login, authenticate email password remember )


authenticate : String -> String -> Bool -> Cmd Msg
authenticate username password rememberMe =
    HttpBuilder.get (authenticateUrl username password rememberMe)
        |> withExpect (Http.expectJson authenticateDecoder)
        |> withCredentials
        |> send (AuthenticateResponse << RemoteData.fromResult)


authenticateUrl : String -> String -> Bool -> String
authenticateUrl username password rememberMe =
    let
        erl =
            Erl.parse (apiUrl ++ "Login")
                |> Erl.addQuery "username" username
                |> Erl.addQuery "password" password
                |> Erl.addQuery "rememberMe" (toString rememberMe)
    in
        Erl.toString erl


authenticateDecoder : Decode.Decoder AuthResult
authenticateDecoder =
    decode makeAuthResult
        |> required "type" Decode.string
        |> required "id" Decode.string
        |> required "result" Decode.string
        |> required "authToken" Decode.string
        |> required "childtypes" (Decode.list entityDecoder)


makeAuthResult : String -> String -> String -> AuthToken -> List Entity -> AuthResult
makeAuthResult resultTypeString resultId result token childtypes =
    let
        resultType =
            Maybe.withDefault RootType (convertNodeType resultTypeString)
    in
        AuthResult
            resultType
            resultId
            result
            token
            childtypes


maybeAuthResultTypes : AuthResult -> Maybe ( NodeType, NodeId, NodeType )
maybeAuthResultTypes authResult =
    if authResult.result == "OK" then
        List.head authResult.childtypes
            |> Maybe.map (\r -> ( authResult.nodeType, authResult.nodeId, r.nodeType ))
    else
        Nothing


updateAuthenticateResponse : Login -> WebData AuthResult -> Return Msg Login
updateAuthenticateResponse login authResult =
    RemoteData.map (updateAuthenticateResponseSuccess login) authResult
        |> RemoteData.withDefault (singleton login)


updateAuthenticateResponseSuccess : Login -> AuthResult -> Return Msg Login
updateAuthenticateResponseSuccess login authResult =
    let
        _ =
            Debug.log "updateAuthenticateResponseSuccess" ( login, authResult )

        maybeTypes =
            maybeAuthResultTypes authResult
    in
        case maybeTypes of
            Just ( parentType, nodeId, childType ) ->
                let
                    path =
                        nodeTypeToPath childType
                in
                    singleton authResult
                        |> Return.map (\new -> { login | authResult = Success new })
                        |> Return.command (Navigation.newUrl ("#" ++ path ++ "/" ++ nodeId))

            Nothing ->
                singleton login


fetchAuthResult : Cmd Msg
fetchAuthResult =
    HttpBuilder.get (apiUrl ++ "User")
        |> withExpect (Http.expectJson authenticateDecoder)
        |> withCredentials
        |> send (AuthenticateResponse << RemoteData.fromResult)


updateTokenResponse : Login -> NodeType -> NodeId -> NodeType -> WebData AuthResult -> Return Msg Login
updateTokenResponse login parentType nodeId childType authResult =
    RemoteData.map (updateTokenResponseSuccess login parentType nodeId childType) authResult
        |> RemoteData.withDefault (singleton login)


updateTokenResponseSuccess : Login -> NodeType -> NodeId -> NodeType -> AuthResult -> Return Msg Login
updateTokenResponseSuccess login parentType nodeId childType authResult =
    let
        path =
            nodeTypeToPath childType
    in
        singleton authResult
            |> Return.map (\new -> { login | authResult = Success new })
            |> Return.command (Navigation.newUrl ("#" ++ path ++ "/" ++ nodeId))


fetchAuthToken : NodeType -> NodeId -> NodeType -> Cmd Msg
fetchAuthToken parentType nodeId childType =
    HttpBuilder.get (apiUrl ++ "User")
        |> withExpect (Http.expectJson authenticateDecoder)
        |> withCredentials
        |> send ((TokenResponse parentType nodeId childType) << RemoteData.fromResult)


updateGotoHome : Login -> Return Msg Login
updateGotoHome login =
    case login.authResult of
        NotAsked ->
            singleton login
                |> command fetchAuthResult

        Loading ->
            singleton login

        Failure error ->
            singleton login

        Success result ->
            updateAuthenticateResponseSuccess login result


updateLoadToken : Login -> NodeType -> NodeId -> NodeType -> Return Msg Login
updateLoadToken login parentType nodeId childType =
    case login.authResult of
        NotAsked ->
            singleton login
                |> command (fetchAuthToken parentType nodeId childType)

        Loading ->
            singleton login

        Failure error ->
            singleton login

        Success result ->
            singleton result
                |> Return.map (\new -> { login | authResult = Success new })
