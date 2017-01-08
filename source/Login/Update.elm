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
import Header.Commands exposing (entityDecoder)
import Helpers.Models exposing (..)
import Tree.Models exposing (convertNodeType)
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

        GotoHome ->
            updateGotoHome login


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
        |> required "childtypes" (Decode.list entityDecoder)


makeAuthResult : String -> String -> String -> List Entity -> AuthResult
makeAuthResult resultTypeString resultId result childtypes =
    let
        resultType =
            Maybe.withDefault RootType (convertNodeType resultTypeString)
    in
        AuthResult
            resultType
            resultId
            result
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
