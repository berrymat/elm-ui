module Header.Update exposing (..)

import Header.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Return exposing (..)
import Ui.DropdownMenu
import Ui.Modal
import Components.Form as Form
import Header.Root.Models exposing (..)
import Header.Customer.Models exposing (..)
import Header.Client.Models exposing (..)
import Header.Site.Models exposing (..)
import Header.Staff.Models exposing (..)
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import RemoteData exposing (..)


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        -- ACTION MENU
        ActionMenu action ->
            updateActionMenu model action

        CloseActionMenu ->
            updateCloseActionMenu model

        NoAction ->
            singleton model

        -- EDIT MODAL
        ModalAction token EditHeader action ->
            updateEditModalAction token model action

        ModalMsg EditHeader msg ->
            updateEditModalMsg model msg

        -- FORM
        EditFormMsg msg ->
            updateEditFormMsg model msg

        HeaderSaveResponse webdata ->
            handleWebDataResponse model webdata "Changes saved" singleton


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subActionMenu =
            Sub.map ActionMenu (Ui.DropdownMenu.subscriptions model.actionMenu)
    in
        Sub.batch
            [ subActionMenu
            ]



-- ACTION MENU UPDATES


applyNewActionMenu : Model -> Ui.DropdownMenu.Model -> Return Msg Model
applyNewActionMenu model newMenu =
    ( { model | actionMenu = newMenu }, Cmd.none )


updateActionMenu : Model -> Ui.DropdownMenu.Msg -> Return Msg Model
updateActionMenu model action =
    let
        newActionMenu =
            Ui.DropdownMenu.update action model.actionMenu
    in
        applyNewActionMenu model newActionMenu


updateCloseActionMenu : Model -> Return Msg Model
updateCloseActionMenu model =
    let
        newActionMenu =
            Ui.DropdownMenu.close model.actionMenu
    in
        applyNewActionMenu model newActionMenu



-- EDIT MODAL UPDATES


updateEditModalAction : AuthToken -> Model -> ModalAction -> Return Msg Model
updateEditModalAction token model action =
    case action of
        Open ->
            updateEditModalOpen model

        Save ->
            case model.editForm of
                Just form ->
                    updateEditModalSave token model form

                Nothing ->
                    singleton model

        Cancel ->
            ( { model | editModal = Ui.Modal.close model.editModal }, Cmd.none )


updateEditModalOpen : Model -> Return Msg Model
updateEditModalOpen model =
    ( { model
        | actionMenu = (Ui.DropdownMenu.close model.actionMenu)
        , editModal = Ui.Modal.open model.editModal
        , editForm = Header.Models.initEditForm model
      }
    , Cmd.none
    )


updateEditModalSave : AuthToken -> Model -> Form.Model Msg -> Return Msg Model
updateEditModalSave token model form =
    let
        nodeId =
            Header.Models.headerId model

        newModel =
            Header.Models.updateState form model

        newEffect =
            saveHeader token nodeId newModel
    in
        ( newModel, newEffect )


updateEditModalMsg : Model -> Ui.Modal.Msg -> Return Msg Model
updateEditModalMsg model msg =
    let
        newEditModal =
            Ui.Modal.update msg model.editModal
    in
        ( { model | editModal = newEditModal }, Cmd.none )



-- EDIT FORM UPDATES


updateEditFormMsg : Model -> Form.Msg -> Return Msg Model
updateEditFormMsg model msg =
    let
        ( newEditModal, effect ) =
            maybeUpdate (Form.update msg) model.editForm
    in
        ( { model | editForm = newEditModal }
        , Cmd.map EditFormMsg effect
        )



-- COMMANDS


saveHeader : AuthToken -> NodeId -> Model -> Cmd Msg
saveHeader token nodeId model =
    case model.header of
        RootHeader root ->
            saveRoot token nodeId root

        CustomerHeader customer ->
            saveCustomer token nodeId customer

        ClientHeader client ->
            saveClient token nodeId client

        SiteHeader site ->
            saveSite token nodeId site

        StaffHeader staff ->
            saveStaff token nodeId staff

        Header.Models.Empty ->
            Cmd.none


saveRoot : AuthToken -> NodeId -> Root -> Cmd Msg
saveRoot token nodeId root =
    putter token "Roots" nodeId (encodeRoot root) (modelDecoder rootDecoder) (HeaderSaveResponse << RemoteData.fromResult)


saveCustomer : AuthToken -> NodeId -> Customer -> Cmd Msg
saveCustomer token nodeId customer =
    putter token "Customers" nodeId (encodeCustomer customer) (modelDecoder customerDecoder) (HeaderSaveResponse << RemoteData.fromResult)


saveClient : AuthToken -> NodeId -> Client -> Cmd Msg
saveClient token nodeId client =
    putter token "Clients" nodeId (encodeClient client) (modelDecoder clientDecoder) (HeaderSaveResponse << RemoteData.fromResult)


saveSite : AuthToken -> NodeId -> Site -> Cmd Msg
saveSite token nodeId site =
    putter token "Sites" nodeId (encodeSite site) (modelDecoder siteDecoder) (HeaderSaveResponse << RemoteData.fromResult)


saveStaff : AuthToken -> NodeId -> Staff -> Cmd Msg
saveStaff token nodeId staff =
    putter token "Staff" nodeId (encodeStaff staff) (modelDecoder staffDecoder) (HeaderSaveResponse << RemoteData.fromResult)


modelDecoder : Decode.Decoder Header -> Decode.Decoder Model
modelDecoder headerDecoder =
    decode Model
        |> required "header" headerDecoder
        |> required "tabs" (Decode.list tabDecoder)
        |> required "childtypes" (Decode.list entityDecoder)
        |> required "useraccess" useraccessDecoder
        |> hardcoded Ui.DropdownMenu.init
        |> hardcoded Ui.Modal.init
        |> hardcoded Nothing


rootDecoder : Decode.Decoder Header
rootDecoder =
    Decode.map RootHeader
        (decode Root
            |> required "id" Decode.string
            |> required "access" rootAccessDecoder
            |> required "values" rootValuesDecoder
        )


customerDecoder : Decode.Decoder Header
customerDecoder =
    Decode.map CustomerHeader
        (decode Customer
            |> required "id" Decode.string
            |> required "access" customerAccessDecoder
            |> required "values" customerValuesDecoder
        )


clientDecoder : Decode.Decoder Header
clientDecoder =
    Decode.map ClientHeader
        (decode Client
            |> required "id" Decode.string
            |> required "access" clientAccessDecoder
            |> required "values" clientValuesDecoder
        )


siteDecoder : Decode.Decoder Header
siteDecoder =
    Decode.map SiteHeader
        (decode Site
            |> required "id" Decode.string
            |> required "access" siteAccessDecoder
            |> required "values" siteValuesDecoder
        )


staffDecoder : Decode.Decoder Header
staffDecoder =
    Decode.map StaffHeader
        (decode Staff
            |> required "id" Decode.string
            |> required "access" staffAccessDecoder
            |> required "values" staffValuesDecoder
        )
