module Header.Models exposing (..)

import Helpers.Models exposing (..)
import RemoteData exposing (..)
import Ui.DropdownMenu
import Ui.Modal
import Components.Form as Form
import Header.Root.Models
import Header.Customer.Models
import Header.Client.Models
import Header.Site.Models
import Header.Staff.Models


type ModalType
    = EditHeader


type ModalAction
    = Open
    | Save
    | Cancel


type Msg
    = ActionMenu Ui.DropdownMenu.Msg
    | CloseActionMenu
    | NoAction
      -- MODALS
    | ModalAction AuthToken ModalType ModalAction
    | ModalMsg ModalType Ui.Modal.Msg
      -- EDIT FORM
    | EditFormMsg Form.Msg
    | HeaderSaveResponse (WebData Model)


type alias Model =
    { header : Header
    , tabs : List Tab
    , childtypes : List Entity
    , useraccess : UserAccess
    , actionMenu : Ui.DropdownMenu.Model
    , editModal : Ui.Modal.Model
    , editForm : Maybe (Form.Model Msg)
    }


type Header
    = RootHeader Header.Root.Models.Root
    | CustomerHeader Header.Customer.Models.Customer
    | ClientHeader Header.Client.Models.Client
    | SiteHeader Header.Site.Models.Site
    | StaffHeader Header.Staff.Models.Staff
    | Empty


headerId : Model -> NodeId
headerId model =
    case model.header of
        RootHeader root ->
            root.id

        CustomerHeader customer ->
            customer.id

        ClientHeader client ->
            client.id

        SiteHeader site ->
            site.id

        StaffHeader staff ->
            staff.id

        Empty ->
            ""


getTabFromType : TabType -> Model -> Tab
getTabFromType tabType model =
    let
        firstTab =
            model.tabs
                |> List.head
                |> Maybe.withDefault (Tab EmptyTab "")
    in
        model.tabs
            |> List.filter (\t -> t.tabType == tabType)
            |> List.head
            |> Maybe.withDefault firstTab


initEditForm : Model -> Maybe (Form.Model Msg)
initEditForm model =
    case model.header of
        RootHeader root ->
            Just (Header.Root.Models.initEditForm root)

        CustomerHeader customer ->
            Just (Header.Customer.Models.initEditForm customer)

        ClientHeader client ->
            Just (Header.Client.Models.initEditForm client)

        SiteHeader site ->
            Just (Header.Site.Models.initEditForm site)

        StaffHeader staff ->
            Just (Header.Staff.Models.initEditForm staff)

        Empty ->
            Nothing


updateState : Form.Model Msg -> Model -> Model
updateState form model =
    case model.header of
        RootHeader root ->
            { model
                | header = RootHeader (Header.Root.Models.updateState form root)
            }

        CustomerHeader customer ->
            { model
                | header = CustomerHeader (Header.Customer.Models.updateState form customer)
            }

        ClientHeader client ->
            { model
                | header = ClientHeader (Header.Client.Models.updateState form client)
            }

        SiteHeader site ->
            { model
                | header = SiteHeader (Header.Site.Models.updateState form site)
            }

        StaffHeader staff ->
            { model
                | header = StaffHeader (Header.Staff.Models.updateState form staff)
            }

        Empty ->
            model
