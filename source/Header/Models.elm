module Header.Models exposing (..)

import Helpers.Models exposing (..)
import RemoteData exposing (..)
import Ui.DropdownMenu
import Ui.Modal
import Components.Form as Form
import Roots.Models
import Customers.Models
import Clients.Models
import Sites.Models
import Staffs.Models


type ModalType
    = EditHeader


type ModalAction
    = Open
    | Save
    | Cancel


type Msg
    = RootsMsg Roots.Models.Msg
    | CustomersMsg Customers.Models.Msg
    | ClientsMsg Clients.Models.Msg
    | SitesMsg Sites.Models.Msg
    | StaffsMsg Staffs.Models.Msg


type alias Model =
    { header : Header
    , tabs : List Tab
    , childtypes : List Entity
    , useraccess : UserAccess
    }


type Header
    = RootHeader Roots.Models.Model
    | CustomerHeader Customers.Models.Customer
    | ClientHeader Clients.Models.Client
    | SiteHeader Sites.Models.Site
    | StaffHeader Staffs.Models.Staff
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



{-
   initEditForm : Model -> Maybe Form.Model
   initEditForm model =
       case model.header of
           RootHeader root ->
               Just (Roots.Models.initEditForm root)

           CustomerHeader customer ->
               Just (Customers.Models.initEditForm customer)

           ClientHeader client ->
               Just (Clients.Models.initEditForm client)

           SiteHeader site ->
               Just (Sites.Models.initEditForm site)

           StaffHeader staff ->
               Just (Staffs.Models.initEditForm staff)

           Empty ->
               Nothing


   updateState : Form.Model -> Model -> Model
   updateState form model =
       case model.header of
           RootHeader root ->
               { model
                   | header = RootHeader (Roots.Models.updateState form root)
               }

           CustomerHeader customer ->
               { model
                   | header = CustomerHeader (Customers.Models.updateState form customer)
               }

           ClientHeader client ->
               { model
                   | header = ClientHeader (Clients.Models.updateState form client)
               }

           SiteHeader site ->
               { model
                   | header = SiteHeader (Sites.Models.updateState form site)
               }

           StaffHeader staff ->
               { model
                   | header = StaffHeader (Staffs.Models.updateState form staff)
               }

           Empty ->
               model
-}
