module Staffs.Models exposing (..)

import Staffs.Staff exposing (..)
import Helpers.Models exposing (..)
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Staffs.Actions.Models as Actions
import Ui.DropdownMenu


type Msg
    = ActionMenu Ui.DropdownMenu.Msg
    | CloseActionMenu
    | NoAction
      -- MODALS
    | OpenModal AuthToken Actions.ModalType
    | ActionsMsg Actions.Msg


type alias Model =
    { id : NodeId
    , access : StaffAccess
    , staff : Staff
    , actionMenu : Ui.DropdownMenu.Model
    , actions : Actions.Model
    }


type alias StaffAccess =
    { name : AccessType
    , image : AccessType
    , address : AccessType
    , contact : AccessType
    }


modelDecoder : Decode.Decoder Model
modelDecoder =
    decode Model
        |> required "id" Decode.string
        |> required "access" staffAccessDecoder
        |> required "values" staffDecoder
        |> hardcoded Ui.DropdownMenu.init
        |> hardcoded Actions.init


staffAccessDecoder : Decode.Decoder StaffAccess
staffAccessDecoder =
    decode createStaffAccess
        |> required "name" Decode.string
        |> required "image" Decode.string
        |> required "address" Decode.string
        |> required "contact" Decode.string


createStaffAccess : String -> String -> String -> String -> StaffAccess
createStaffAccess name image address contact =
    StaffAccess
        (convertAccessType name)
        (convertAccessType image)
        (convertAccessType address)
        (convertAccessType contact)


initStaff : NodeId -> Staff
initStaff entityId =
    Staff entityId
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
