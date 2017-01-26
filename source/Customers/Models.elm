module Customers.Models exposing (..)

import Customers.Customer exposing (..)
import Helpers.Models exposing (..)
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Customers.Actions.Models as Actions
import Ui.DropdownMenu


type Msg
    = ActionMenu Ui.DropdownMenu.Msg
    | CloseActionMenu
    | NoAction
      -- MODALS
    | ModalAction AuthToken Actions.ModalType
    | ActionsMsg Actions.Msg


type alias Model =
    { id : NodeId
    , access : CustomerAccess
    , customer : Customer
    , actionMenu : Ui.DropdownMenu.Model
    , actions : Actions.Model
    }


type alias CustomerAccess =
    { name : AccessType
    , image : AccessType
    , address : AccessType
    , contact : AccessType
    }


modelDecoder : Decode.Decoder Model
modelDecoder =
    decode Model
        |> required "id" Decode.string
        |> required "access" customerAccessDecoder
        |> required "values" customerDecoder
        |> hardcoded Ui.DropdownMenu.init
        |> hardcoded Actions.init


customerAccessDecoder : Decode.Decoder CustomerAccess
customerAccessDecoder =
    decode createCustomerAccess
        |> required "name" Decode.string
        |> required "image" Decode.string
        |> required "address" Decode.string
        |> required "contact" Decode.string


createCustomerAccess : String -> String -> String -> String -> CustomerAccess
createCustomerAccess name image address contact =
    CustomerAccess
        (convertAccessType name)
        (convertAccessType image)
        (convertAccessType address)
        (convertAccessType contact)


initCustomer : NodeId -> Customer
initCustomer entityId =
    Customer entityId Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
