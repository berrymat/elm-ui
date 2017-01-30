module Clients.Models exposing (..)

import Clients.Client exposing (..)
import Helpers.Models exposing (..)
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Clients.Actions.Models as Actions
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
    , access : ClientAccess
    , client : Client
    , actionMenu : Ui.DropdownMenu.Model
    , actions : Actions.Model
    }


type alias ClientAccess =
    { name : AccessType
    , image : AccessType
    , address : AccessType
    , contact : AccessType
    }


modelDecoder : Decode.Decoder Model
modelDecoder =
    decode Model
        |> required "id" Decode.string
        |> required "access" clientAccessDecoder
        |> required "values" clientDecoder
        |> hardcoded Ui.DropdownMenu.init
        |> hardcoded Actions.init


clientAccessDecoder : Decode.Decoder ClientAccess
clientAccessDecoder =
    decode createClientAccess
        |> required "name" Decode.string
        |> required "image" Decode.string
        |> required "address" Decode.string
        |> required "contact" Decode.string


createClientAccess : String -> String -> String -> String -> ClientAccess
createClientAccess name image address contact =
    ClientAccess
        (convertAccessType name)
        (convertAccessType image)
        (convertAccessType address)
        (convertAccessType contact)


initClient : NodeId -> Client
initClient entityId =
    Client entityId Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
