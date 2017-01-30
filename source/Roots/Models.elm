module Roots.Models exposing (..)

import Roots.Root exposing (..)
import Helpers.Models exposing (..)
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Roots.Actions.Models as Actions
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
    , access : RootAccess
    , root : Root
    , actionMenu : Ui.DropdownMenu.Model
    , actions : Actions.Model
    }


type alias RootAccess =
    { name : AccessType
    , image : AccessType
    , address : AccessType
    , contact : AccessType
    }


modelDecoder : Decode.Decoder Model
modelDecoder =
    decode Model
        |> required "id" Decode.string
        |> required "access" rootAccessDecoder
        |> required "values" rootDecoder
        |> hardcoded Ui.DropdownMenu.init
        |> hardcoded Actions.init


rootAccessDecoder : Decode.Decoder RootAccess
rootAccessDecoder =
    decode createRootAccess
        |> required "name" Decode.string
        |> required "image" Decode.string
        |> required "address" Decode.string
        |> required "contact" Decode.string


createRootAccess : String -> String -> String -> String -> RootAccess
createRootAccess name image address contact =
    RootAccess
        (convertAccessType name)
        (convertAccessType image)
        (convertAccessType address)
        (convertAccessType contact)


initRoot : NodeId -> Root
initRoot entityId =
    Root entityId Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
