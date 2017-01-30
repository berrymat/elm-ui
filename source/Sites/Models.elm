module Sites.Models exposing (..)

import Sites.Site exposing (..)
import Helpers.Models exposing (..)
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Sites.Actions.Models as Actions
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
    , access : SiteAccess
    , site : Site
    , actionMenu : Ui.DropdownMenu.Model
    , actions : Actions.Model
    }


type alias SiteAccess =
    { name : AccessType
    , image : AccessType
    , address : AccessType
    , contact : AccessType
    , managers : AccessType
    }


modelDecoder : Decode.Decoder Model
modelDecoder =
    decode Model
        |> required "id" Decode.string
        |> required "access" siteAccessDecoder
        |> required "values" siteDecoder
        |> hardcoded Ui.DropdownMenu.init
        |> hardcoded Actions.init


siteAccessDecoder : Decode.Decoder SiteAccess
siteAccessDecoder =
    decode createSiteAccess
        |> required "name" Decode.string
        |> required "image" Decode.string
        |> required "address" Decode.string
        |> required "contact" Decode.string
        |> required "managers" Decode.string


createSiteAccess : String -> String -> String -> String -> String -> SiteAccess
createSiteAccess name image address contact managers =
    SiteAccess
        (convertAccessType name)
        (convertAccessType image)
        (convertAccessType address)
        (convertAccessType contact)
        (convertAccessType managers)


initSite : NodeId -> Site
initSite entityId =
    Site entityId
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
        Nothing
        Nothing
        Nothing
