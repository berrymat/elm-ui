module Issues.Models exposing (..)

import Helpers.Models exposing (..)
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Table
import Ui.DropdownMenu
import Issues.Actions.Models as Actions
import Issues.Issue exposing (..)


type Msg
    = ToggleIssue NodeId
    | SetQuery String
    | SetTableState Table.State
      -- ACTION MENU
    | ActionMenu Ui.DropdownMenu.Msg
    | CloseActionMenu
    | NoAction
      -- MODALS
    | OpenModal AuthToken Actions.ModalType
    | ActionsMsg Actions.Msg


type alias Model =
    { id : NodeId
    , canAdd : Bool
    , canEdit : Bool
    , canDelete : Bool
    , sites : List IssueSite
    , issues : List Issue
    , tableState : Table.State
    , query : String
    , actionMenu : Ui.DropdownMenu.Model
    , actions : Actions.Model
    }


initIssue : NodeId -> Issue
initIssue entityId =
    Issue entityId "" "" 0.0 0.0 "" "" "" "" [] "" Nothing False


modelDecoder : Decode.Decoder Model
modelDecoder =
    decode Model
        |> required "id" Decode.string
        |> required "canAdd" Decode.bool
        |> required "canEdit" Decode.bool
        |> required "canDelete" Decode.bool
        |> required "sites" (Decode.list issueSiteDecoder)
        |> required "issues" (Decode.list issueDecoder)
        |> hardcoded (Table.initialSort "name")
        |> hardcoded ""
        |> hardcoded Ui.DropdownMenu.init
        |> hardcoded Actions.init
