module Users.Restrict.Models exposing (..)

import Helpers.Models exposing (..)
import Table
import Ui.Modal
import RemoteData exposing (..)
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode


type Msg
    = Open NodeId
    | Save AuthToken
    | Cancel
    | RestrictResponse (WebData (List Restriction))
    | SaveResponse (WebData (List Restriction))
    | ToggleRestriction NodeId
    | SetQuery String
    | SetTableState Table.State
    | ModalMsg Ui.Modal.Msg


type alias Restriction =
    { id : NodeId
    , name : String
    , selected : Bool
    }


type alias Model =
    { modal : Ui.Modal.Model
    , nodeId : NodeId
    , restrictions : WebData (List Restriction)
    , tableState : Table.State
    , query : String
    }


init : Model
init =
    { modal = Ui.Modal.init
    , nodeId = ""
    , restrictions = NotAsked
    , tableState = Table.initialSort "Email"
    , query = ""
    }


restrictionsDecoder : Decode.Decoder (List Restriction)
restrictionsDecoder =
    Decode.list restrictionDecoder


restrictionDecoder : Decode.Decoder Restriction
restrictionDecoder =
    decode Restriction
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "selected" Decode.bool


encodeRestrictions : List Restriction -> Encode.Value
encodeRestrictions restrictions =
    Encode.list (List.map encodeRestriction restrictions)


encodeRestriction : Restriction -> Encode.Value
encodeRestriction restriction =
    Encode.object
        [ ( "id", Encode.string restriction.id )
        , ( "name", Encode.string restriction.name )
        , ( "selected", Encode.bool restriction.selected )
        ]
