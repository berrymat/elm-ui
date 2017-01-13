module Users.Restrict.Models exposing (..)

import Helpers.Models exposing (..)
import Table
import Ui.Modal
import RemoteData exposing (..)
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import Users.Manager.User exposing (..)
import Return exposing (..)
import Helpers.Helpers exposing (..)


type Msg
    = Save AuthToken
    | Cancel
    | RestrictResponse (WebData (List Restriction))
    | SaveResponse (WebData User)
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
    { id : NodeId
    , modal : Ui.Modal.Model
    , restrictions : WebData (List Restriction)
    , tableState : Table.State
    , query : String
    , response : WebData User
    }


init : User -> Return Msg Model
init user =
    ( { id = user.id
      , modal = Ui.Modal.open Ui.Modal.init
      , restrictions = Loading
      , tableState = Table.initialSort "Email"
      , query = ""
      , response = NotAsked
      }
    , fetcher "Restrictions"
        user.id
        restrictionsDecoder
        (RestrictResponse << RemoteData.fromResult)
    )


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
