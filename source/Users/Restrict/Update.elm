module Users.Restrict.Update exposing (..)

import Users.Restrict.Models exposing (..)
import Helpers.Helpers exposing (..)
import Helpers.Models exposing (..)
import Return exposing (..)
import RemoteData exposing (..)
import Table
import Ui.Modal


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        Open nodeId ->
            updateOpen model nodeId

        Save authToken ->
            updateSave model authToken

        Cancel ->
            updateCancel model

        RestrictResponse response ->
            updateRestrictResponse model response

        SaveResponse response ->
            updateSaveResponse model response

        ToggleRestriction nodeId ->
            updateToggleRestriction model nodeId

        SetQuery query ->
            updateSetQuery model query

        SetTableState tableState ->
            updateSetTableState model tableState

        ModalMsg modalMsg ->
            updateModalMsg model modalMsg


updateOpen : Model -> NodeId -> Return Msg Model
updateOpen model nodeId =
    let
        cmdFetch =
            fetcher "Restrictions"
                nodeId
                restrictionsDecoder
                (RestrictResponse << RemoteData.fromResult)
    in
        ( { model
            | restrictions = Loading
            , nodeId = nodeId
            , modal = Ui.Modal.open model.modal
          }
        , cmdFetch
        )


updateSave : Model -> AuthToken -> Return Msg Model
updateSave model authToken =
    let
        saveRestrictions authToken restrictions =
            ( restrictions
            , putter authToken
                "Restrictions"
                model.nodeId
                (encodeRestrictions restrictions)
                restrictionsDecoder
                (SaveResponse << RemoteData.fromResult)
            )

        ( newRestrictions, saveCmd ) =
            RemoteData.update (saveRestrictions authToken) model.restrictions
    in
        ( { model | restrictions = newRestrictions }, saveCmd )


updateCancel : Model -> Return Msg Model
updateCancel model =
    ( { model | modal = Ui.Modal.close model.modal }, Cmd.none )


updateRestrictResponse : Model -> WebData (List Restriction) -> Return Msg Model
updateRestrictResponse model response =
    ( { model | restrictions = response }, Cmd.none )


updateSetQuery : Model -> String -> Return Msg Model
updateSetQuery model query =
    ( { model | query = query }, Cmd.none )


updateSetTableState : Model -> Table.State -> Return Msg Model
updateSetTableState model tableState =
    ( { model | tableState = tableState }, Cmd.none )


updateModalMsg : Model -> Ui.Modal.Msg -> Return Msg Model
updateModalMsg model modalMsg =
    ( { model | modal = Ui.Modal.update modalMsg model.modal }, Cmd.none )


updateSaveResponse : Model -> WebData (List Restriction) -> Return Msg Model
updateSaveResponse model response =
    let
        newModal =
            RemoteData.map (\r -> Ui.Modal.close model.modal) response
                |> RemoteData.withDefault model.modal
    in
        ( { model | restrictions = response, modal = newModal }, Cmd.none )


updateToggleRestriction : Model -> NodeId -> Return Msg Model
updateToggleRestriction model nodeId =
    let
        toggleRestrictions restrictions =
            List.map
                (\r ->
                    if r.id == nodeId then
                        { r | selected = not r.selected }
                    else
                        r
                )
                restrictions

        newRestrictions =
            RemoteData.map toggleRestrictions model.restrictions
    in
        ( { model | restrictions = newRestrictions }, Cmd.none )
