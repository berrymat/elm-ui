module Header.Update exposing (..)

import Header.Models exposing (..)
import Helpers.Helpers exposing (..)
import Return exposing (..)
import Roots.Models
import Customers.Models
import Clients.Models
import Sites.Models
import Staffs.Models
import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Roots.Update
import Customers.Update
import Clients.Update
import Sites.Update
import Staffs.Update


update : Msg -> Model -> Return Msg Model
update msg model =
    case ( msg, model.header ) of
        ( RootsMsg rootsMsg, RootHeader root ) ->
            Roots.Update.update rootsMsg root
                |> Return.mapBoth RootsMsg (\r -> { model | header = RootHeader r })

        ( CustomersMsg customersMsg, CustomerHeader customer ) ->
            Customers.Update.update customersMsg customer
                |> Return.mapBoth CustomersMsg (\r -> { model | header = CustomerHeader r })

        ( ClientsMsg clientsMsg, ClientHeader client ) ->
            Clients.Update.update clientsMsg client
                |> Return.mapBoth ClientsMsg (\r -> { model | header = ClientHeader r })

        ( SitesMsg sitesMsg, SiteHeader site ) ->
            Sites.Update.update sitesMsg site
                |> Return.mapBoth SitesMsg (\r -> { model | header = SiteHeader r })

        ( StaffsMsg staffsMsg, StaffHeader staff ) ->
            Staffs.Update.update staffsMsg staff
                |> Return.mapBoth StaffsMsg (\r -> { model | header = StaffHeader r })

        x ->
            let
                _ =
                    Debug.log "Stray found" x
            in
                singleton model


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.header of
        RootHeader root ->
            Sub.map RootsMsg (Roots.Update.subscriptions root)

        CustomerHeader customer ->
            Sub.map CustomersMsg (Customers.Update.subscriptions customer)

        ClientHeader client ->
            Sub.map ClientsMsg (Clients.Update.subscriptions client)

        SiteHeader site ->
            Sub.map SitesMsg (Sites.Update.subscriptions site)

        StaffHeader staff ->
            Sub.map StaffsMsg (Staffs.Update.subscriptions staff)

        Empty ->
            Sub.none



-- COMMANDS
{-
   saveHeader : AuthToken -> NodeId -> Model -> Cmd Msg
   saveHeader token nodeId model =
       case model.header of
           RootHeader root ->
               saveRoot token nodeId root

           CustomerHeader customer ->
               saveCustomer token nodeId customer

           ClientHeader client ->
               saveClient token nodeId client

           SiteHeader site ->
               saveSite token nodeId site

           StaffHeader staff ->
               saveStaff token nodeId staff

           Header.Models.Empty ->
               Cmd.none


   saveRoot : AuthToken -> NodeId -> Root -> Cmd Msg
   saveRoot token nodeId root =
       putter token "Roots" nodeId (encodeRoot root) (modelDecoder rootDecoder) (HeaderSaveResponse << RemoteData.fromResult)


   saveCustomer : AuthToken -> NodeId -> Customer -> Cmd Msg
   saveCustomer token nodeId customer =
       putter token "Customers" nodeId (encodeCustomer customer) (modelDecoder customerDecoder) (HeaderSaveResponse << RemoteData.fromResult)


   saveClient : AuthToken -> NodeId -> Client -> Cmd Msg
   saveClient token nodeId client =
       putter token "Clients" nodeId (encodeClient client) (modelDecoder clientDecoder) (HeaderSaveResponse << RemoteData.fromResult)


   saveSite : AuthToken -> NodeId -> Site -> Cmd Msg
   saveSite token nodeId site =
       putter token "Sites" nodeId (encodeSite site) (modelDecoder siteDecoder) (HeaderSaveResponse << RemoteData.fromResult)


   saveStaff : AuthToken -> NodeId -> Staff -> Cmd Msg
   saveStaff token nodeId staff =
       putter token "Staff" nodeId (encodeStaff staff) (modelDecoder staffDecoder) (HeaderSaveResponse << RemoteData.fromResult)

-}


modelDecoder : Decode.Decoder Header -> Decode.Decoder Model
modelDecoder headerDecoder =
    decode Model
        |> required "header" headerDecoder
        |> required "tabs" (Decode.list tabDecoder)
        |> required "childtypes" (Decode.list entityDecoder)
        |> required "useraccess" useraccessDecoder


rootDecoder : Decode.Decoder Header
rootDecoder =
    Decode.map RootHeader Roots.Models.modelDecoder


customerDecoder : Decode.Decoder Header
customerDecoder =
    Decode.map CustomerHeader Customers.Models.modelDecoder


clientDecoder : Decode.Decoder Header
clientDecoder =
    Decode.map ClientHeader Clients.Models.modelDecoder


siteDecoder : Decode.Decoder Header
siteDecoder =
    Decode.map SiteHeader Sites.Models.modelDecoder


staffDecoder : Decode.Decoder Header
staffDecoder =
    Decode.map StaffHeader Staffs.Models.modelDecoder
