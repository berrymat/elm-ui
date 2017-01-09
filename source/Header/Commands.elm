module Header.Commands exposing (..)

import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import Container.Messages exposing (..)
import Container.Models exposing (..)
import Tree.Models exposing (..)
import Header.Models exposing (..)
import Helpers.Models exposing (..)
import Helpers.Helpers exposing (fetcher, putter)
import RemoteData exposing (..)


fetchHeader : Container -> ( NodeType, NodeId, Bool ) -> ( Container, Cmd Msg )
fetchHeader container ( nodeType, nodeId, isTree ) =
    let
        cmd =
            if nodeId /= "" && nodeId /= (headerId container.headerData) then
                case nodeType of
                    RootType ->
                        fetchRoot nodeId isTree

                    CustomerType ->
                        fetchCustomer nodeId isTree

                    ClientType ->
                        fetchClient nodeId isTree

                    SiteType ->
                        fetchSite nodeId isTree

                    StaffType ->
                        fetchStaff nodeId isTree

                    FolderType ->
                        Cmd.none
            else
                Cmd.none

        newData =
            if cmd /= Cmd.none then
                Loading
            else
                container.headerData
    in
        ( { container | headerData = newData }, cmd )


putHeader : AuthToken -> NodeId -> HeaderData -> Cmd Msg
putHeader token nodeId data =
    case data.header of
        RootHeader root ->
            putRoot token nodeId root

        CustomerHeader customer ->
            putCustomer token nodeId customer

        ClientHeader client ->
            putClient token nodeId client

        SiteHeader site ->
            putSite token nodeId site

        StaffHeader staff ->
            putStaff token nodeId staff

        Empty ->
            Cmd.none


fetchRoot : NodeId -> Bool -> Cmd Msg
fetchRoot nodeId isTree =
    fetcher "Roots" nodeId rootDecoder ((HeaderResponse isTree) << RemoteData.fromResult)


putRoot : AuthToken -> NodeId -> Root -> Cmd Msg
putRoot token nodeId root =
    putter token "Roots" nodeId (encodeRoot root) rootDecoder (HeaderPutResponse << RemoteData.fromResult)


fetchCustomer : NodeId -> Bool -> Cmd Msg
fetchCustomer nodeId isTree =
    fetcher "Customers" nodeId customerDecoder ((HeaderResponse isTree) << RemoteData.fromResult)


putCustomer : AuthToken -> NodeId -> Customer -> Cmd Msg
putCustomer token nodeId customer =
    putter token "Customers" nodeId (encodeCustomer customer) customerDecoder (HeaderPutResponse << RemoteData.fromResult)


fetchClient : NodeId -> Bool -> Cmd Msg
fetchClient nodeId isTree =
    fetcher "Clients" nodeId clientDecoder ((HeaderResponse isTree) << RemoteData.fromResult)


putClient : AuthToken -> NodeId -> Client -> Cmd Msg
putClient token nodeId client =
    putter token "Clients" nodeId (encodeClient client) clientDecoder (HeaderPutResponse << RemoteData.fromResult)


fetchSite : NodeId -> Bool -> Cmd Msg
fetchSite nodeId isTree =
    fetcher "Sites" nodeId siteDecoder ((HeaderResponse isTree) << RemoteData.fromResult)


putSite : AuthToken -> NodeId -> Site -> Cmd Msg
putSite token nodeId site =
    putter token "Sites" nodeId (encodeSite site) siteDecoder (HeaderPutResponse << RemoteData.fromResult)


fetchStaff : NodeId -> Bool -> Cmd Msg
fetchStaff nodeId isTree =
    fetcher "Staff" nodeId staffDecoder ((HeaderResponse isTree) << RemoteData.fromResult)


putStaff : AuthToken -> NodeId -> Staff -> Cmd Msg
putStaff token nodeId staff =
    putter token "Staff" nodeId (encodeStaff staff) staffDecoder (HeaderPutResponse << RemoteData.fromResult)



-- DECODERS


rootDecoder : Decode.Decoder HeaderData
rootDecoder =
    Decode.map4 HeaderData
        (Decode.map RootHeader
            (decode Root
                |> required "id" Decode.string
                |> required "access" rootAccessDecoder
                |> required "values" rootValuesDecoder
            )
        )
        (field "tabs" (Decode.list tabDecoder))
        (field "childtypes" (Decode.list entityDecoder))
        (field "useraccess" useraccessDecoder)


encodeRoot : Root -> Encode.Value
encodeRoot root =
    Encode.object
        [ ( "values"
          , Encode.object
                [ ( "name", Encode.string (Maybe.withDefault "" root.values.name) )
                , ( "address1", Encode.string (Maybe.withDefault "" root.values.address1) )
                , ( "address2", Encode.string (Maybe.withDefault "" root.values.address2) )
                , ( "address3", Encode.string (Maybe.withDefault "" root.values.address3) )
                , ( "address4", Encode.string (Maybe.withDefault "" root.values.address4) )
                , ( "postcode", Encode.string (Maybe.withDefault "" root.values.postcode) )
                , ( "contact", Encode.string (Maybe.withDefault "" root.values.contact) )
                , ( "tel", Encode.string (Maybe.withDefault "" root.values.tel) )
                , ( "email", Encode.string (Maybe.withDefault "" root.values.email) )
                ]
          )
        ]


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


rootValuesDecoder : Decode.Decoder RootValues
rootValuesDecoder =
    decode RootValues
        |> required "name" (Decode.nullable Decode.string)
        |> required "image" (Decode.nullable Decode.string)
        |> required "address1" (Decode.nullable Decode.string)
        |> required "address2" (Decode.nullable Decode.string)
        |> required "address3" (Decode.nullable Decode.string)
        |> required "address4" (Decode.nullable Decode.string)
        |> required "postcode" (Decode.nullable Decode.string)
        |> required "contact" (Decode.nullable Decode.string)
        |> required "tel" (Decode.nullable Decode.string)
        |> required "email" (Decode.nullable Decode.string)



-- Customer


customerDecoder : Decode.Decoder HeaderData
customerDecoder =
    Decode.map4 HeaderData
        (Decode.map CustomerHeader
            (decode customer
                |> required "id" Decode.string
                |> required "access" customerAccessDecoder
                |> required "values" customerValuesDecoder
            )
        )
        (field "tabs" (Decode.list tabDecoder))
        (field "childtypes" (Decode.list entityDecoder))
        (field "useraccess" useraccessDecoder)


encodeCustomer : Customer -> Encode.Value
encodeCustomer customer =
    Encode.object
        [ ( "values"
          , Encode.object
                [ ( "name", Encode.string (Maybe.withDefault "" customer.values.name) )
                , ( "address1", Encode.string (Maybe.withDefault "" customer.values.address1) )
                , ( "address2", Encode.string (Maybe.withDefault "" customer.values.address2) )
                , ( "address3", Encode.string (Maybe.withDefault "" customer.values.address3) )
                , ( "address4", Encode.string (Maybe.withDefault "" customer.values.address4) )
                , ( "postcode", Encode.string (Maybe.withDefault "" customer.values.postcode) )
                , ( "contact", Encode.string (Maybe.withDefault "" customer.values.contact) )
                , ( "tel", Encode.string (Maybe.withDefault "" customer.values.tel) )
                , ( "email", Encode.string (Maybe.withDefault "" customer.values.email) )
                ]
          )
        ]


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


customerValuesDecoder : Decode.Decoder CustomerValues
customerValuesDecoder =
    decode CustomerValues
        |> required "name" (Decode.nullable Decode.string)
        |> required "image" (Decode.nullable Decode.string)
        |> required "address1" (Decode.nullable Decode.string)
        |> required "address2" (Decode.nullable Decode.string)
        |> required "address3" (Decode.nullable Decode.string)
        |> required "address4" (Decode.nullable Decode.string)
        |> required "postcode" (Decode.nullable Decode.string)
        |> required "contact" (Decode.nullable Decode.string)
        |> required "tel" (Decode.nullable Decode.string)
        |> required "email" (Decode.nullable Decode.string)



-- Client


clientDecoder : Decode.Decoder HeaderData
clientDecoder =
    Decode.map4 HeaderData
        (Decode.map ClientHeader
            (decode Client
                |> required "id" Decode.string
                |> required "access" clientAccessDecoder
                |> required "values" clientValuesDecoder
            )
        )
        (field "tabs" (Decode.list tabDecoder))
        (field "childtypes" (Decode.list entityDecoder))
        (field "useraccess" useraccessDecoder)


encodeClient : Client -> Encode.Value
encodeClient client =
    Encode.object
        [ ( "values"
          , Encode.object
                [ ( "name", Encode.string (Maybe.withDefault "" client.values.name) )
                , ( "address1", Encode.string (Maybe.withDefault "" client.values.address1) )
                , ( "address2", Encode.string (Maybe.withDefault "" client.values.address2) )
                , ( "address3", Encode.string (Maybe.withDefault "" client.values.address3) )
                , ( "address4", Encode.string (Maybe.withDefault "" client.values.address4) )
                , ( "postcode", Encode.string (Maybe.withDefault "" client.values.postcode) )
                , ( "contact", Encode.string (Maybe.withDefault "" client.values.contact) )
                , ( "tel", Encode.string (Maybe.withDefault "" client.values.tel) )
                , ( "email", Encode.string (Maybe.withDefault "" client.values.email) )
                ]
          )
        ]


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


clientValuesDecoder : Decode.Decoder ClientValues
clientValuesDecoder =
    decode ClientValues
        |> required "no" (Decode.nullable Decode.string)
        |> required "name" (Decode.nullable Decode.string)
        |> required "image" (Decode.nullable Decode.string)
        |> required "address1" (Decode.nullable Decode.string)
        |> required "address2" (Decode.nullable Decode.string)
        |> required "address3" (Decode.nullable Decode.string)
        |> required "address4" (Decode.nullable Decode.string)
        |> required "postcode" (Decode.nullable Decode.string)
        |> required "contact" (Decode.nullable Decode.string)
        |> required "tel" (Decode.nullable Decode.string)
        |> required "email" (Decode.nullable Decode.string)



-- Site


siteDecoder : Decode.Decoder HeaderData
siteDecoder =
    Decode.map4 HeaderData
        (Decode.map SiteHeader
            (decode Site
                |> required "id" Decode.string
                |> required "access" siteAccessDecoder
                |> required "values" siteValuesDecoder
            )
        )
        (field "tabs" (Decode.list tabDecoder))
        (field "childtypes" (Decode.list entityDecoder))
        (field "useraccess" useraccessDecoder)


encodeSite : Site -> Encode.Value
encodeSite site =
    Encode.object
        [ ( "values"
          , Encode.object
                [ ( "name", Encode.string (Maybe.withDefault "" site.values.name) )
                , ( "address1", Encode.string (Maybe.withDefault "" site.values.address1) )
                , ( "address2", Encode.string (Maybe.withDefault "" site.values.address2) )
                , ( "address3", Encode.string (Maybe.withDefault "" site.values.address3) )
                , ( "address4", Encode.string (Maybe.withDefault "" site.values.address4) )
                , ( "postcode", Encode.string (Maybe.withDefault "" site.values.postcode) )
                , ( "contact", Encode.string (Maybe.withDefault "" site.values.contact) )
                , ( "tel", Encode.string (Maybe.withDefault "" site.values.tel) )
                , ( "email", Encode.string (Maybe.withDefault "" site.values.email) )
                ]
          )
        ]


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


siteValuesDecoder : Decode.Decoder SiteValues
siteValuesDecoder =
    decode SiteValues
        |> required "no" (Decode.nullable Decode.string)
        |> required "name" (Decode.nullable Decode.string)
        |> required "image" (Decode.nullable Decode.string)
        |> required "address1" (Decode.nullable Decode.string)
        |> required "address2" (Decode.nullable Decode.string)
        |> required "address3" (Decode.nullable Decode.string)
        |> required "address4" (Decode.nullable Decode.string)
        |> required "postcode" (Decode.nullable Decode.string)
        |> required "contact" (Decode.nullable Decode.string)
        |> required "tel" (Decode.nullable Decode.string)
        |> required "email" (Decode.nullable Decode.string)
        |> required "divisionMgr" (Decode.nullable Decode.string)
        |> required "areaMgr" (Decode.nullable Decode.string)
        |> required "supervisor" (Decode.nullable Decode.string)



-- Staff


staffDecoder : Decode.Decoder HeaderData
staffDecoder =
    Decode.map4 HeaderData
        (Decode.map StaffHeader
            (decode Staff
                |> required "id" Decode.string
                |> required "access" staffAccessDecoder
                |> required "values" staffValuesDecoder
            )
        )
        (field "tabs" (Decode.list tabDecoder))
        (field "childtypes" (Decode.list entityDecoder))
        (field "useraccess" useraccessDecoder)


encodeStaff : Staff -> Encode.Value
encodeStaff staff =
    Encode.object
        [ ( "values"
          , Encode.object
                [ ( "name", Encode.string (Maybe.withDefault "" staff.values.name) )
                , ( "address1", Encode.string (Maybe.withDefault "" staff.values.address1) )
                , ( "address2", Encode.string (Maybe.withDefault "" staff.values.address2) )
                , ( "address3", Encode.string (Maybe.withDefault "" staff.values.address3) )
                , ( "address4", Encode.string (Maybe.withDefault "" staff.values.address4) )
                , ( "postcode", Encode.string (Maybe.withDefault "" staff.values.postcode) )
                , ( "tel", Encode.string (Maybe.withDefault "" staff.values.tel) )
                , ( "mob", Encode.string (Maybe.withDefault "" staff.values.mob) )
                , ( "email", Encode.string (Maybe.withDefault "" staff.values.email) )
                ]
          )
        ]


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


staffValuesDecoder : Decode.Decoder StaffValues
staffValuesDecoder =
    decode StaffValues
        |> required "no" (Decode.nullable Decode.string)
        |> required "name" (Decode.nullable Decode.string)
        |> required "image" (Decode.nullable Decode.string)
        |> required "address1" (Decode.nullable Decode.string)
        |> required "address2" (Decode.nullable Decode.string)
        |> required "address3" (Decode.nullable Decode.string)
        |> required "address4" (Decode.nullable Decode.string)
        |> required "postcode" (Decode.nullable Decode.string)
        |> required "tel" (Decode.nullable Decode.string)
        |> required "mob" (Decode.nullable Decode.string)
        |> required "email" (Decode.nullable Decode.string)


createEntity : String -> String -> Entity
createEntity id name =
    Entity (convertNodeType id |> Maybe.withDefault RootType) name


createTab : String -> String -> Tab
createTab id name =
    if id == "folders" then
        Tab FoldersType name
    else if id == "users" then
        Tab UsersType name
    else if id == "cases" then
        Tab CasesType name
    else
        Tab FoldersType name


tabDecoder : Decode.Decoder Tab
tabDecoder =
    Decode.map2 createTab
        (field "id" Decode.string)
        (field "name" Decode.string)


entityDecoder : Decode.Decoder Entity
entityDecoder =
    Decode.map2 createEntity
        (field "id" Decode.string)
        (field "name" Decode.string)


useraccessDecoder : Decode.Decoder UserAccess
useraccessDecoder =
    Decode.map3 UserAccess
        (field "admin" Decode.bool)
        (field "owner" Decode.bool)
        (field "root" Decode.bool)
