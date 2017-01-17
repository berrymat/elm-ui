module Issues.Issue exposing (..)

import Json.Decode as Decode exposing (field, at)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode
import Ui.Native.FileManager exposing (..)
import Http


type alias Issue =
    { id : String
    , siteId : String
    , caseNumber : String
    , createdDateTime : Float
    , closedDateTime : Float
    , status : String
    , clientName : String
    , siteName : String
    , photoUrl : String
    , notes : List String
    , comment : String
    , file : Maybe File
    , checked : Bool
    }


issueDecoder : Decode.Decoder Issue
issueDecoder =
    decode Issue
        |> required "id" Decode.string
        |> required "siteId" Decode.string
        |> required "caseNumber" Decode.string
        |> required "createdDateTime" Decode.float
        |> required "closedDateTime" Decode.float
        |> required "status" Decode.string
        |> required "clientName" Decode.string
        |> required "siteName" Decode.string
        |> required "photoUrl" Decode.string
        |> required "notes" (Decode.list Decode.string)
        |> hardcoded ""
        |> hardcoded Nothing
        |> hardcoded False


issueParts : Issue -> List Http.Part
issueParts issue =
    let
        filePart file =
            [ Ui.Native.FileManager.toFormData "file" file ]

        fileParts =
            Maybe.map filePart issue.file
                |> Maybe.withDefault []

        encodedIssues =
            [ (Http.stringPart "values" (Encode.encode 2 (encodeIssue issue))) ]
    in
        List.append encodedIssues fileParts


encodeIssue : Issue -> Encode.Value
encodeIssue issue =
    Encode.object
        [ ( "id", Encode.string issue.id )
        , ( "siteId", Encode.string issue.siteId )
        , ( "comment", Encode.string issue.comment )
        ]


type alias IssueSite =
    { id : String
    , name : String
    }


issueSiteDecoder : Decode.Decoder IssueSite
issueSiteDecoder =
    decode IssueSite
        |> required "id" Decode.string
        |> required "name" Decode.string



{-
   public class Root
   {
       public string id { get; set; }
       public bool canAdd { get; set; }
       public bool canEdit { get; set; }
       public bool canDelete { get; set; }

       public List<ApiIssue> issues { get; set; }
   }

   public class ApiIssue
   {
       public string id { get; set; }
       public string caseNumber { get; set; }
       public double createdDateTime { get; set; }
       public double closedDateTime { get; set; }
       public string status { get; set; }
       public string clientName { get; set; }
       public string siteName { get; set; }
       public string[] notes { get; set; }
       public string photoUrl { get; set; }
       public IssueSite[] sites { get; set; }
   }

   public class IssueSite
   {
       public string id { get; set; }
       public string name { get; set; }
   }
-}
