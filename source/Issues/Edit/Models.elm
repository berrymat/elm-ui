module Issues.Edit.Models exposing (..)

import Helpers.Models exposing (..)
import Components.Form as Form exposing (ValidationError)
import Components.Validators exposing (..)
import Ui.Chooser
import Ui.Modal
import RemoteData exposing (..)
import Issues.Issue exposing (..)


type Msg
    = Save AuthToken
    | Cancel
    | SaveResponse (WebData Issue)
    | ModalMsg Ui.Modal.Msg
    | FormMsg Form.Msg


type alias Model =
    { id : NodeId
    , issue : Issue
    , method : HttpMethod
    , form : Form.Model
    , modal : Ui.Modal.Model
    , response : WebData Issue
    }


init : List IssueSite -> Issue -> HttpMethod -> Model
init sites issue method =
    { id = issue.id
    , issue = issue
    , method = method
    , form = issueForm sites issue method
    , modal = Ui.Modal.open Ui.Modal.init
    , response = NotAsked
    }


siteData : List IssueSite -> List Ui.Chooser.Item
siteData sites =
    List.map (\s -> { label = s.name, value = s.id }) sites


issueForm : List IssueSite -> Issue -> HttpMethod -> Form.Model
issueForm sites issue method =
    let
        ( fileInputs, choosers ) =
            case method of
                Post ->
                    ( [ ( "Photo", 3, "image/*", [] ) ]
                    , [ ( "Site", 1, siteData sites, "Choose Site...", issue.siteId, [ Form.Validator requiredChooser ] ) ]
                    )

                _ ->
                    ( [], [] )
    in
        Form.init
            { checkboxes = []
            , inputs = []
            , fileInputs = fileInputs
            , numberRanges = []
            , textareas =
                [ ( "Comment", 2, "Add Comment...", issue.comment, [ Form.Validator requiredTextarea ] )
                ]
            , choosers = choosers
            , colors = []
            , dates = []
            , titles = []
            }


updateIssue : Form.Model -> Issue -> Issue
updateIssue form issue =
    { issue
        | siteId = Form.valueOfChooser "Site" issue.siteId form
        , comment = Form.valueOfTextarea "Comment" issue.comment form
        , file = Form.valueOfFileInput "Photo" issue.file form
    }
