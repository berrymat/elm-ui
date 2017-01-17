module Components.Validators
    exposing
        ( requiredInput
        , requiredChooser
        , requiredTextarea
        , requiredValidEmail
        , optionalValidEmail
        )

import Components.Form as Form exposing (ValidationError)
import Ui.Chooser
import Ui.Input
import Ui.Textarea
import Regex
import Set


requiredInput : Form.Model -> Ui.Input.Model -> ValidationError
requiredInput model input =
    if (String.length input.value) == 0 then
        Just "Value is required"
    else
        Nothing


requiredChooser : Form.Model -> Ui.Chooser.Model -> ValidationError
requiredChooser model chooser =
    if (Set.isEmpty chooser.selected) then
        Just "Value is required"
    else
        Nothing


requiredTextarea : Form.Model -> Ui.Textarea.Model -> ValidationError
requiredTextarea model textarea =
    if (String.isEmpty textarea.value) then
        Just "Value is required"
    else
        Nothing


requiredValidEmail : Form.Model -> Ui.Input.Model -> ValidationError
requiredValidEmail model input =
    if (String.length input.value) == 0 || not (isValidEmail input.value) then
        Just "A valid email address is required (e.g. john.smith@name.com)"
    else
        Nothing


optionalValidEmail : Form.Model -> Ui.Input.Model -> ValidationError
optionalValidEmail model input =
    if (String.length input.value) > 0 && not (isValidEmail input.value) then
        Just "Please enter a valid email address (e.g. john.smith@name.com)"
    else
        Nothing


isValidEmail : String -> Bool
isValidEmail value =
    let
        validEmail =
            Regex.regex "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
                |> Regex.caseInsensitive
    in
        Regex.contains validEmail value
