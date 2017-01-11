module Components.Validators exposing (requiredInput, requiredValidEmail, optionalValidEmail)

import Components.Form as Form exposing (ValidationError)
import Ui.Input
import Regex


requiredInput : Form.Model -> Ui.Input.Model -> ValidationError
requiredInput model input =
    if (String.length input.value) == 0 then
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
