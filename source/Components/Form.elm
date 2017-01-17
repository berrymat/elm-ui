module Components.Form exposing (..)

import Html exposing (node, text)
import Html.Attributes exposing (class)
import Html.Keyed
import Html.Lazy
import Ext.Color exposing (Hsv)
import Dict exposing (Dict)
import Color
import Date
import Ui.Native.Uid as Uid
import Ui.ColorPicker
import Ui.NumberRange
import Ui.DatePicker
import Ui.Textarea
import Ui.Checkbox
import Ui.Chooser
import Ui.Input
import Ui.FileInput
import Ui.Native.FileManager exposing (..)


type Msg
    = NumberRanges String Ui.NumberRange.Msg
    | DatePickers String Ui.DatePicker.Msg
    | Checkboxes String Ui.Checkbox.Msg
    | Colors String Ui.ColorPicker.Msg
    | Textareas String Ui.Textarea.Msg
    | Choosers String Ui.Chooser.Msg
    | Inputs String Ui.Input.Msg
    | FileInputs String Ui.FileInput.Msg
    | Validate


type alias ValidationError =
    Maybe String


type Validator a
    = Validator (Model -> a -> ValidationError)


type alias ModelItem model =
    ( Int, model, List (Validator model), ValidationError )


type alias Model =
    { numberRanges : Dict String (ModelItem Ui.NumberRange.Model)
    , checkboxes : Dict String (ModelItem Ui.Checkbox.Model)
    , colors : Dict String (ModelItem Ui.ColorPicker.Model)
    , textareas : Dict String (ModelItem Ui.Textarea.Model)
    , choosers : Dict String (ModelItem Ui.Chooser.Model)
    , dates : Dict String (ModelItem Ui.DatePicker.Model)
    , inputs : Dict String (ModelItem Ui.Input.Model)
    , fileInputs : Dict String (ModelItem Ui.FileInput.Model)
    , titles : Dict String ( Int, String )
    , valid : Maybe Bool
    , uid : String
    }


type alias TempModel =
    { numberRanges : List ( String, Int, Float, String, Float, Float, Int, Float, List (Validator Ui.NumberRange.Model) )
    , checkboxes : List ( String, Int, Bool, List (Validator Ui.Checkbox.Model) )
    , colors : List ( String, Int, Color.Color, List (Validator Ui.ColorPicker.Model) )
    , textareas : List ( String, Int, String, String, List (Validator Ui.Textarea.Model) )
    , choosers : List ( String, Int, List Ui.Chooser.Item, String, String, List (Validator Ui.Chooser.Model) )
    , dates : List ( String, Int, Date.Date, List (Validator Ui.DatePicker.Model) )
    , inputs : List ( String, Int, String, String, Maybe String, List (Validator Ui.Input.Model) )
    , fileInputs : List ( String, Int, String, List (Validator Ui.FileInput.Model) )
    , titles : List ( String, Int, String )
    }


init : TempModel -> Model
init data =
    let
        initDatePickers ( name, index, value, validators ) =
            ( name, ( index, Ui.DatePicker.init value, validators, Nothing ) )

        initCheckbox ( name, index, value, validators ) =
            ( name, ( index, Ui.Checkbox.init value, validators, Nothing ) )

        initChooser ( name, index, data, placeholder, value, validators ) =
            let
                initChooser =
                    Ui.Chooser.init data placeholder value

                chooser =
                    { initChooser | closeOnSelect = True }
            in
                ( name, ( index, chooser, validators, Nothing ) )

        initInput ( name, index, placeholder, value, maybeKind, validators ) =
            let
                initInput =
                    Ui.Input.init value placeholder

                input =
                    maybeKind
                        |> Maybe.map (\kind -> { initInput | kind = kind })
                        |> Maybe.withDefault initInput
            in
                ( name, ( index, input, validators, Nothing ) )

        initColors ( name, index, value, validators ) =
            ( name, ( index, Ui.ColorPicker.init value, validators, Nothing ) )

        initTextarea ( name, index, placeholder, value, validators ) =
            ( name, ( index, Ui.Textarea.init value placeholder, validators, Nothing ) )

        initNumberRange ( name, index, value, affix, min, max, round, step, validators ) =
            let
                baseNumberRange =
                    Ui.NumberRange.init value

                numberRange =
                    { baseNumberRange
                        | affix = affix
                        , round = round
                        , step = step
                        , max = max
                        , min = min
                    }
            in
                ( name, ( index, numberRange, validators, Nothing ) )

        initFileInput ( name, index, accept, validators ) =
            ( name, ( index, Ui.FileInput.init accept, validators, Nothing ) )

        initTitle ( name, index, value ) =
            ( name, ( index, value ) )
    in
        validateModel
            { numberRanges = Dict.fromList (List.map initNumberRange data.numberRanges)
            , checkboxes = Dict.fromList (List.map initCheckbox data.checkboxes)
            , textareas = Dict.fromList (List.map initTextarea data.textareas)
            , choosers = Dict.fromList (List.map initChooser data.choosers)
            , dates = Dict.fromList (List.map initDatePickers data.dates)
            , colors = Dict.fromList (List.map initColors data.colors)
            , inputs = Dict.fromList (List.map initInput data.inputs)
            , fileInputs = Dict.fromList (List.map initFileInput data.fileInputs)
            , titles = Dict.fromList (List.map initTitle data.titles)
            , valid = Nothing
            , uid = Uid.uid ()
            }


validateModel : Model -> Model
validateModel model =
    let
        validateItem ( index, data, validators, error ) =
            ( index, data, validators, (validate model validators data) )

        validateDict dict =
            Dict.map (\name item -> validateItem item) dict
    in
        { model
            | inputs = (validateDict model.inputs)
            , fileInputs = (validateDict model.fileInputs)
            , numberRanges = (validateDict model.numberRanges)
            , checkboxes = (validateDict model.checkboxes)
            , textareas = (validateDict model.textareas)
            , choosers = (validateDict model.choosers)
            , dates = (validateDict model.dates)
            , colors = (validateDict model.colors)
        }


isFormValid : Model -> Bool
isFormValid model =
    let
        isItemValid _ ( _, _, _, error ) valid =
            if valid then
                error |> Maybe.map (\_ -> False) |> Maybe.withDefault True
            else
                False

        isDictValid dict =
            Dict.foldl isItemValid True dict
    in
        (isDictValid model.inputs)
            && (isDictValid model.fileInputs)
            && (isDictValid model.numberRanges)
            && (isDictValid model.checkboxes)
            && (isDictValid model.textareas)
            && (isDictValid model.choosers)
            && (isDictValid model.dates)
            && (isDictValid model.colors)


nextPosition : Model -> Int
nextPosition model =
    (Dict.size model.numberRanges)
        + (Dict.size model.checkboxes)
        + (Dict.size model.textareas)
        + (Dict.size model.choosers)
        + (Dict.size model.dates)
        + (Dict.size model.colors)
        + (Dict.size model.inputs)
        + (Dict.size model.fileInputs)
        + (Dict.size model.titles)


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        colorSub name colorPicker =
            Sub.map (Colors name) (Ui.ColorPicker.subscriptions colorPicker)

        colorSubs =
            Dict.toList model.colors
                |> List.map (\( key, ( pos, colorPicker, validators, error ) ) -> colorSub key colorPicker)

        numberRangeSub name numberRange =
            Sub.map (NumberRanges name) (Ui.NumberRange.subscriptions numberRange)

        numberRangeSubs =
            Dict.toList model.numberRanges
                |> List.map (\( key, ( pos, numberRange, validators, error ) ) -> numberRangeSub key numberRange)
    in
        Sub.batch (colorSubs ++ numberRangeSubs)


valueOfSimple :
    String
    -> value
    -> (model -> value)
    -> Dict String (ModelItem model)
    -> value
valueOfSimple name default accessor dict =
    Dict.get name dict
        |> Maybe.map (\( _, s, _, _ ) -> s)
        |> Maybe.map accessor
        |> Maybe.withDefault default


valueOfCheckbox : String -> Bool -> Model -> Bool
valueOfCheckbox name default model =
    valueOfSimple name default .value model.checkboxes


valueOfInput : String -> String -> Model -> String
valueOfInput name default model =
    valueOfSimple name default .value model.inputs


valueOfFileInput : String -> Maybe File -> Model -> Maybe File
valueOfFileInput name default model =
    valueOfSimple name default .file model.fileInputs


valueOfNumberRange : String -> Float -> Model -> Float
valueOfNumberRange name default model =
    valueOfSimple name default .value model.numberRanges


valueOfTextarea : String -> String -> Model -> String
valueOfTextarea name default model =
    valueOfSimple name default .value model.textareas


valueOfColor : String -> Hsv -> Model -> Hsv
valueOfColor name default model =
    valueOfSimple name default (\item -> item.colorPanel.value) model.colors


valueOfDate : String -> Date.Date -> Model -> Date.Date
valueOfDate name default model =
    valueOfSimple name default (\item -> item.calendar.value) model.dates


valueOfChooser : String -> String -> Model -> String
valueOfChooser name default model =
    case Dict.get name model.choosers of
        Just ( index, chooser, validators, error ) ->
            Maybe.withDefault default (Ui.Chooser.getFirstSelected chooser)

        _ ->
            default


validate : Model -> List (Validator a) -> a -> ValidationError
validate model validators input =
    let
        validate validator error =
            let
                (Validator validatorFn) =
                    validator
            in
                case error of
                    Just _ ->
                        error

                    Nothing ->
                        validatorFn model input
    in
        List.foldl validate Nothing validators


validateItem : ModelItem model -> value -> (value -> model -> model) -> Model -> ModelItem model
validateItem ( index, itemModel, validators, error ) value setter model =
    let
        newValue =
            setter value itemModel

        newError =
            validate model validators newValue
    in
        ( index, newValue, validators, newError )


updateColor : String -> Color.Color -> Model -> Model
updateColor name value model =
    let
        updatedColor item =
            case item of
                Just itemModel ->
                    Just (validateItem itemModel value Ui.ColorPicker.setValue model)

                _ ->
                    item
    in
        { model | colors = Dict.update name updatedColor model.colors }


updateDate : String -> Date.Date -> Model -> Model
updateDate name value model =
    let
        updatedDate item =
            case item of
                Just itemModel ->
                    Just (validateItem itemModel value Ui.DatePicker.setValue model)

                _ ->
                    item
    in
        { model | dates = Dict.update name updatedDate model.dates }


updateTextarea : String -> String -> Model -> Model
updateTextarea name value model =
    let
        updatedTextarea item =
            case item of
                Just itemModel ->
                    Just (validateItem itemModel value Ui.Textarea.setValue model)

                _ ->
                    item
    in
        { model | textareas = Dict.update name updatedTextarea model.textareas }


updateInput : String -> String -> Model -> Model
updateInput name value model =
    let
        updatedInput item =
            case item of
                Just itemModel ->
                    Just (validateItem itemModel value Ui.Input.setValue model)

                _ ->
                    item
    in
        { model | inputs = Dict.update name updatedInput model.inputs }


updateNumberRange : String -> Float -> Model -> Model
updateNumberRange name value model =
    let
        updatedNumberRange item =
            case item of
                Just itemModel ->
                    Just (validateItem itemModel value Ui.NumberRange.setValue model)

                _ ->
                    item
    in
        { model | numberRanges = Dict.update name updatedNumberRange model.numberRanges }


updateCheckbox : String -> Bool -> Model -> Model
updateCheckbox name value model =
    let
        updatedCheckbox item =
            case item of
                Just itemModel ->
                    Just (validateItem itemModel value Ui.Checkbox.setValue model)

                _ ->
                    item
    in
        { model | checkboxes = Dict.update name updatedCheckbox model.checkboxes }


updateDict :
    Model
    -> String
    -> msg
    -> (msg -> model -> ( model, Cmd msg ))
    -> Dict String (ModelItem model)
    -> ( Cmd msg, Dict String (ModelItem model) )
updateDict model name act fn dict =
    case Dict.get name dict of
        Just ( index, value, validators, error ) ->
            let
                ( updateValue, effect ) =
                    fn act value

                newError =
                    validate model validators updateValue
            in
                ( effect, Dict.insert name ( index, updateValue, validators, newError ) dict )

        Nothing ->
            ( Cmd.none, dict )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        DatePickers name act ->
            let
                ( effect, updatedDatePickers ) =
                    updateDict model name act Ui.DatePicker.update model.dates
            in
                ( { model | dates = updatedDatePickers }
                , Cmd.map (DatePickers name) effect
                )

        Checkboxes name act ->
            let
                ( effect, updatedCheckboxes ) =
                    updateDict model name act Ui.Checkbox.update model.checkboxes
            in
                ( { model | checkboxes = updatedCheckboxes }
                , Cmd.map (Checkboxes name) effect
                )

        Choosers name act ->
            let
                ( effect, updatedChoosers ) =
                    updateDict model name act Ui.Chooser.update model.choosers
            in
                ( { model | choosers = updatedChoosers }
                , Cmd.map (Choosers name) effect
                )

        Inputs name act ->
            let
                ( effect, updatedInputs ) =
                    updateDict model name act Ui.Input.update model.inputs
            in
                ( { model | inputs = updatedInputs }
                , Cmd.map (Inputs name) effect
                )

        FileInputs name act ->
            let
                ( effect, updatedFileInputs ) =
                    updateDict model name act Ui.FileInput.update model.fileInputs
            in
                ( { model | fileInputs = updatedFileInputs }
                , Cmd.map (FileInputs name) effect
                )

        Colors name act ->
            let
                ( effect, updatedColors ) =
                    updateDict model name act Ui.ColorPicker.update model.colors
            in
                ( { model | colors = updatedColors }
                , Cmd.map (Colors name) effect
                )

        Textareas name act ->
            let
                ( effect, updatedTextareas ) =
                    updateDict model name act Ui.Textarea.update model.textareas
            in
                ( { model | textareas = updatedTextareas }
                , Cmd.map (Textareas name) effect
                )

        NumberRanges name act ->
            let
                ( effect, updatedNumberRanges ) =
                    updateDict model name act Ui.NumberRange.update model.numberRanges
            in
                ( { model | numberRanges = updatedNumberRanges }
                , Cmd.map (NumberRanges name) effect
                )

        Validate ->
            ( { model | valid = Just (isFormValid model) }, Cmd.none )


view : (Msg -> msg) -> Model -> Html.Html msg
view address fields =
    let
        renderDatePicker name ( data, error, valid ) =
            blockField name
                (Html.map (address << (DatePickers name)) (Ui.DatePicker.view "en_us" data))
                error
                valid

        renderCheckbox name ( data, error, valid ) =
            inlineField name
                (Html.map (address << (Checkboxes name)) (Ui.Checkbox.view data))
                error
                valid

        renderChooser name ( data, error, valid ) =
            blockField name
                (Html.map (address << (Choosers name)) (Ui.Chooser.view data))
                error
                valid

        renderInput name ( data, error, valid ) =
            blockField name
                (Html.map (address << (Inputs name)) (Ui.Input.view data))
                error
                valid

        renderFileInput name ( data, error, valid ) =
            blockField name
                (Html.map (address << (FileInputs name)) (Ui.FileInput.view data))
                error
                valid

        renderColorPicker name ( data, error, valid ) =
            blockField name
                (Html.map (address << (Colors name)) (Ui.ColorPicker.view data))
                error
                valid

        renderTextarea name ( data, error, valid ) =
            blockField name
                (Html.map (address << (Textareas name)) (Ui.Textarea.view data))
                error
                valid

        renderNumberRange name ( data, error, valid ) =
            blockField name
                (Html.map (address << (NumberRanges name)) (Ui.NumberRange.view data))
                error
                valid

        renderTitle name data =
            node "ui-form-title" [] [ text data ]

        errorMessage error valid =
            let
                message =
                    error |> Maybe.withDefault ""

                className =
                    if (valid |> Maybe.withDefault True) then
                        ""
                    else
                        "error"
            in
                node "ui-form-error" [ class className ] [ text message ]

        blockField name child error valid =
            node "ui-form-block"
                []
                [ node "ui-form-label" [] [ text name ]
                , child
                , errorMessage error valid
                ]

        inlineField name child error valid =
            node "ui-form-inline"
                []
                [ child
                , node "ui-form-label" [] [ text name ]
                , errorMessage error valid
                ]

        renderList fn ( name, ( index, data, validators, error ) ) =
            ( index, fn name ( data, error, fields.valid ) )

        renderMap fn list =
            List.map (\item -> renderList fn item) (Dict.toList list)

        renderTitleList fn ( name, ( index, data ) ) =
            ( index, fn name data )

        renderTitleMap fn list =
            List.map (\item -> renderTitleList fn item) (Dict.toList list)

        items =
            ((renderMap renderCheckbox fields.checkboxes)
                ++ (renderMap (Html.Lazy.lazy2 renderColorPicker) fields.colors)
                ++ (renderMap (Html.Lazy.lazy2 renderChooser) fields.choosers)
                ++ (renderMap (Html.Lazy.lazy2 renderDatePicker) fields.dates)
                ++ (renderMap (Html.Lazy.lazy2 renderInput) fields.inputs)
                ++ (renderMap (Html.Lazy.lazy2 renderFileInput) fields.fileInputs)
                ++ (renderMap (Html.Lazy.lazy2 renderTextarea) fields.textareas)
                ++ (renderMap (Html.Lazy.lazy2 renderNumberRange) fields.numberRanges)
                ++ (renderTitleMap (Html.Lazy.lazy2 renderTitle) fields.titles)
            )

        sortedItems =
            List.sortWith (\( a, _ ) ( b, _ ) -> compare a b) items
                |> List.map (\( key, value ) -> ( fields.uid ++ (toString key), value ))
    in
        node "ui-form"
            []
            [ Html.Keyed.node "ui-form-fields" [] sortedItems
            ]
