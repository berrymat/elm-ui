module Components.Form exposing (..)

import Html exposing (node, text)
import Html.Keyed
import Html.Lazy


--import Html.App

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
import Ui.Button
import Ui.Input


type Msg
    = NumberRanges String Ui.NumberRange.Msg
    | DatePickers String Ui.DatePicker.Msg
    | Checkboxes String Ui.Checkbox.Msg
    | Colors String Ui.ColorPicker.Msg
    | Textareas String Ui.Textarea.Msg
    | Choosers String Ui.Chooser.Msg
    | Inputs String Ui.Input.Msg


type alias ValidationError =
    Maybe String


type InputValidator
    = InputValidator (Model -> Ui.Input.Model -> ValidationError)


type alias Model =
    { numberRanges : Dict String ( Int, Ui.NumberRange.Model )
    , checkboxes : Dict String ( Int, Ui.Checkbox.Model )
    , colors : Dict String ( Int, Ui.ColorPicker.Model )
    , textareas : Dict String ( Int, Ui.Textarea.Model )
    , choosers : Dict String ( Int, Ui.Chooser.Model )
    , dates : Dict String ( Int, Ui.DatePicker.Model )
    , inputs : Dict String ( Int, Ui.Input.Model, List InputValidator, ValidationError )
    , titles : Dict String ( Int, String )
    , uid : String
    }


type alias TempModel =
    { numberRanges : List ( String, Int, Float, String, Float, Float, Int, Float )
    , choosers : List ( String, Int, List Ui.Chooser.Item, String, String )
    , textareas : List ( String, Int, String, String )
    , inputs : List ( String, Int, String, String, Maybe String )
    , colors : List ( String, Int, Color.Color )
    , checkboxes : List ( String, Int, Bool )
    , dates : List ( String, Int, Date.Date )
    , titles : List ( String, Int, String )
    }


type alias TempModelEx =
    { numberRanges : List ( String, Int, Float, String, Float, Float, Int, Float )
    , choosers : List ( String, Int, List Ui.Chooser.Item, String, String )
    , textareas : List ( String, Int, String, String )
    , inputs : List ( String, Int, String, String, Maybe String, List InputValidator )
    , colors : List ( String, Int, Color.Color )
    , checkboxes : List ( String, Int, Bool )
    , dates : List ( String, Int, Date.Date )
    , titles : List ( String, Int, String )
    }


init : TempModel -> Model
init data =
    let
        initDatePickers ( name, index, value ) =
            ( name, ( index, Ui.DatePicker.init value ) )

        initCheckbox ( name, index, value ) =
            ( name, ( index, Ui.Checkbox.init value ) )

        initChooser ( name, index, data, placeholder, value ) =
            ( name, ( index, Ui.Chooser.init data placeholder value ) )

        initInput ( name, index, placeholder, value, maybeKind ) =
            let
                initInput =
                    Ui.Input.init value placeholder

                input =
                    maybeKind
                        |> Maybe.map (\kind -> { initInput | kind = kind })
                        |> Maybe.withDefault initInput
            in
                ( name, ( index, input, [], Nothing ) )

        initColors ( name, index, value ) =
            ( name, ( index, Ui.ColorPicker.init value ) )

        initTextarea ( name, index, placeholder, value ) =
            ( name, ( index, Ui.Textarea.init value placeholder ) )

        initNumberRange ( name, index, value, affix, min, max, round, step ) =
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
                ( name, ( index, numberRange ) )

        initTitle ( name, index, value ) =
            ( name, ( index, value ) )
    in
        { numberRanges = Dict.fromList (List.map initNumberRange data.numberRanges)
        , checkboxes = Dict.fromList (List.map initCheckbox data.checkboxes)
        , textareas = Dict.fromList (List.map initTextarea data.textareas)
        , choosers = Dict.fromList (List.map initChooser data.choosers)
        , dates = Dict.fromList (List.map initDatePickers data.dates)
        , colors = Dict.fromList (List.map initColors data.colors)
        , inputs = Dict.fromList (List.map initInput data.inputs)
        , titles = Dict.fromList (List.map initTitle data.titles)
        , uid = Uid.uid ()
        }


initEx : TempModelEx -> Model
initEx data =
    let
        initDatePickers ( name, index, value ) =
            ( name, ( index, Ui.DatePicker.init value ) )

        initCheckbox ( name, index, value ) =
            ( name, ( index, Ui.Checkbox.init value ) )

        initChooser ( name, index, data, placeholder, value ) =
            ( name, ( index, Ui.Chooser.init data placeholder value ) )

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

        initColors ( name, index, value ) =
            ( name, ( index, Ui.ColorPicker.init value ) )

        initTextarea ( name, index, placeholder, value ) =
            ( name, ( index, Ui.Textarea.init value placeholder ) )

        initNumberRange ( name, index, value, affix, min, max, round, step ) =
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
                ( name, ( index, numberRange ) )

        initTitle ( name, index, value ) =
            ( name, ( index, value ) )
    in
        { numberRanges = Dict.fromList (List.map initNumberRange data.numberRanges)
        , checkboxes = Dict.fromList (List.map initCheckbox data.checkboxes)
        , textareas = Dict.fromList (List.map initTextarea data.textareas)
        , choosers = Dict.fromList (List.map initChooser data.choosers)
        , dates = Dict.fromList (List.map initDatePickers data.dates)
        , colors = Dict.fromList (List.map initColors data.colors)
        , inputs = Dict.fromList (List.map initInput data.inputs)
        , titles = Dict.fromList (List.map initTitle data.titles)
        , uid = Uid.uid ()
        }


nextPosition : Model -> Int
nextPosition model =
    (Dict.size model.numberRanges)
        + (Dict.size model.checkboxes)
        + (Dict.size model.textareas)
        + (Dict.size model.choosers)
        + (Dict.size model.dates)
        + (Dict.size model.colors)
        + (Dict.size model.inputs)
        + (Dict.size model.titles)


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        colorSub name colorPicker =
            Sub.map (Colors name) (Ui.ColorPicker.subscriptions colorPicker)

        colorSubs =
            Dict.toList model.colors
                |> List.map (\( key, ( pos, colorPicker ) ) -> colorSub key colorPicker)

        numberRangeSub name numberRange =
            Sub.map (NumberRanges name) (Ui.NumberRange.subscriptions numberRange)

        numberRangeSubs =
            Dict.toList model.numberRanges
                |> List.map (\( key, ( pos, numberRange ) ) -> numberRangeSub key numberRange)
    in
        Sub.batch (colorSubs ++ numberRangeSubs)


valueOfSimple :
    String
    -> value
    -> (model -> value)
    -> Dict String ( Int, model )
    -> value
valueOfSimple name default accessor dict =
    Dict.get name dict
        |> Maybe.map (\( f, s ) -> s)
        |> Maybe.map accessor
        |> Maybe.withDefault default


valueOfSimpleEx :
    String
    -> value
    -> (model -> value)
    -> Dict String ( Int, model, List InputValidator, ValidationError )
    -> value
valueOfSimpleEx name default accessor dict =
    Dict.get name dict
        |> Maybe.map (\( f, s, i, v ) -> s)
        |> Maybe.map accessor
        |> Maybe.withDefault default


valueOfCheckbox : String -> Bool -> Model -> Bool
valueOfCheckbox name default model =
    valueOfSimple name default .value model.checkboxes


valueOfInput : String -> String -> Model -> String
valueOfInput name default model =
    valueOfSimpleEx name default .value model.inputs


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
        Just ( index, chooser ) ->
            Maybe.withDefault default (Ui.Chooser.getFirstSelected chooser)

        _ ->
            default


updateColor : String -> Color.Color -> Model -> Model
updateColor name value model =
    let
        updatedColor item =
            case item of
                Just ( index, colorPicker ) ->
                    Just ( index, Ui.ColorPicker.setValue value colorPicker )

                _ ->
                    item
    in
        { model | colors = Dict.update name updatedColor model.colors }


updateDate : String -> Date.Date -> Model -> Model
updateDate name value model =
    let
        updatedDate item =
            case item of
                Just ( index, datepicker ) ->
                    Just ( index, Ui.DatePicker.setValue value datepicker )

                _ ->
                    item
    in
        { model | dates = Dict.update name updatedDate model.dates }


updateTextarea : String -> String -> Model -> Model
updateTextarea name value model =
    let
        updatedTextarea item =
            case item of
                Just ( index, input ) ->
                    Just ( index, Ui.Textarea.setValue value input )

                _ ->
                    item
    in
        { model | textareas = Dict.update name updatedTextarea model.textareas }


validateInput : Model -> List InputValidator -> Ui.Input.Model -> ValidationError
validateInput model validators input =
    let
        validate validator error =
            let
                (InputValidator validatorFn) =
                    validator
            in
                case error of
                    Just _ ->
                        error

                    Nothing ->
                        validatorFn model input
    in
        List.foldl validate Nothing validators



{-
   foldl : (a -> b -> b) -> b -> List a -> b
   Reduce a list from the left.

   foldl (::) [] [1,2,3] == [3,2,1]
-}


updateInput : String -> String -> Model -> Model
updateInput name value model =
    let
        updatedInput item =
            case item of
                Just ( index, input, validators, error ) ->
                    let
                        newInput =
                            Ui.Input.setValue value input

                        newError =
                            validateInput model validators newInput
                    in
                        Just ( index, newInput, validators, newError )

                _ ->
                    item
    in
        { model | inputs = Dict.update name updatedInput model.inputs }


updateNumberRange : String -> Float -> Model -> Model
updateNumberRange name value model =
    let
        updatedNumberRange item =
            case item of
                Just ( index, numberRange ) ->
                    Just ( index, Ui.NumberRange.setValue value numberRange )

                _ ->
                    item
    in
        { model | numberRanges = Dict.update name updatedNumberRange model.numberRanges }


updateCheckbox : String -> Bool -> Model -> Model
updateCheckbox name value model =
    let
        updatedCheckbox item =
            case item of
                Just ( index, checkbox ) ->
                    Just ( index, Ui.Checkbox.setValue value checkbox )

                _ ->
                    item
    in
        { model | checkboxes = Dict.update name updatedCheckbox model.checkboxes }


updateDict :
    String
    -> msg
    -> (msg -> model -> ( model, Cmd msg ))
    -> Dict String ( Int, model )
    -> ( Cmd msg, Dict String ( Int, model ) )
updateDict name act fn dict =
    case Dict.get name dict of
        Just ( index, value ) ->
            let
                ( updateValue, effect ) =
                    fn act value
            in
                ( effect, Dict.insert name ( index, updateValue ) dict )

        Nothing ->
            ( Cmd.none, dict )


updateDictEx :
    String
    -> msg
    -> (msg -> model -> ( model, Cmd msg ))
    -> Dict String ( Int, model, List InputValidator, ValidationError )
    -> ( Cmd msg, Dict String ( Int, model, List InputValidator, ValidationError ) )
updateDictEx name act fn dict =
    case Dict.get name dict of
        Just ( index, value, validators, error ) ->
            let
                ( updateValue, effect ) =
                    fn act value
            in
                ( effect, Dict.insert name ( index, updateValue, validators, error ) dict )

        Nothing ->
            ( Cmd.none, dict )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        DatePickers name act ->
            let
                ( effect, updatedDatePickers ) =
                    updateDict name act Ui.DatePicker.update model.dates
            in
                ( { model | dates = updatedDatePickers }
                , Cmd.map (DatePickers name) effect
                )

        Checkboxes name act ->
            let
                ( effect, updatedCheckboxes ) =
                    updateDict name act Ui.Checkbox.update model.checkboxes
            in
                ( { model | checkboxes = updatedCheckboxes }
                , Cmd.map (Checkboxes name) effect
                )

        Choosers name act ->
            let
                ( effect, updatedChoosers ) =
                    updateDict name act Ui.Chooser.update model.choosers
            in
                ( { model | choosers = updatedChoosers }
                , Cmd.map (Choosers name) effect
                )

        Inputs name act ->
            let
                ( effect, updatedInputs ) =
                    updateDictEx name act Ui.Input.update model.inputs
            in
                ( { model | inputs = updatedInputs }
                , Cmd.map (Inputs name) effect
                )

        Colors name act ->
            let
                ( effect, updatedColors ) =
                    updateDict name act Ui.ColorPicker.update model.colors
            in
                ( { model | colors = updatedColors }
                , Cmd.map (Colors name) effect
                )

        Textareas name act ->
            let
                ( effect, updatedTextareas ) =
                    updateDict name act Ui.Textarea.update model.textareas
            in
                ( { model | textareas = updatedTextareas }
                , Cmd.map (Textareas name) effect
                )

        NumberRanges name act ->
            let
                ( effect, updatedNumberRanges ) =
                    updateDict name act Ui.NumberRange.update model.numberRanges
            in
                ( { model | numberRanges = updatedNumberRanges }
                , Cmd.map (NumberRanges name) effect
                )


view : (Msg -> msg) -> Model -> Html.Html msg
view address fields =
    let
        renderDatePicker name data =
            blockField name
                (Html.map (address << (DatePickers name)) (Ui.DatePicker.view "en_us" data))

        renderCheckbox name data =
            inlineField name
                (Html.map (address << (Checkboxes name)) (Ui.Checkbox.view data))

        renderChooser name data =
            blockField name
                (Html.map (address << (Choosers name)) (Ui.Chooser.view data))

        renderInput name data error =
            blockField name
                (Html.map (address << (Inputs name)) (Ui.Input.view data))

        renderColorPicker name data =
            blockField name
                (Html.map (address << (Colors name)) (Ui.ColorPicker.view data))

        renderTextarea name data =
            blockField name
                (Html.map (address << (Textareas name)) (Ui.Textarea.view data))

        renderNumberRange name data =
            blockField name
                (Html.map (address << (NumberRanges name)) (Ui.NumberRange.view data))

        renderTitle name data =
            node "ui-form-title" [] [ text data ]

        blockField name child =
            node "ui-form-block"
                []
                [ node "ui-form-label" [] [ text name ]
                , child
                ]

        inlineField name child =
            node "ui-form-inline"
                []
                [ child
                , node "ui-form-label" [] [ text name ]
                ]

        renderList fn ( name, ( index, data ) ) =
            ( index, fn name data )

        renderListEx fn ( name, ( index, data, validators, error ) ) =
            ( index, fn name data error )

        renderButton ( index, msg, button ) =
            let
                html =
                    node "ui-form-block"
                        []
                        [ Ui.Button.view msg button
                        ]
            in
                ( index, html )

        renderButtons list =
            List.map renderButton list

        renderMap fn list =
            List.map (\item -> renderList fn item) (Dict.toList list)

        renderMapEx fn list =
            List.map (\item -> renderListEx fn item) (Dict.toList list)

        items =
            ((renderMap renderCheckbox fields.checkboxes)
                ++ (renderMap (Html.Lazy.lazy2 renderColorPicker) fields.colors)
                ++ (renderMap (Html.Lazy.lazy2 renderChooser) fields.choosers)
                ++ (renderMap (Html.Lazy.lazy2 renderDatePicker) fields.dates)
                ++ (renderMapEx (Html.Lazy.lazy3 renderInput) fields.inputs)
                ++ (renderMap (Html.Lazy.lazy2 renderTextarea) fields.textareas)
                ++ (renderMap (Html.Lazy.lazy2 renderNumberRange) fields.numberRanges)
                ++ (renderMap (Html.Lazy.lazy2 renderTitle) fields.titles)
            )

        sortedItems =
            List.sortWith (\( a, _ ) ( b, _ ) -> compare a b) items
                |> List.map (\( key, value ) -> ( fields.uid ++ (toString key), value ))
    in
        node "ui-form"
            []
            [ Html.Keyed.node "ui-form-fields" [] sortedItems
            ]
