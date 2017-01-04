module Helpers.Button
    exposing
        ( Model
        , init
        , view
        , render
        , attributes
        , primaryBig
        , primarySmall
        , primary
        , secondaryBig
        , secondarySmall
        , secondary
        , warningBig
        , warningSmall
        , warning
        , successBig
        , successSmall
        , success
        , dangerBig
        , dangerSmall
        , danger
        )

{-| Basic button component that implements:
  - **5 different types** (primary, secondary, warning, danger, success)
  - **3 different sizes** (small, medium, big)
  - **focus state** with animation
  - **disabled state**
  - **readonly state**

# Model
@docs Model, init

# View
@docs view, render

# View Variations
@docs primary, primaryBig, primarySmall
@docs secondary, secondaryBig, secondarySmall
@docs warning, warningBig, warningSmall
@docs success, successBig, successSmall
@docs danger, dangerBig, dangerSmall

# Functions
@docs attributes
-}

import Html.Attributes exposing (classList)
import Html.Events.Extra exposing (onKeys)
import Html.Events exposing (on)
import Html exposing (node, text)
import Html.Lazy
import Ui.Helpers.Ripple as Ripple
import Ui
import Json.Decode


{-| Representation of a button:
  - **disabled** - Whether or not the button is disabled
  - **readonly** - Whether or not the button is readonly
  - **kind** - The type of the button
  - **size** - The size of the button
  - **text** - The text of the button
-}
type alias Model =
    { disabled : Bool
    , readonly : Bool
    , kind : String
    , size : String
    , text : String
    }


{-| Initializes a button with the given data.

    button = Ui.Button.init False "Upload" "primary" "medium"
-}
init : Bool -> Bool -> String -> String -> String -> Model
init disabled readonly text kind size =
    { disabled = disabled
    , readonly = readonly
    , text = text
    , kind = kind
    , size = size
    }


{-| Lazily renders a button.

    Ui.Button.view address button
-}
view : Json.Decode.Decoder msg -> Model -> Html.Html msg
view msg model =
    Html.Lazy.lazy2 render msg model


{-| Renders a button.
-}
render : Json.Decode.Decoder msg -> Model -> Html.Html msg
render msg model =
    node
        "ui-button"
        (attributes msg model)
        [ Ripple.view
        , node "span" [] [ text model.text ]
        ]


{-| Renders a **big primary** button with the given text.
-}
primaryBig : String -> Json.Decode.Decoder msg -> Html.Html msg
primaryBig title msg =
    Html.Lazy.lazy2 render msg (init False False title "primary" "big")


{-| Renders a **small primary** button with the given text.
-}
primarySmall : String -> Json.Decode.Decoder msg -> Html.Html msg
primarySmall title msg =
    Html.Lazy.lazy2 render msg (init False False title "primary" "small")


{-| Renders a **medium primary** (normal) button with the given text.
-}
primary : String -> Json.Decode.Decoder msg -> Html.Html msg
primary title msg =
    Html.Lazy.lazy2 render msg (init False False title "primary" "medium")


{-| Renders a **big secondary** button with the given text.
-}
secondaryBig : String -> Json.Decode.Decoder msg -> Html.Html msg
secondaryBig title msg =
    Html.Lazy.lazy2 render msg (init False False title "secondary" "big")


{-| Renders a **small secondary** button with the given text.
-}
secondarySmall : String -> Json.Decode.Decoder msg -> Html.Html msg
secondarySmall title msg =
    Html.Lazy.lazy2 render msg (init False False title "secondary" "small")


{-| Renders a **medium secondary** (normal) button with the given text.
-}
secondary : String -> Json.Decode.Decoder msg -> Html.Html msg
secondary title msg =
    Html.Lazy.lazy2 render msg (init False False title "secondary" "medium")


{-| Renders a **big warning** button with the given text.
-}
warningBig : String -> Json.Decode.Decoder msg -> Html.Html msg
warningBig title msg =
    Html.Lazy.lazy2 render msg (init False False title "warning" "big")


{-| Renders a **small warning** button with the given text.
-}
warningSmall : String -> Json.Decode.Decoder msg -> Html.Html msg
warningSmall title msg =
    Html.Lazy.lazy2 render msg (init False False title "warning" "small")


{-| Renders a **medium warning** (normal) button with the given text.
-}
warning : String -> Json.Decode.Decoder msg -> Html.Html msg
warning title msg =
    Html.Lazy.lazy2 render msg (init False False title "warning" "medium")


{-| Renders a **big success** button with the given text.
-}
successBig : String -> Json.Decode.Decoder msg -> Html.Html msg
successBig title msg =
    Html.Lazy.lazy2 render msg (init False False title "success" "big")


{-| Renders a **small success** button with the given text.
-}
successSmall : String -> Json.Decode.Decoder msg -> Html.Html msg
successSmall title msg =
    Html.Lazy.lazy2 render msg (init False False title "success" "small")


{-| Renders a **medium success** (normal) button with the given text.
-}
success : String -> Json.Decode.Decoder msg -> Html.Html msg
success title msg =
    Html.Lazy.lazy2 render msg (init False False title "success" "medium")


{-| Renders a **big danger** button with the given text.
-}
dangerBig : String -> Json.Decode.Decoder msg -> Html.Html msg
dangerBig title msg =
    Html.Lazy.lazy2 render msg (init False False title "danger" "big")


{-| Renders a **small danger** button with the given text.
-}
dangerSmall : String -> Json.Decode.Decoder msg -> Html.Html msg
dangerSmall title msg =
    Html.Lazy.lazy2 render msg (init False False title "danger" "small")


{-| Renders a **medium danger** (normal) button with the given text.
-}
danger : String -> Json.Decode.Decoder msg -> Html.Html msg
danger title msg =
    Html.Lazy.lazy2 render msg (init False False title "danger" "medium")


{-| Creates the attributes for a button that contains events, tabindex and
classes.
-}
attributes :
    Json.Decode.Decoder msg
    -> { b | disabled : Bool, kind : String, size : String, readonly : Bool }
    -> List (Html.Attribute msg)
attributes msg model =
    let
        actions =
            Ui.enabledActions model
                [ on "click" msg
                  {-
                     , onKeys
                         [ ( 13, msg )
                         , ( 32, msg )
                         ]
                  -}
                ]
    in
        [ classList
            [ ( "disabled", model.disabled )
            , ( "readonly", model.readonly )
            , ( "ui-button-" ++ model.size, True )
            , ( "ui-button-" ++ model.kind, True )
            ]
        ]
            ++ (Ui.tabIndex model)
            ++ actions
