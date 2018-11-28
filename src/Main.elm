module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, br, div, input, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Round exposing (round)


-- MAIN


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { content : String
    , height : Float
    }


init : Model
init =
    { content = ""
    , height = 0
    }


weightCalculator : Float -> Float -> Float
weightCalculator bmi height =
    bmi * height / 100 * height / 100



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            let
                newHeight =
                    Maybe.withDefault 0
                        (String.toFloat newContent)

                newHeightValidated =
                    if newHeight < 0 then
                        0
                    else if newHeight > 250 then
                        250
                    else
                        newHeight

                newContentValidated =
                    if newHeightValidated == 0 then
                        ""
                    else if newHeightValidated == 250 then
                        ""
                    else
                        newContent
            in
            { model
                | content = newContentValidated
                , height = newHeightValidated
            }



-- VIEW


view : Model -> Html Msg
view model =
    table
        [ class "table"
        , style "font-size" "6.5vw"
        ]
        ([ viewTableHead ] ++ [ viewTableBody model ])


type alias AlertClass =
    String


alertSuccess : AlertClass
alertSuccess =
    "alert-success"


alertInfo : AlertClass
alertInfo =
    "alert-primary"


alertWarning : AlertClass
alertWarning =
    "alert-warning"


alertDanger : AlertClass
alertDanger =
    "alert-danger"


alertHeader : AlertClass
alertHeader =
    "alert-secondary"


alertInput : AlertClass
alertInput =
    "alert-light"


calcWeight : Float -> Model -> String
calcWeight bmi model =
    round 0 (weightCalculator bmi model.height)


viewTableHead : Html Msg
viewTableHead =
    thead [ class "thead-light" ]
        [ viewTableHeadLine ]


viewTableHeadLine : Html Msg
viewTableHeadLine =
    th []
        [ div
            (alertStyle alertHeader
                ++ [ style "font-size" "8vw" ]
            )
            [ text "Beregn din ideal vægt" ]
        ]


viewTableBody : Model -> Html Msg
viewTableBody model =
    tbody []
        [ viewTableInputRow alertInput model
        , viewTableFirstRow "Under normal:" alertInfo 18.5 model
        , viewTableRow "Normal vægt:" alertSuccess 18.5 25 model
        , viewTableRow "Overvægtig:" alertWarning 25 30 model
        , viewTableRow "Overvægtig fedme klasse 1:" alertDanger 30 35 model
        , viewTableRow "Overvægtig fedme klasse 2:" alertDanger 35 40 model
        , viewTableLastRow "Overvægtig fedme klasse 3:" alertDanger 40 model
        ]


alertStyle : AlertClass -> List (Attribute msg)
alertStyle alert =
    [ class "alert"
    , class alert
    ]


viewTableInputRow : AlertClass -> Model -> Html Msg
viewTableInputRow alertClass model =
    tr []
        [ td []
            [ div (alertStyle alertClass)
                [ text "Din højde: "
                , input
                    [ style "font-size" "7vw"
                    , style "with" "auto"
                    , style "margin-left" "10px"
                    , placeholder "180"
                    , type_ "number"
                    , size 3
                    , Html.Attributes.min "100"
                    , Html.Attributes.max "300"
                    , value model.content
                    , onInput Change
                    ]
                    []
                , text " cm"
                ]
            ]
        ]


viewTableFirstRow : String -> AlertClass -> Float -> Model -> Html Msg
viewTableFirstRow title alertClass bmi model =
    tr []
        [ td []
            [ div
                (alertStyle alertClass)
                [ text title
                , br [] []
                , text "mindre end "
                , text (calcWeight bmi model)
                , text " kg."
                ]
            ]
        ]


viewTableRow : String -> AlertClass -> Float -> Float -> Model -> Html Msg
viewTableRow title alertClass bmi1 bmi2 model =
    tr []
        [ td []
            [ div
                (alertStyle alertClass)
                [ text title
                , br [] []
                , text "mellem "
                , text (calcWeight bmi1 model)
                , text " og "
                , text (calcWeight bmi2 model)
                , text " kg."
                ]
            ]
        ]


viewTableLastRow : String -> AlertClass -> Float -> Model -> Html Msg
viewTableLastRow title alertClass bmi model =
    tr []
        [ td []
            [ div
                (alertStyle alertClass)
                [ text title
                , br [] []
                , text "over "
                , text (calcWeight bmi model)
                , text " kg."
                ]
            ]
        ]
