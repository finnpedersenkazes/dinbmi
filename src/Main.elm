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
        [ style "font-size" "7vw"
        , style "font-family" "Helvetica Neue,Helvetica,Arial,sans-serif"
        , style "margin" "4px"
        , style "min-with" "100%"
        , style "background-size" "contain"
        ]
        ([ viewTableHead ] ++ [ viewTableBody model ])


type alias AlertColor =
    { textColor : String
    , backgroundColor : String
    , borderColor : String
    }


colorSuccess : AlertColor
colorSuccess =
    { textColor = "#3c763d"
    , backgroundColor = "#dff0d8"
    , borderColor = "#d6e9c6"
    }


colorInfo : AlertColor
colorInfo =
    { textColor = "#31708f"
    , backgroundColor = "#d9edf7"
    , borderColor = "#bce8f1"
    }


colorWarning : AlertColor
colorWarning =
    { textColor = "#8a6d3b"
    , backgroundColor = "#fcf8e3"
    , borderColor = "#faebcc"
    }


colorDanger : AlertColor
colorDanger =
    { textColor = "#a94442"
    , backgroundColor = "#f2dede"
    , borderColor = "#ebccd1"
    }


colorInput : AlertColor
colorInput =
    { textColor = colorInfo.textColor
    , backgroundColor = "#fff"
    , borderColor = colorInfo.borderColor
    }


colorHeader : AlertColor
colorHeader =
    { textColor = colorInfo.textColor
    , backgroundColor = "#eee"
    , borderColor = colorInfo.borderColor
    }


calcWeight : Float -> Model -> String
calcWeight bmi model =
    round 0 (weightCalculator bmi model.height)


viewTableHead : Html Msg
viewTableHead =
    thead []
        [ viewTableHeadLine ]


viewTableHeadLine : Html Msg
viewTableHeadLine =
    th []
        [ div
            (cellStyles colorHeader
                ++ [ style "font-size" "8vw" ]
            )
            [ text "Beregn din ideal vægt ..." ]
        ]


viewTableBody : Model -> Html Msg
viewTableBody model =
    tbody []
        [ viewTableInputRow colorInput model
        , viewTableFirstRow "Under normal:" colorInfo 18.5 model
        , viewTableRow "Normal vægt:" colorSuccess 18.5 25 model
        , viewTableRow "Overvægtig:" colorWarning 25 30 model
        , viewTableRow "Overvægtig fedme klasse 1:" colorDanger 30 35 model
        , viewTableRow "Overvægtig fedme klasse 2:" colorDanger 35 40 model
        , viewTableLastRow "Overvægtig fedme klasse 3:" colorDanger 40 model
        ]


cellStyles : AlertColor -> List (Attribute msg)
cellStyles color =
    [ style "padding" "10px"
    , style "margin-bottom" "4px"
    , style "border" "1px solid transparent"
    , style "border-radius" "3px"
    , style "background-color" color.backgroundColor
    , style "color" color.textColor
    , style "border-color" color.borderColor
    , style "width" "100%"
    ]


viewTableInputRow : AlertColor -> Model -> Html Msg
viewTableInputRow color model =
    tr []
        [ td []
            [ div (cellStyles color)
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


viewTableFirstRow : String -> AlertColor -> Float -> Model -> Html Msg
viewTableFirstRow title color bmi model =
    tr []
        [ td []
            [ div
                (cellStyles color)
                [ text title
                , br [] []
                , text "mindre end "
                , text (calcWeight bmi model)
                , text " kg."
                ]
            ]
        ]


viewTableRow : String -> AlertColor -> Float -> Float -> Model -> Html Msg
viewTableRow title color bmi1 bmi2 model =
    tr []
        [ td []
            [ div
                (cellStyles color)
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


viewTableLastRow : String -> AlertColor -> Float -> Model -> Html Msg
viewTableLastRow title color bmi model =
    tr []
        [ td []
            [ div
                (cellStyles color)
                [ text title
                , br [] []
                , text "over "
                , text (calcWeight bmi model)
                , text " kg."
                ]
            ]
        ]
