module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, input, table, td, text, tr)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (keyCode, on, onClick)
import Json.Decode as Json



-- MAIN

main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


-- MODEL

type Operator
    = Plus
    | Minus
    | Star
    | Slash


operatorLabel : Operator -> String
operatorLabel operator =
    case operator of
        Plus -> "+"
        Minus -> "-"
        Star -> "*"
        Slash -> "/"


type Input
    = OperandValue Int
    | OperatorValue Operator

type ModelStates = 
    Empty
    | JustFirstOperand Int
    | FirstOperandAndOperator Int Operator
    | FirstOperandAndOperatorAndSecondOperand Int Operator Int


type alias Model =
    { state: ModelStates }


init : Model
init =
    { state = Empty }



-- UPDATE


newExpression : Model -> Input -> Model
newExpression model newValue =
    case newValue of 
    OperandValue x -> 
        case model.state of 
            Empty -> 
                {model | state = JustFirstOperand x}
            JustFirstOperand first_operand -> 
                {model | state = JustFirstOperand (first_operand*10 + x)}
            FirstOperandAndOperator first_operand operator -> 
                {model | state = FirstOperandAndOperatorAndSecondOperand first_operand operator x}
            FirstOperandAndOperatorAndSecondOperand first_operand operator second_operand -> 
                {model | state = FirstOperandAndOperatorAndSecondOperand first_operand operator (second_operand*10 + x)}
    
    OperatorValue x ->
        case model.state of
            Empty -> 
                model
            JustFirstOperand first_operand -> 
                {model | state = FirstOperandAndOperator first_operand x}
            FirstOperandAndOperator first_operand _ -> 
                {model | state = FirstOperandAndOperator first_operand x}
            FirstOperandAndOperatorAndSecondOperand first_operand operator second_operand -> 
                {model | state = FirstOperandAndOperator (finalResult first_operand second_operand operator) x}


finalResult: Int -> Int -> Operator -> Int
finalResult a b operator =
    case operator of
        Plus -> a + b
        Minus -> a - b
        Star -> a * b
        Slash -> round (toFloat a / toFloat b)


calculateExpression : Model -> Model
calculateExpression model =
    case model.state of
        Empty ->
            model
        JustFirstOperand _ ->
            model
        FirstOperandAndOperator _ _ ->
            model
        FirstOperandAndOperatorAndSecondOperand first_operand operator second_operand -> 
            {model | state = JustFirstOperand (finalResult first_operand second_operand operator)}
            

displayExpression: Model -> String
displayExpression model =
    case model.state of
         Empty -> ""
         JustFirstOperand first_operand -> String.fromInt first_operand
         FirstOperandAndOperator first_operand operator -> String.join "" [String.fromInt first_operand, operatorLabel operator]
         FirstOperandAndOperatorAndSecondOperand first_operand operator second_operand -> String.join "" [String.fromInt first_operand, operatorLabel operator, String.fromInt second_operand]


type Msg
    = InputValue Input
    | Calculate
    | Clear
    | KeyDown Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputValue value ->
            newExpression model value

        Calculate ->
            calculateExpression model

        Clear ->
            { model | state = Empty}

        KeyDown key ->
            if key == 13 then
                calculateExpression model

            else
                model



-- VIEW


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
        [ table []
            [ tr []
                [ input [ placeholder "", value (displayExpression model), onKeyDown KeyDown ] []
                ]
            , tr []
                [ tr []
                    [ td [] [ button [ onClick (InputValue (OperandValue 7)) ] [ text "7" ] ]
                    , td [] [ button [ onClick (InputValue (OperandValue 8)) ] [ text "8" ] ]
                    , td [] [ button [ onClick (InputValue (OperandValue 9)) ] [ text "9" ] ]
                    , td [] [ button [ onClick (InputValue (OperatorValue Slash)) ] [ text "/" ] ]
                    ]
                , tr
                    []
                    [ td [] [ button [ onClick (InputValue (OperandValue 4)) ] [ text "4" ] ]
                    , td [] [ button [ onClick (InputValue (OperandValue 5)) ] [ text "5" ] ]
                    , td [] [ button [ onClick (InputValue (OperandValue 6)) ] [ text "6" ] ]
                    , td [] [ button [ onClick (InputValue (OperatorValue Star)) ] [ text "*" ] ]
                    ]
                , tr
                    []
                    [ td [] [ button [ onClick (InputValue (OperandValue 1)) ] [ text "1" ] ]
                    , td [] [ button [ onClick (InputValue (OperandValue 2)) ] [ text "2" ] ]
                    , td [] [ button [ onClick (InputValue (OperandValue 3)) ] [ text "3" ] ]
                    , td [] [ button [ onClick (InputValue (OperatorValue Minus)) ] [ text "-" ] ]
                    ]
                , tr
                    []
                    [ td [] [ button [ onClick (InputValue (OperandValue 0)) ] [ text "0" ] ]
                    , td [] [ button [ onClick Calculate ] [ text "=" ] ]
                    , td [] [ button [ onClick (InputValue (OperatorValue Plus)) ] [ text "+" ] ]
                    , td [] [ button [ onClick Clear ] [ text "CLR" ] ]
                    ]
                ]
            ]
        ]
