import Html exposing (..)
import Html.Lazy exposing (lazy)
import Html.Events exposing (onClick)
import Html.App as App

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Time exposing (Time, second)

import CDN exposing (bootstrap)

import Bootstrap.Grid exposing (..)
import Bootstrap.Buttons exposing (..)
import Bootstrap.Wells exposing (..)

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Temperature = Float
type alias Model =
  { time : Time
  , inner : Temperature
  , outer : Temperature
  , config : Maybe Temperature
  , room_color : String
  }

bounding : Temperature -> Temperature -> Temperature -> Bool
bounding mini maxi t = mini <= t && t <= maxi

outer_bounding : Temperature -> Bool
outer_bounding = bounding -10 45

config_bounding : Temperature -> Bool
config_bounding = bounding 17 30

init : (Model, Cmd Msg)
init =
  let
    model =
      { time = 0
      , inner = 20
      , outer = 30
      , config = Nothing
      , room_color = "#ffffff"
      }
  in (model, Cmd.none)

-- UPDATE
type ControlVariable
  = Increment
  | Decrement

type Msg
  = Tick Time
  | Temp ControlVariable

fuzzy : Model -> Model
fuzzy m =
  let
    membership_hot temp = 0
    membership_cold temp = 0
  in
    m

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    step t = if outer_bounding t then 1 else 0
  in
    case msg of
      Tick newTime -> (fuzzy {model | time = newTime}, Cmd.none)
      Temp Increment -> ({model | outer = model.outer + step (model.outer + 1) }, Cmd.none)
      Temp Decrement -> ({model | outer = model.outer - step (model.outer - 1) }, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick

-- VIEW
view : Model -> Html Msg
view model =
  let
    str_of_conf conf = Maybe.map (\str -> str ++ " ℃") (Maybe.map toString conf)
  in
    div []
    [ bootstrap.css
    , bootstrap.js
    , containerFluid
      [ row
        [ column [Medium Six]
          [ Svg.svg [ viewBox "50 50 150 150", width "300px" ]
            [ rect
              [ x "100"
              , y "100"
              , width "50"
              , height "50"
              , stroke "#000000"
              , fill model.room_color
              ]
              []
            ]
          ]
        , column [Medium Four]
          [ div [width "300px"]
            [ hr [] []
            , div [] [Html.text ("Temperature of out: " ++ toString model.outer ++ " ℃")]
            , btn BtnDefault [] [] [ onClick (Temp Increment) ] [ Html.text "+" ]
            , btn BtnDefault [] [] [ onClick (Temp Decrement) ] [ Html.text "-" ]
            , hr [] []
            , div [] [Html.text ("Config of machine: " ++
                        Maybe.withDefault "Not worked" (str_of_conf model.config))]
            ]
          ]
        ]
      ]
    ]
