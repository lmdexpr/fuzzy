module Main exposing (..)

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
import Bootstrap.Panels exposing (..)

import Fuzzy

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
  { room : Temperature
  , out  : Temperature
  , config : Temperature
  , room_color : String
  , out_color: String
  }

bounding : Temperature -> Temperature -> Temperature -> Temperature
bounding mini maxi t = if mini > t then mini else if t > maxi then maxi else t

bounding_b : Temperature -> Temperature -> Temperature -> Bool
bounding_b mini maxi t = bounding mini maxi t == t

t_max : Temperature
t_max = 45

t_min : Temperature
t_min = -10

conf_max : Temperature
conf_max = 30

conf_min : Temperature
conf_min = 17

init : (Model, Cmd Msg)
init =
  let
    model =
      { room = 20.0
      , out  = 20.0
      , config = 20.0
      , room_color = "#ffffff"
      , out_color = "#ffffff"
      }
  in (model, Cmd.none)

-- UPDATE
type ControlVariable
  = Increment Float
  | Decrement Float

type Msg
  = Tick Time
  | Temp ControlVariable

fuzzy_update : Model -> Model
fuzzy_update m =
  let
    mem_or temp = Basics.max (Fuzzy.mem_hot temp) (Fuzzy.mem_cold temp)
    diff temp = Fuzzy.mem_hot temp - Fuzzy.mem_cold temp
    set_color v = Fuzzy.get_color (255 * (2 * mem_or v - 2)) (diff v > 0)
  in
    { m
    | room = bounding t_min t_max <| m.room + 0.1 * (m.out - m.room) + 0.2 * (m.config - m.room)
    , config = bounding conf_min conf_max <| m.config + Fuzzy.inference m.room m.out
    , room_color = set_color m.room
    , out_color = set_color m.out
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    t = model.out
    step v op =
      (toFloat <| round <| 10 * (if bounding_b t_min t_max (op t v) then (op t v) else t)) / 10
  in
    case msg of
      Tick _ -> (fuzzy_update model, Cmd.none)
      Temp (Increment v) -> ({model | out = step v (+) }, Cmd.none)
      Temp (Decrement v) -> ({model | out = step v (-) }, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick

-- VIEW
view : Model -> Html Msg
view model =
  div []
  [ bootstrap.css, bootstrap.js
  , containerFluid
    [ row [ well WellLarge [] [ h1 [] [ Html.text "Fuzzy Simulator" ] ] ]
    , row
      [ column [Small Four, Medium Four]
        [ Svg.svg [ viewBox "50 50 150 150"]--, width "300px" ]
          [ rect
            [ x "50", y "50", width "150", height "150"
            , stroke "#000000", fill model.out_color
            ]
            []
          , rect
            [ x "100", y "100", width "50", height "50"
            , stroke "#000000", fill model.room_color
            ]
            []
          ]
        ]
      , column [Small Five, Medium Five]
        [ row [ div [] [] ]
        , panel PrimaryPanel []
          [ panelHeading (PanelH4 "Temperature of out") [] []
          , panelBody [] [ Html.text (toString model.out ++ "  ℃ : " ++ model.out_color) ]
          ]
        , panel PrimaryPanel []
          [ panelBody []
            [ h4 [] [ Html.text "Controller of out's temperature" ]
            , column [Small Two, Medium Two]
              [ btn BtnDefault [] [] [ onClick (Temp <| Increment 1) ] [ Html.text "+1" ] ]
            , column [Small Three, Medium Three]
              [ btn BtnDefault [] [] [ onClick (Temp <| Increment 0.1) ] [ Html.text "+0.1" ] ]
            , column [Small Three, Medium Three]
              [ btn BtnDefault [] [] [ onClick (Temp <| Decrement 0.1) ] [ Html.text "-0.1" ] ]
            , column []
              [ btn BtnDefault [] [] [ onClick (Temp <| Decrement 1) ] [ Html.text "-1" ] ]
            ]
          ]
        , panel PrimaryPanel []
          [ panelHeading (PanelH4 "Temperature of room") [] []
          , panelBody [] [ Html.text (toString model.room ++ "  ℃ : " ++ model.room_color) ]
          ]
        , panel NormalPanel []
          [ panelHeading (PanelH4 "Config of machine") [] []
          , panelBody []
            [ Html.text (toString model.config ++ " ℃") ]
          ]
        ]
      , column [Small One, Medium One] []
      ]
    ]
  ]
