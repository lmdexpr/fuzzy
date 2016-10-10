import Char exposing (fromCode)
import String exposing (fromChar)

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
  , config : Maybe Temperature
  , room_color : String
  , out_color: String
  }

bounding : Temperature -> Temperature -> Temperature -> Bool
bounding mini maxi t = mini <= t && t <= maxi

out_bounding : Temperature -> Bool
out_bounding = bounding -10 45

config_bounding : Temperature -> Bool
config_bounding = bounding 17 30

init : (Model, Cmd Msg)
init =
  let
    model =
      { room = 20.0
      , out  = 20.0
      , config = Nothing
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

fuzzy_color : Float -> Bool -> String
fuzzy_color inp p =
  let
    getChr c = if c < 10 then toString c else fromChar <| fromCode (87+c)
    getStr n b = if n < b then getChr n else (toRadix16 (n//b)) ++  (getChr (n%b))
    toRadix16 n = getStr n 16
  in
    inp |> abs |> round |> toRadix16
    |> (\str -> case String.length str of
                0 -> "00"
                1 -> "0" ++ str
                _ -> str)
    |> String.repeat 2 |> (if p then (++) "ff" else \x -> x ++ "ff") |> (++) "#"

-- http://www.sist.ac.jp/~kanakubo/research/reasoning_kr/fuzzy.html
fuzzy : Model -> Model
fuzzy m =
  let
    membership_hot temp =
      if temp < 10 then 0.0 else if temp > 30 then 1.0 else temp / 20.0 - 0.5
    membership_cold temp =
      if temp < 10 then 1.0 else if temp > 30 then 0.0 else 1.5 - temp / 20.0
    diff temp = membership_hot temp - membership_cold temp
    mem_or temp = Basics.max (membership_hot temp) (membership_cold temp)
    set_color f = fuzzy_color (255 * (2 * mem_or f - 2)) (diff f > 0)
  in
    { m
    | room = 20.0
    , room_color = set_color m.room
    , out_color = set_color m.out
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    t = model.out
    step v op =
      (toFloat <| round <| 10 * (if out_bounding (op t v) then (op t v) else t)) / 10
  in
    case msg of
      Tick _ -> (fuzzy model, Cmd.none)
      Temp (Increment v) -> ({model | out = step v (+) }, Cmd.none)
      Temp (Decrement v) -> ({model | out = step v (-) }, Cmd.none)

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
          , panel NormalPanel []
            [ panelHeading (PanelH4 "Config of machine") [] []
            , panelBody []
              [ Html.text (Maybe.withDefault "Not worked" (str_of_conf model.config)) ]
            ]
          ]
        , column [Small One, Medium One] []
        ]
      ]
    ]
