import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Model =
  { time : Time
  , now_temp : Float
  , out_temp : Float
  }

temp_max = 50 : Float
temp_min = -10 : Float

init : (Model, Cmd Msg)
init =
  ({0, 20}, Cmd.none)

-- UPDATE
type Msg
  = Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({model | t = newTime}, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick

-- VIEW
fuzzy_color : Model -> String
fuzzy_color {_, now_temp} =

view : Model -> Html Msg
view model =
  let
    angle =
      turns (Time.inMinutes model.t)

    handX =
      toString (50 + 40 * cos angle)

    handY =
      toString (50 + 40 * sin angle)
  in
    svg [ viewBox "50 50 150 150", width "300px" ]
      [ rect [ x "100", y "100", width 50, height 50, fill (fuzzy_color model) ] []
      ]
