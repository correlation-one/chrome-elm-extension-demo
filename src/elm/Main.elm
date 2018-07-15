module Main exposing (..)

import AnimationFrame
import Html exposing (Html)
import Time exposing (Time)
import Physics.Space exposing (SolarBody, SolarSystem, updatePlanets)
import Render.Main exposing (Drawable)
import Html
    exposing
        ( Html
        , a
        , button
        , div
        , h3
        , program
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )


-- old stuff
--import Data.Currency exposing (Currency, currenciesDecoder)
--import Http


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- ANIMATION
-- MODEL


type Msg
    = OnDelta Time


type alias Model =
    SolarSystem



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnDelta elapsed ->
            ( { model
                | currentTime = elapsed + model.currentTime
                , planets = (updatePlanets elapsed model.planets)
              }
            , Cmd.none
            )



-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


view : Model -> Html msg
view model =
    let
        planetToDrawable : SolarBody -> Drawable
        planetToDrawable { x, y } =
            { x = x / model.radius, y = y / model.radius }

        webglView =
            model.planets
                |> List.map planetToDrawable
                |> Render.Main.view
    in
        div [] [ webglView ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs OnDelta



-- INIT


model : SolarSystem
model =
    { currentTime = 0
    , numPlanets = 5
    , radius = 2.5e11
    , planets =
        [ { x = 0.0e0, y = 0.0e0, velocityX = 0.0e0, velocityY = 0, mass = 1.989e30, name = "sun.gif" }
        , { x = 5.79e10, y = 0.0e0, velocityX = 0.0e0, velocityY = 4.79e4, mass = 3.302e23, name = "mercury.gif" }
        , { x = 1.082e11, y = 0.0e0, velocityX = 0.0e0, velocityY = 3.5e4, mass = 4.869e24, name = "venus.gif" }
        , { x = 1.496e11, y = 0.0e0, velocityX = 0.0e0, velocityY = 2.98e4, mass = 5.974e24, name = "earth.gif" }
        , { x = 2.279e11, y = 0.0e0, velocityX = 0.0e0, velocityY = 2.41e4, mass = 6.419e23, name = "mars.gif" }
        ]
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )
