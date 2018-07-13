module Main exposing (..)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader)
import Debug


-- old stuff
--import Data.Currency exposing (Currency, currenciesDecoder)
--import Html
--exposing
--( Html
--, a
--, button
--, div
--, h3
--, program
--, table
--, tbody
--, td
--, text
--, th
--, thead
--, tr
--)
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


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


triMesh : Mesh Vertex
triMesh =
    WebGL.triangles
        [ ( Vertex (vec3 0 0 0) (vec3 1 0 0)
          , Vertex (vec3 1 1 0) (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) (vec3 0 0 1)
          )
        ]


oldMesh : Mesh Vertex
oldMesh =
    WebGL.points
        [ Vertex (vec3 0 0 0) (vec3 1 0 0)
        , Vertex (vec3 1 1 0) (vec3 0 1 0)
        , Vertex (vec3 1 -1 0) (vec3 0 0 1)
        ]


perspective : Float -> Mat4
perspective t =
    Mat4.mul
        (Mat4.makePerspective 45 1 0.01 100)
        (Mat4.makeLookAt (vec3 (4 * cos t) 0 (4 * sin t)) (vec3 0 0 0) (vec3 0 1 0))


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;

        uniform mat4 perspective;
        varying vec3 vcolor;

        void main () {
            vcolor = color;
            gl_Position = perspective * vec4(position, 1.0);
            gl_PointSize = 10.0;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }

    |]


triangleVertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
triangleVertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
        }

    |]


triangleFragmentShader : Shader {} Uniforms { vcolor : Vec3 }
triangleFragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }

    |]



-- MODEL


type Msg
    = OnDelta Time


{-| Used by animation shaders
-}
type alias Uniforms =
    { perspective : Mat4 }


type alias SolarBody =
    { x : Float
    , y : Float
    , velocityX : Float
    , velocityY : Float
    , mass : Float
    , filename : String
    }


type alias SolarSystem =
    { currentTime : Time
    , numPlanets : Int
    , radius : Float
    , planets : List SolarBody
    }


type alias Force =
    { fx : Float
    , fy : Float
    }


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
        scaleFromUniverse coordinate =
            coordinate / model.radius

        toVertex : SolarBody -> Vertex
        toVertex { x, y } =
            Vertex (vec3 (scaleFromUniverse x) (scaleFromUniverse y) 0) (vec3 0 0 0)

        mesh : Mesh Vertex
        mesh =
            WebGL.points
                (List.map toVertex model.planets)
    in
        WebGL.toHtml
            [ width 400
            , height 400
            , style [ ( "display", "block" ) ]
            ]
            [ WebGL.entity
                vertexShader
                fragmentShader
                mesh
                --{ perspective = Mat4.identity }
                { perspective = perspective (model.currentTime / 1000) }
            ]



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
        [ { x = 1.496e11, y = 0.0e0, velocityX = 0.0e0, velocityY = 2.98e4, mass = 5.974e24, filename = "earth.gif" }
        , { x = 2.279e11, y = 0.0e0, velocityX = 0.0e0, velocityY = 2.41e4, mass = 6.419e23, filename = "mars.gif" }
        , { x = 5.79e10, y = 0.0e0, velocityX = 0.0e0, velocityY = 4.79e4, mass = 3.302e23, filename = "mercury.gif" }
        , { x = 0.0e0, y = 0.0e0, velocityX = 0.0e0, velocityY = 2.41e4, mass = 1.989e30, filename = "sun.gif" }
        , { x = 1.082e11, y = 0.0e0, velocityX = 0.0e0, velocityY = 3.5e4, mass = 4.869e24, filename = "venus.gif" }
        ]
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- PHYSICS


nonPlanet : SolarBody
nonPlanet =
    { x = 0
    , y = 0
    , velocityX = 0
    , velocityY = 0
    , mass = 0
    , filename = "nonPlanet"
    }


noForces : Force
noForces =
    { fx = 0, fy = 0 }


combineForces : Force -> Force -> Force
combineForces total delta =
    { total | fx = total.fx + delta.fx, fy = total.fy + delta.fy }


nudgeDist : Float
nudgeDist =
    1.0e9


nudge : SolarBody -> SolarBody
nudge body =
    { body | x = body.x + nudgeDist, y = body.y + nudgeDist }


forcesToString : List Force -> String
forcesToString fs =
    fs
        |> List.map toString
        |> String.concat


sumPlanetForces : List SolarBody -> SolarBody -> Force
sumPlanetForces otherPlanets p =
    otherPlanets
        |> List.map (gravityForce p)
        |> List.foldl combineForces noForces


updatePlanets : Time -> List SolarBody -> List SolarBody
updatePlanets dt planets =
    let
        summedForces =
            planets
                |> List.map (sumPlanetForces planets)
                |> forcesToString
                |> Debug.log "forces"
    in
        planets



--List.map
--nudge
--planets


gravitationalConstant : Float
gravitationalConstant =
    6.67e-11


gravityForce : SolarBody -> SolarBody -> Force
gravityForce body1 body2 =
    let
        dx =
            body1.x - body2.x

        dy =
            body1.y - body2.y

        coincident =
            (dx + dy) < 0.1

        rsquared =
            (dx * dx) + (dy * dy)

        r =
            sqrt rsquared

        cosTheta =
            dx / r

        sinTheta =
            dy / r

        ifNotSamePlanet val =
            if coincident then
                0
            else
                val

        -- HACK: shortcut force to zero if bodies are on top of each other
        -- This allows us to simplify comparison code by forgetting case
        -- where we compare a planets gravitic force against itself
        f =
            (gravitationalConstant * body1.mass * body2.mass) / rsquared
    in
        { fx = ifNotSamePlanet (f * cosTheta)
        , fy = ifNotSamePlanet (f * sinTheta)
        }
