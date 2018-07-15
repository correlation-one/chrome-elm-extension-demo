module Render.Main exposing (..)

import Time exposing (Time)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Mesh, Shader)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)


type alias Uniforms =
    { perspective : Mat4 }


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


{-| XY coordinates ranging from -1 to 1
-}
type alias Drawable =
    { x : Float, y : Float }


toVertex : { x : Float, y : Float } -> Vertex
toVertex { x, y } =
    Vertex (vec3 x y 0) (vec3 0 0 0)


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


view : List Drawable -> Html msg
view elements =
    let
        renderMesh : Mesh Vertex
        renderMesh =
            WebGL.points
                (List.map toVertex elements)

        wegblContainer mesh =
            WebGL.toHtml
                [ width 400
                , height 400
                , style [ ( "display", "block" ) ]
                ]
                [ WebGL.entity
                    vertexShader
                    fragmentShader
                    mesh
                    { perspective = Mat4.identity }

                --{ perspective = perspective (model.currentTime / 1000) }
                ]
    in
        wegblContainer renderMesh
