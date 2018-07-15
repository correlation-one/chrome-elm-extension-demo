module Physics.Space exposing (..)

import Time exposing (Time)


type alias SolarBody =
    { x : Float
    , y : Float
    , velocityX : Float
    , velocityY : Float
    , mass : Float
    , id : String
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



--type SolarBodyType
--= Sun
--| Mercury
--| Venus
--| Earth
--| Mars
--| Other


updatePlanets : Time -> List SolarBody -> List SolarBody
updatePlanets dt planets =
    let
        planetForces =
            planets
                |> List.map (\body -> calculateGravityForce body planets)

        forcePlanetPairs =
            List.map2 (,) planetForces planets

        scaledDT =
            dt * 1000
    in
        List.map (\( force, body ) -> (stepBody force scaledDT body)) forcePlanetPairs


calculateGravityForce : SolarBody -> List SolarBody -> Force
calculateGravityForce targetBody allBodies =
    allBodies
        |> List.filter ((/=) targetBody)
        |> List.map (gravityForce targetBody)
        |> List.foldl combineForces noForces


gravitationalConstant : Float
gravitationalConstant =
    6.67e-11


gravityForce : SolarBody -> SolarBody -> Force
gravityForce body1 body2 =
    let
        dx =
            body2.x - body1.x

        dy =
            body2.y - body1.y

        rsquared =
            (dx * dx) + (dy * dy)

        r =
            sqrt rsquared

        cosTheta =
            dx / r

        sinTheta =
            dy / r

        f =
            (gravitationalConstant * body1.mass * body2.mass) / rsquared
    in
        { fx = f * cosTheta
        , fy = f * sinTheta
        }


stepBody : Force -> Time -> SolarBody -> SolarBody
stepBody { fx, fy } dt body =
    let
        ( ax, ay ) =
            ( fx / body.mass, fy / body.mass )

        ( vx, vy ) =
            ( body.velocityX + ax * dt, body.velocityY + ay * dt )

        ( px, py ) =
            ( body.x + vx * dt, body.y + vy * dt )
    in
        { body | x = px, y = py, velocityX = vx, velocityY = vy }


nudge : SolarBody -> SolarBody
nudge body =
    { body | x = body.x + nudgeDist, y = body.y + nudgeDist }


forcesToString : List Force -> String
forcesToString fs =
    fs
        |> List.map toString
        |> String.concat


nonPlanet : SolarBody
nonPlanet =
    { x = 0
    , y = 0
    , velocityX = 0
    , velocityY = 0
    , mass = 0
    , id = "other"
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
