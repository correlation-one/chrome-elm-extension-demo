-- module Data.FoxBit exposing (..)
-- import Json.Decode exposing (Decoder, string, succeed, field, at)
-- import Json.Decode.Pipeline exposing (decode, required, hardcoded)
-- import Data.Currency exposing (Currency)
--
-- type alias Currency =
--   { high: String
--   , low: String
--   , vol: String
--   , last: String
--   , buy: String
--   , sell: String
--   }
--
--
-- foxBitDecoder : Decoder Currency
--   decode String
--
-- dex : Decoder Currency
-- dex =
--   decode Currency
--     |> required "high" string
--     |> required "low" string
--     |> required "vol" string
--     |> required "last" string
--     |> required "buy" string
--     |> required "sell" string
--     |> hardcoded "FoxBit"
