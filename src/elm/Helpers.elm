module Helpers exposing (..)


pairwiseList : List a -> List a -> List ( a, a )
pairwiseList list1 list2 =
    list1
        |> List.concatMap (\list1Elem -> (List.map (\list2Elem -> ( list1Elem, list2Elem )) list2))


distinctPairwiseList : List a -> List a -> List ( a, a )
distinctPairwiseList list1 list2 =
    pairwiseList list1 list2
        |> List.filter (\( a, b ) -> a /= b)
