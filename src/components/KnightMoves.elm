module Components.KnightMoves exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (class)
import List exposing (map, map2, reverse, filter, foldl, member, concatMap, concat, append, length)
import Set
import Components.ChessBoard exposing (..)
import Types.Messages exposing (..)


renderKnightMoves : Location -> Location -> Html Msg
renderKnightMoves knightLocation targetLocation =
    let
        ( moves, movesList, lastLocations ) =
            knightMovesToTarget knightLocation targetLocation

        visitedLocations =
            lastLocations
                :: movesList
                |> concat
                |> length
    in
        div [ class "knight-moves" ]
            [ div [ class "total-moves" ]
                [ text <| toString moves
                , text " moves to target"
                ]
            , div []
                (movesList
                    |> map
                        (\movesListStep ->
                            div [] [ text <| toString movesListStep ]
                        )
                )
            , div [] [ text <| toString lastLocations ]
            , div [] [ text <| toString visitedLocations ]
            ]


knightMovesToTarget : Location -> Location -> ( Int, List (List Location), List Location )
knightMovesToTarget knightLocation targetLocation =
    if ((not <| insideChessBoard knightLocation) || (not <| insideChessBoard targetLocation)) then
        ( 0, [ [] ], [] )
    else
        recursiveMovesToTarget [ [ knightLocation ] ] (possibleKnightMoves knightLocation) targetLocation


recursiveMovesToTarget : List (List Location) -> List Location -> Location -> ( Int, List (List Location), List Location )
recursiveMovesToTarget visitedList currentLocations targetLocation =
    if (length currentLocations == 0 || member targetLocation currentLocations) then
        ( length visitedList, visitedList, currentLocations )
    else
        let
            newVisitedList =
                append visitedList [ currentLocations ]

            newLocations =
                concatMap possibleKnightMoves currentLocations
                    |> Set.fromList
                    |> Set.filter
                        (\loc ->
                            not <|
                                member loc <|
                                    concat newVisitedList
                        )
                    |> Set.toList
        in
            recursiveMovesToTarget newVisitedList newLocations targetLocation


insideChessBoard : Location -> Bool
insideChessBoard ( row, col ) =
    row
        >= 0
        && row
        < chessBoardSize
        && col
        >= 0
        && col
        < chessBoardSize


addDiffToLocation : Location -> Location -> Location
addDiffToLocation ( row, col ) ( rowAdd, colAdd ) =
    ( row + rowAdd, col + colAdd )


possibleKnightMoves : Location -> List Location
possibleKnightMoves location =
    let
        movesBase =
            [ 2, 2, -2, -2, 1, -1, 1, -1 ]
    in
        reverse movesBase
            |> map2 (,) movesBase
            |> map (addDiffToLocation location)
            |> filter insideChessBoard
