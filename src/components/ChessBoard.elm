module Components.ChessBoard exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List exposing (indexedMap, repeat)
import Types.Messages exposing (..)


type Piece
    = Knight
    | Target
    | Empty


type alias Location =
    ( Int, Int )


type alias ChessBoardRow =
    List Piece


type alias ChessBoard =
    List ChessBoardRow


chessBoardSize : Int
chessBoardSize =
    8


renderChessboard : Location -> Location -> Html Msg
renderChessboard knightLocation targetLocation =
    let
        chessBoard : ChessBoard
        chessBoard =
            repeat chessBoardSize (repeat chessBoardSize Empty)
                |> indexedMap
                    (\rowIndex chessRow ->
                        indexedMap
                            (\colIndex piece ->
                                if ( rowIndex, colIndex ) == knightLocation then
                                    Knight
                                else if ( rowIndex, colIndex ) == targetLocation then
                                    Target
                                else
                                    piece
                            )
                            chessRow
                    )
    in
        div [ class "chess-board" ] (indexedMap (\index chessBoardRow -> div [ class "chess-board__row" ] (renderChessboardRow chessBoardRow index)) chessBoard)


renderChessboardRow : ChessBoardRow -> Int -> List (Html Msg)
renderChessboardRow chessBoardRow rowIndex =
    indexedMap
        (\index piece ->
            case piece of
                Empty ->
                    div [ class "chess-board__cell", onClick (BoardClick ( rowIndex, index )) ] [ text "" ]

                Knight ->
                    div [ class "chess-board__cell--knight" ] [ text "" ]

                Target ->
                    div [ class "chess-board__cell--target" ] [ text "" ]
        )
        chessBoardRow


renderChessboardPieceCoordinates : ChessBoard -> Html Msg
renderChessboardPieceCoordinates chessBoard =
    div [ class "chess-board" ] [ text "hello" ]
