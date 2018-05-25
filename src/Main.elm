module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Components.ChessBoard exposing (..)
import Components.KnightMoves exposing (..)
import Types.Messages exposing (..)


---- MODEL ----


type alias Model =
    { knightLocation : Location
    , targetLocation : Location
    , setKnight : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { knightLocation = ( 0, 0 )
      , targetLocation = ( 1, 2 )
      , setKnight = True
      }
    , Cmd.none
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        BoardClick location ->
            let
                newModel =
                    if model.setKnight then
                        { model
                            | knightLocation = location
                            , setKnight = not model.setKnight
                        }
                    else
                        { model
                            | targetLocation = location
                            , setKnight = not model.setKnight
                        }
            in
                ( newModel
                , Cmd.none
                )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Chess is awesome!" ]
        , renderChessboard model.knightLocation model.targetLocation
        , renderPieceCoordinates Knight model.knightLocation
        , renderPieceCoordinates Target model.targetLocation
        , renderKnightMoves model.knightLocation model.targetLocation
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
