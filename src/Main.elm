module Main exposing (Model, Msg(..), init, main, update, view)

import Array2D exposing (..)
import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import List.Extra exposing (groupsOf)
import Random



---- MODEL ----


type BoardCell
    = Uninitialized
    | Bomb
    | BombNeighbor Int
    | Empty


type alias Board =
    Array2D BoardCell


type GameBoardCell
    = Unrevealed BoardCell
    | Revealed BoardCell


type alias GameBoard =
    Array2D GameBoardCell


type alias Height =
    Int


type alias Width =
    Int


type GameState
    = NotStarted Height Width
    | Playing GameBoard
    | GameOver GameBoard
    | Completed GameBoard


boardToGameBoard : Board -> GameBoard
boardToGameBoard =
    Array2D.map Unrevealed


boardFromList : Width -> List BoardCell -> Board
boardFromList width =
    List.Extra.groupsOf width >> Array2D.fromList


gameBoardFromList : Width -> List BoardCell -> GameBoard
gameBoardFromList width =
    boardFromList width >> boardToGameBoard


type alias Row =
    Int


type alias Column =
    Int


boardCellFromGameBoardCell : GameBoardCell -> BoardCell
boardCellFromGameBoardCell cell =
    case cell of
        Unrevealed bc ->
            bc

        Revealed bc ->
            bc


reveal : Row -> Column -> GameBoard -> ( Maybe BoardCell, GameBoard )
reveal row col board =
    let
        boardCell =
            Array2D.get row col board
                |> Maybe.map boardCellFromGameBoardCell
    in
    case boardCell of
        Nothing ->
            ( boardCell, board )

        Just c ->
            ( boardCell, board |> Array2D.set row col (Revealed c) )


peek : Row -> Column -> GameBoard -> Maybe BoardCell
peek row col =
    reveal row col >> Tuple.first


isEmptyCell : Row -> Column -> GameBoard -> Bool
isEmptyCell row col board =
    let
        cell =
            peek row col board |> Maybe.withDefault Uninitialized
    in
    cell == Empty


type alias Model =
    Maybe Int


init : ( Model, Cmd Msg )
init =
    ( Nothing, Cmd.none )



---- UPDATE ----


type Msg
    = PickRandom_0_100
    | PickRandom_1000_2000
    | PickRandom_10000_100000
    | GeneratedInt Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PickRandom_0_100 ->
            ( model, random_0_100 |> Random.generate GeneratedInt )

        PickRandom_1000_2000 ->
            ( model, random_1000_2000 |> Random.generate GeneratedInt )

        PickRandom_10000_100000 ->
            ( model, random_10000_100000 |> Random.generate GeneratedInt )

        GeneratedInt i ->
            ( Just i, Cmd.none )



---- VIEW ----


random_0_100 : Random.Generator Int
random_0_100 =
    Random.int 0 100


random_1000_2000 =
    Random.int 1000 2000


random_10000_100000 =
    Random.int 10000 100000


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick PickRandom_0_100 ] [ text "Random 0-100" ]
        , button [ onClick PickRandom_1000_2000 ] [ text "Random 1000-2000" ]
        , button [ onClick PickRandom_10000_100000 ] [ text "Random 10000-100000" ]
        , h1 [] [ textFromModel model |> text ]
        ]


textFromModel : Model -> String
textFromModel model =
    case model of
        Nothing ->
            ""

        Just i ->
            String.fromInt i



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
