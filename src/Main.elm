module Main exposing (GameState, Msg(..), init, main, update, view)

import Array
import Array2D exposing (..)
import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, disabled, href, src)
import Html.Styled.Events exposing (onClick)
import List.Extra exposing (cartesianProduct, groupsOf)
import Random
import Random.List exposing (shuffle)



---- MODEL ----


type PreGenerationBoardCell
    = Uninitialized
    | FutureBomb


type Cell
    = Bomb
    | BombNeighbor Int
    | Empty


type GameCell
    = Unrevealed Cell
    | Revealed Cell


type alias Board boardCell =
    Array2D boardCell


type alias PreGenerationBoard =
    Board PreGenerationBoardCell


type alias UnmaskedBoard =
    Board Cell


type alias GameBoard =
    Board GameCell


type alias Height =
    Int


type alias Width =
    Int


type GameState
    = NotStarted Height Width
    | Playing GameBoard
    | GameOver GameBoard
    | Completed GameBoard



--- Initialize the board here


cellNeighborsDeltas : List ( Row, Column )
cellNeighborsDeltas =
    [ ( 1, -1 ), ( 1, 0 ), ( 1, 1 ), ( 0, -1 ), ( 0, 1 ), ( -1, -1 ), ( -1, 0 ), ( -1, 1 ) ]


possibleNeighbors : ( Row, Column ) -> List ( Row, Column )
possibleNeighbors pos =
    let
        addTuples ( a, b ) ( c, d ) =
            ( a + c, b + d )
    in
    cellNeighborsDeltas |> List.map (addTuples pos)


getCellOrDefault : a -> ( Row, Column ) -> Board a -> a
getCellOrDefault defVal ( row, col ) =
    Array2D.get row col >> Maybe.withDefault defVal


getFromPregenerationBoard : ( Row, Column ) -> PreGenerationBoard -> PreGenerationBoardCell
getFromPregenerationBoard =
    getCellOrDefault Uninitialized


flip : (a -> b -> c) -> b -> a -> c
flip f y x =
    f x y


countPreGenerationNeighborCells : PreGenerationBoardCell -> ( Row, Column ) -> PreGenerationBoard -> Int
countPreGenerationNeighborCells cellType pos board =
    possibleNeighbors pos
        |> List.map (flip getFromPregenerationBoard <| board)
        |> List.filter ((==) FutureBomb)
        |> List.length


countPreGenerationNeighborBombs : ( Row, Column ) -> PreGenerationBoard -> Int
countPreGenerationNeighborBombs =
    countPreGenerationNeighborCells FutureBomb


mapBoard : (( Row, Column ) -> a -> b) -> Board a -> Board b
mapBoard f =
    Array2D.indexedMap (\row col -> f ( row, col ))


toCell : PreGenerationBoard -> ( Row, Column ) -> PreGenerationBoardCell -> Cell
toCell board pos cell =
    case cell of
        FutureBomb ->
            Bomb

        Uninitialized ->
            let
                neighborBombs =
                    countPreGenerationNeighborBombs pos board
            in
            if neighborBombs == 0 then
                Empty

            else
                BombNeighbor neighborBombs


initializeBoard : PreGenerationBoard -> UnmaskedBoard
initializeBoard board =
    board
        |> Array2D.indexedMap (\row col -> toCell board ( row, col ))


boardToGameBoard : UnmaskedBoard -> GameBoard
boardToGameBoard =
    Array2D.map Unrevealed


boardFromList : Width -> List boardCell -> Board boardCell
boardFromList width =
    List.Extra.groupsOf width >> Array2D.fromList


gameBoardFromList : Width -> List Cell -> GameBoard
gameBoardFromList width =
    boardFromList width >> boardToGameBoard


unmaskedBoardFromList : Width -> List PreGenerationBoardCell -> UnmaskedBoard
unmaskedBoardFromList width =
    boardFromList width >> initializeBoard


type alias Row =
    Int


type alias Column =
    Int


boardCellFromGameBoardCell : GameCell -> Cell
boardCellFromGameBoardCell cell =
    case cell of
        Unrevealed bc ->
            bc

        Revealed bc ->
            bc


reveal : Row -> Column -> GameBoard -> ( Maybe Cell, GameBoard )
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


peek : Row -> Column -> GameBoard -> Maybe Cell
peek row col =
    reveal row col >> Tuple.first


isEmptyCell : Row -> Column -> GameBoard -> Bool
isEmptyCell row col board =
    let
        cell =
            peek row col board |> Maybe.withDefault Empty
    in
    cell == Empty


mapRows : (Row -> a) -> GameBoard -> List a
mapRows f board =
    let
        maxIndexRow =
            Array2D.rows board - 1
    in
    List.range 0 maxIndexRow
        |> List.map f


mapRowCells : Row -> (Column -> GameCell -> a) -> GameBoard -> List a
mapRowCells row f =
    Array2D.getRow row
        >> Maybe.withDefault Array.empty
        >> Array.indexedMap f
        >> Array.toList


getBoardWidth : Board a -> Int
getBoardWidth =
    Array2D.columns



--- Board Generation Section


generateUninitializedBoard : Height -> Width -> GameBoard
generateUninitializedBoard height width =
    List.repeat (width * height) Uninitialized
        |> unmaskedBoardFromList width
        |> boardToGameBoard


generateBoardList : Height -> Width -> Int -> List PreGenerationBoardCell
generateBoardList h w bombs =
    List.repeat bombs FutureBomb ++ List.repeat (h * w - bombs) Uninitialized


unmaskedBoardFromShuffledList : Width -> List PreGenerationBoardCell -> Random.Generator UnmaskedBoard
unmaskedBoardFromShuffledList w =
    shuffle >> Random.map (unmaskedBoardFromList w)


generateRandomBoard : Height -> Width -> Int -> Random.Generator GameBoard
generateRandomBoard h w bombs =
    generateBoardList h w bombs
        |> unmaskedBoardFromShuffledList w
        |> Random.map boardToGameBoard


generateEasyRandomBoard : Random.Generator GameBoard
generateEasyRandomBoard =
    generateRandomBoard 9 9 10


generateMediumRandomBoard : Random.Generator GameBoard
generateMediumRandomBoard =
    generateRandomBoard 16 16 40


generateHardRandomBoard : Random.Generator GameBoard
generateHardRandomBoard =
    generateRandomBoard 16 30 99


init : flags -> ( GameState, Cmd Msg )
init _ =
    ( NotStarted 9 9, Cmd.none )



---- UPDATE ----


type Msg
    = Restart
    | GenerateEasy Row Column
    | GenerateMedium
    | GenerateHard
    | GeneratedBoard Row Column GameBoard


update : Msg -> GameState -> ( GameState, Cmd Msg )
update msg gameState =
    case msg of
        GenerateEasy row col ->
            ( gameState, Random.generate (GeneratedBoard row col) generateEasyRandomBoard )

        GeneratedBoard row col board ->
            let
                targetCell =
                    peek row col board |> Maybe.withDefault Bomb
            in
            if targetCell == Empty then
                ( Playing board, Cmd.none )

            else
                ( gameState, Random.generate (GeneratedBoard row col) generateEasyRandomBoard )

        Restart ->
            ( NotStarted 9 9, Cmd.none )

        _ ->
            ( NotStarted 9 9, Cmd.none )



---- VIEW ----


view : GameState -> Html Msg
view gameState =
    div []
        [ button [ onClick Restart ] [ text "New Game" ]
        , gameState |> boardFromState |> textFromBoard
        ]


boardFromState : GameState -> GameBoard
boardFromState state =
    case state of
        NotStarted height width ->
            generateUninitializedBoard height width

        Playing board ->
            board

        _ ->
            generateUninitializedBoard 0 0


textFromBoard : GameBoard -> Html Msg
textFromBoard board =
    Html.Styled.table
        [ css
            [ width (pct 80)
            , margin auto
            ]
        ]
        (toTableRows board)


toTableRows : GameBoard -> List (Html Msg)
toTableRows board =
    board |> mapRows (toTableRow board)


toTableRow : GameBoard -> Row -> Html Msg
toTableRow board row =
    tr [] <| mapRowCells row (toTableCell board row) board


toTableCell : GameBoard -> Row -> Column -> GameCell -> Html Msg
toTableCell board row col gcell =
    let
        bWidth =
            getBoardWidth board

        widthRatio =
            pct (100.0 / toFloat bWidth)
    in
    td
        [ css
            [ width widthRatio
            , position relative
            , border3 (px 1) solid (rgb 100 50 100)
            , textAlign center
            ]
        , onClick (GenerateEasy row col)
        ]
        [ textFromGameCell gcell ]


textFromGameCell : GameCell -> Html Msg
textFromGameCell gcell =
    case gcell of
        Unrevealed c ->
            text ("U-" ++ stringFromCell c)

        Revealed c ->
            text ("R-" ++ stringFromCell c)


stringFromCell : Cell -> String
stringFromCell c =
    case c of
        Empty ->
            "E"

        Bomb ->
            "B"

        BombNeighbor i ->
            "N" ++ String.fromInt i



---- PROGRAM ----


main : Program () GameState Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
