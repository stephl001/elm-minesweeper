module Main exposing (Model, Msg(..), init, main, update, view)

import Array2D exposing (..)
import Browser
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
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


generateBoardList : Height -> Width -> Int -> List PreGenerationBoardCell
generateBoardList h w bombs =
    List.repeat bombs FutureBomb ++ List.repeat (h * w - bombs) Uninitialized



{-
      gameBoardFromShuffledList : Width -> List PreGenerationBoardCell -> Random.Generator PreGenerationBoard
      gameBoardFromShuffledList w =
          shuffle >> Random.map (gameBoardFromList w)


      generateRandomBoard : Height -> Width -> Int -> Random.Generator GameBoard
      generateRandomBoard h w bombs =
          generateBoardList h w bombs
              |> gameBoardFromShuffledList w



   generateEasyRandomBoard : Random.Generator GameBoard
   generateEasyRandomBoard =
       generateRandomBoard 9 9 10


   generateMediumRandomBoard : Random.Generator GameBoard
   generateMediumRandomBoard =
       generateRandomBoard 16 16 40


   generateHardRandomBoard : Random.Generator GameBoard
   generateHardRandomBoard =
       generateRandomBoard 16 30 99
-}


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
