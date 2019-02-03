module Main exposing (GameState, Msg(..), init, main, update, view)

import Array
import Array2D exposing (..)
import Browser
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, height, htmlAttribute, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (..)
import Html.Attributes exposing (class, href, src, style)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as Decode
import List.Extra exposing (cartesianProduct, groupsOf)
import Random
import Random.List exposing (shuffle)



---- MODEL ----


type PreGenerationBoardCell
    = Uninitialized
    | FutureBomb


type BombState
    = Unexploded
    | Exploded


type Cell
    = Bomb BombState
    | BombNeighbor Int
    | Empty


type GameCell
    = Unrevealed Cell
    | Revealed Cell
    | Flagged Cell


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


type alias BombCount =
    Int


type GameState
    = LevelUnselected
    | NotStarted Height Width
    | Playing GameBoard
    | GameOver GameBoard
    | Completed GameBoard


type alias Model =
    { gameState : GameState
    }



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
            Bomb Unexploded

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


toRevealedCell : GameCell -> GameCell
toRevealedCell cell =
    case cell of
        Unrevealed c ->
            Revealed c

        Flagged c ->
            Revealed c

        Revealed _ ->
            cell


revealBoard : GameBoard -> GameBoard
revealBoard =
    mapBoard (\_ -> toRevealedCell)


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

        Flagged bc ->
            bc

        Revealed bc ->
            bc


updateCell : (Cell -> GameCell) -> ( Row, Column ) -> GameBoard -> ( Maybe Cell, GameBoard )
updateCell toGameCell ( row, col ) board =
    let
        boardCell =
            Array2D.get row col board
                |> Maybe.map boardCellFromGameBoardCell
    in
    case boardCell of
        Nothing ->
            ( boardCell, board )

        Just c ->
            ( boardCell, board |> Array2D.set row col (toGameCell c) )


reveal : ( Row, Column ) -> GameBoard -> ( Maybe Cell, GameBoard )
reveal =
    updateCell Revealed


flag : ( Row, Column ) -> GameBoard -> ( Maybe Cell, GameBoard )
flag pos board =
    let
        cellTransform =
            if isFlagedCell board pos then
                Unrevealed

            else
                Flagged
    in
    updateCell cellTransform pos board


peek : ( Row, Column ) -> GameBoard -> Maybe Cell
peek pos =
    reveal pos >> Tuple.first


isEmptyCell : ( Row, Column ) -> GameBoard -> Bool
isEmptyCell pos board =
    let
        cell =
            peek pos board |> Maybe.withDefault Empty
    in
    cell == Empty


isRevealedCell : GameBoard -> ( Row, Column ) -> Bool
isRevealedCell board pos =
    let
        cell =
            getCellOrDefault (Unrevealed Empty) pos board
    in
    case cell of
        Unrevealed _ ->
            False

        Flagged _ ->
            False

        Revealed _ ->
            True


isFlagedCell : GameBoard -> ( Row, Column ) -> Bool
isFlagedCell board pos =
    let
        cell =
            getCellOrDefault (Unrevealed Empty) pos board
    in
    case cell of
        Unrevealed _ ->
            False

        Flagged _ ->
            True

        Revealed _ ->
            False


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


getCellsCount : Board a -> Int
getCellsCount board =
    let
        height =
            Array2D.rows board

        width =
            Array2D.columns board
    in
    height * width


type alias BoardStats =
    { cellsCount : Int
    , bombsCount : Int
    , unrevealedCellsCount : Int
    }


emptyBoardStats : BoardStats
emptyBoardStats =
    BoardStats 0 0 0


statsFromCell : GameCell -> BoardStats
statsFromCell cell =
    case cell of
        Revealed (Bomb _) ->
            { cellsCount = 1, bombsCount = 1, unrevealedCellsCount = 0 }

        Revealed _ ->
            { cellsCount = 1, bombsCount = 0, unrevealedCellsCount = 0 }

        Unrevealed (Bomb _) ->
            { cellsCount = 1, bombsCount = 1, unrevealedCellsCount = 1 }

        Unrevealed _ ->
            { cellsCount = 1, bombsCount = 0, unrevealedCellsCount = 1 }

        Flagged (Bomb _) ->
            { cellsCount = 1, bombsCount = 1, unrevealedCellsCount = 1 }

        Flagged _ ->
            { cellsCount = 1, bombsCount = 0, unrevealedCellsCount = 1 }


combineStats : BoardStats -> BoardStats -> BoardStats
combineStats stats1 stats2 =
    BoardStats (stats1.cellsCount + stats2.cellsCount) (stats1.bombsCount + stats2.bombsCount) (stats1.unrevealedCellsCount + stats2.unrevealedCellsCount)


getBoardStats : GameBoard -> BoardStats
getBoardStats board =
    let
        rowCount =
            Array2D.rows board

        getRow =
            flip Array2D.getRow

        getRowOrDefault =
            getRow board >> Maybe.withDefault Array.empty

        statsFromCellsArray =
            Array.map statsFromCell >> Array.foldl combineStats emptyBoardStats
    in
    List.range 0 (rowCount - 1)
        |> List.map (getRowOrDefault >> statsFromCellsArray)
        |> List.foldl combineStats emptyBoardStats


gameStateFromBoard : GameBoard -> GameState
gameStateFromBoard board =
    let
        stats =
            getBoardStats board
    in
    if stats.bombsCount == stats.unrevealedCellsCount then
        Completed board

    else
        Playing board



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


generateRandomBoard : Height -> Width -> BombCount -> Random.Generator GameBoard
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



---- INIT ----


notStartedEasy : GameState
notStartedEasy =
    NotStarted 9 9


notStartedMedium : GameState
notStartedMedium =
    NotStarted 16 16


notStartedHard : GameState
notStartedHard =
    NotStarted 16 30


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { gameState = LevelUnselected }, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- UPDATE ----


type GameMode
    = Easy
    | Medium
    | Hard


type Msg
    = NewGame GameMode
    | GenerateEasy Row Column
    | GenerateMedium Row Column
    | GenerateHard Row Column
    | GeneratedBoard Row Column GameBoard
    | RevealCell Row Column
    | FlagCell Row Column


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateEasy row col ->
            ( model, Random.generate (GeneratedBoard row col) generateEasyRandomBoard )

        GenerateMedium row col ->
            ( model, Random.generate (GeneratedBoard row col) generateMediumRandomBoard )

        GenerateHard row col ->
            ( model, Random.generate (GeneratedBoard row col) generateHardRandomBoard )

        GeneratedBoard row col board ->
            let
                targetCell =
                    peek ( row, col ) board |> Maybe.withDefault (Bomb Unexploded)
            in
            if targetCell == Empty then
                { model | gameState = Playing board } |> update (RevealCell row col)

            else
                ( model, board |> randomGeneratorFromBoard |> Random.generate (GeneratedBoard row col) )

        NewGame mode ->
            case mode of
                Easy ->
                    ( { model | gameState = notStartedEasy }, Cmd.none )

                Medium ->
                    ( { model | gameState = notStartedMedium }, Cmd.none )

                Hard ->
                    ( { model | gameState = notStartedHard }, Cmd.none )

        RevealCell row col ->
            revealGameCell ( row, col ) model

        FlagCell row col ->
            flagGameCell ( row, col ) model


flagGameCell : ( Row, Column ) -> Model -> ( Model, Cmd Msg )
flagGameCell pos ({ gameState } as model) =
    let
        board =
            boardFromState gameState

        ( cell, newBoard ) =
            flag pos board
    in
    ( { model | gameState = Playing newBoard }, Cmd.none )


revealGameCell : ( Row, Column ) -> Model -> ( Model, Cmd Msg )
revealGameCell pos ({ gameState } as model) =
    let
        board =
            boardFromState gameState

        ( cell, newBoard ) =
            reveal pos board

        revealEmptyCells =
            revealSiblingCells pos
    in
    case cell of
        Nothing ->
            ( model, Cmd.none )

        Just c ->
            case c of
                Bomb _ ->
                    let
                        revealedBomb =
                            Revealed <| Bomb Exploded

                        updatedBoard =
                            updateCell (always revealedBomb) pos board |> Tuple.second
                    in
                    ( { model | gameState = GameOver (revealBoard updatedBoard) }, Cmd.none )

                Empty ->
                    ( { model | gameState = gameStateFromBoard newBoard } |> revealEmptyCells, Cmd.none )

                BombNeighbor _ ->
                    ( { model | gameState = gameStateFromBoard newBoard }, Cmd.none )


revealSiblingCells : ( Row, Column ) -> Model -> Model
revealSiblingCells pos ({ gameState } as model) =
    let
        board =
            boardFromState model.gameState

        isNotRevealed =
            not << isRevealedCell board
    in
    possibleNeighbors pos
        |> List.filter isNotRevealed
        |> List.foldl (\p m -> revealGameCell p m |> Tuple.first) model



---- VIEW ----


view : Model -> Browser.Document Msg
view =
    mainDisplay >> Element.layout [] >> List.singleton >> Browser.Document "Minesweeper"


mainDisplay : Model -> Element.Element Msg
mainDisplay { gameState } =
    column [ width fill ]
        [ gameHeader gameState
        , notificationBar gameState
        , boardView gameState
        ]


gameHeader : GameState -> Element.Element Msg
gameHeader _ =
    row [ width fill, centerX ] [ gameTitle ]


gameTitle : Element.Element Msg
gameTitle =
    Element.el
        [ Font.color (Element.rgb 0 0 1)
        , Font.size 40
        , Font.family
            [ Font.typeface "Comic Sans MS"
            , Font.sansSerif
            ]
        , centerX
        , padding 20
        ]
        (Element.text "Minesweeper")


notificationBar : GameState -> Element.Element Msg
notificationBar gameState =
    row
        [ width fill, Background.color (Element.rgb 0 1 0), height (px 50) ]
        [ titleFromGameState gameState |> Element.text
        , Element.text " ----- "
        , subtitleFromGameState gameState |> Element.text
        ]


boardView : GameState -> Element.Element Msg
boardView gameState =
    case gameState of
        LevelUnselected ->
            row [ width fill, centerY, spacing 30 ]
                [ Input.button [ Border.rounded 5, Border.solid ]
                    { onPress = Just (NewGame Easy)
                    , label = Element.text "Easy"
                    }
                , Input.button []
                    { onPress = Just (NewGame Medium)
                    , label = Element.text "Medium"
                    }
                , Input.button []
                    { onPress = Just (NewGame Hard)
                    , label = Element.text "Hard"
                    }
                ]

        _ ->
            drawBoard gameState
                |> column [ centerX ]
                |> List.singleton
                |> row [ width fill ]


boardFromState : GameState -> GameBoard
boardFromState state =
    case state of
        NotStarted height width ->
            generateUninitializedBoard height width

        GameOver board ->
            revealBoard board

        Playing board ->
            board

        Completed board ->
            board

        LevelUnselected ->
            generateUninitializedBoard 0 0


titleFromGameState : GameState -> String
titleFromGameState state =
    case state of
        LevelUnselected ->
            "Choose a level..."

        NotStarted _ _ ->
            "Not Started"

        Playing _ ->
            "Currently Playing"

        GameOver _ ->
            "Game Over!"

        Completed _ ->
            "Congratulations! You Won!"


boardStatsFromGameState : GameState -> BoardStats
boardStatsFromGameState =
    boardFromState >> getBoardStats


subtitleFromGameState : GameState -> String
subtitleFromGameState state =
    let
        boardStats =
            boardStatsFromGameState state
    in
    case state of
        Playing _ ->
            "Bombs: " ++ String.fromInt boardStats.bombsCount ++ " -- Unrevealed Cells: " ++ String.fromInt boardStats.unrevealedCellsCount

        _ ->
            ""


drawBoard : GameState -> List (Element.Element Msg)
drawBoard state =
    let
        board =
            boardFromState state
    in
    board |> mapRows (toTableRow state)


toTableRow : GameState -> Row -> Element.Element Msg
toTableRow state rowIndex =
    let
        board =
            boardFromState state
    in
    row [ padding 5, spacing 10 ] <| mapRowCells rowIndex (toTableCell state rowIndex) board


toTableCell : GameState -> Row -> Column -> GameCell -> Element.Element Msg
toTableCell state row col gcell =
    el
        (getButtonAttributes state row col
            ++ [ width (px 50), height (px 50), Border.solid, Border.width 1 ]
        )
        (elementFromGameCell gcell)


getButtonAttributes : GameState -> Row -> Column -> List (Element.Attribute Msg)
getButtonAttributes state row col =
    let
        board =
            boardFromState state

        generationHandler =
            generatorFromBoard board
    in
    case state of
        NotStarted _ _ ->
            [ Events.onClick (generationHandler row col) ]

        Playing _ ->
            if isRevealedCell board ( row, col ) then
                []

            else
                [ Events.onClick (RevealCell row col)
                , preventDefaultOn "contextmenu" (Decode.succeed ( FlagCell row col, True )) |> htmlAttribute
                ]

        GameOver _ ->
            []

        Completed _ ->
            []

        LevelUnselected ->
            []


generatorFromGameState : GameState -> (Row -> Column -> Msg)
generatorFromGameState =
    boardFromState >> generatorFromBoard


generatorFromBoard : GameBoard -> (Row -> Column -> Msg)
generatorFromBoard =
    getBoardWidth >> generatorFromBoardWidth


generatorFromBoardWidth : Int -> (Row -> Column -> Msg)
generatorFromBoardWidth width =
    case width of
        16 ->
            GenerateMedium

        30 ->
            GenerateHard

        _ ->
            GenerateEasy


randomGeneratorFromBoard : GameBoard -> Random.Generator GameBoard
randomGeneratorFromBoard =
    getBoardWidth >> randomGeneratorFromBoardWidth


randomGeneratorFromBoardWidth : Int -> Random.Generator GameBoard
randomGeneratorFromBoardWidth width =
    case width of
        16 ->
            generateMediumRandomBoard

        30 ->
            generateHardRandomBoard

        _ ->
            generateEasyRandomBoard


imageSrc file =
    "assets/images/" ++ file


fixedImage : String -> Element.Element Msg
fixedImage src =
    Element.image [ width (px 50), height (px 50) ] { src = src, description = "" }


elementFromGameCell : GameCell -> Element.Element Msg
elementFromGameCell gcell =
    case gcell of
        Unrevealed c ->
            Element.text ""

        Flagged c ->
            fixedImage <| imageSrc "flag.png"

        Revealed c ->
            elementFromCell c


elementFromCell : Cell -> Element.Element Msg
elementFromCell c =
    case c of
        Empty ->
            Element.el
                [ width fill, height fill, Background.color (rgb255 211 211 211) ]
                (Element.text "")

        Bomb Unexploded ->
            fixedImage <| imageSrc "bomb.png"

        Bomb Exploded ->
            fixedImage <| imageSrc "explosion.png"

        BombNeighbor i ->
            fixedImage <| imageSrc (String.fromInt i ++ ".png")



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
