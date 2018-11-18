module Main exposing (GameState, Msg(..), init, main, update, view)

import Array
import Array2D exposing (..)
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Navbar as Navbar
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href, src, style)
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


type alias BombCount =
    Int


type GameState
    = NotStarted Height Width
    | Playing GameBoard
    | GameOver GameBoard
    | Completed GameBoard


type alias Model =
    { gameState : GameState
    , navbarState : Navbar.State
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


toRevealedCell : GameCell -> GameCell
toRevealedCell cell =
    case cell of
        Unrevealed c ->
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

        Revealed bc ->
            bc


reveal : ( Row, Column ) -> GameBoard -> ( Maybe Cell, GameBoard )
reveal ( row, col ) board =
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

        Revealed _ ->
            True


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
        Revealed Bomb ->
            { cellsCount = 1, bombsCount = 1, unrevealedCellsCount = 0 }

        Revealed _ ->
            { cellsCount = 1, bombsCount = 0, unrevealedCellsCount = 0 }

        Unrevealed Bomb ->
            { cellsCount = 1, bombsCount = 1, unrevealedCellsCount = 1 }

        Unrevealed _ ->
            { cellsCount = 1, bombsCount = 0, unrevealedCellsCount = 1 }


combineStats : BoardStats -> BoardStats -> BoardStats
combineStats stats1 stats2 =
    BoardStats (stats1.cellsCount + stats2.cellsCount) (stats1.bombsCount + stats2.bombsCount) (stats1.unrevealedCellsCount + stats2.unrevealedCellsCount)


getBoardStats : GameBoard -> BoardStats
getBoardStats board =
    let
        rowCount =
            Array2D.rows board
    in
    List.range 0 (rowCount - 1)
        |> List.map (\rowIndex -> Array2D.getRow rowIndex board |> Maybe.withDefault Array.empty)
        |> List.map (Array.map statsFromCell >> Array.foldl combineStats emptyBoardStats)
        |> List.foldl combineStats emptyBoardStats



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
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { gameState = notStartedEasy, navbarState = navbarState }, navbarCmd )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navbarState NavbarMsg



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
    | NavbarMsg Navbar.State


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
                    peek ( row, col ) board |> Maybe.withDefault Bomb
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

        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )


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
                Bomb ->
                    ( { model | gameState = GameOver (revealBoard board) }, Cmd.none )

                Empty ->
                    ( { model | gameState = Playing newBoard } |> revealEmptyCells, Cmd.none )

                BombNeighbor _ ->
                    ( { model | gameState = Playing newBoard }, Cmd.none )


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


view : Model -> Html Msg
view model =
    Grid.container []
        -- Wrap in a container to center the navbar
        [ CDN.stylesheet
        , Navbar.config NavbarMsg
            |> Navbar.withAnimation
            |> Navbar.collapseMedium
            -- Collapse menu at the medium breakpoint
            |> Navbar.info
            -- Customize coloring
            |> Navbar.brand
                -- Add logo to your brand with a little styling to align nicely
                [ href "#" ]
                [ img
                    [ src "assets/images/elm-bootstrap.svg"
                    , class "d-inline-block align-top"
                    , style "width" "30px"
                    ]
                    []
                , text " Minesweeper"
                ]
            |> Navbar.items
                [ Navbar.dropdown
                    -- Adding dropdowns is pretty simple
                    { id = "mydropdown"
                    , toggle = Navbar.dropdownToggle [] [ text "Nouvelle Partie" ]
                    , items =
                        [ Navbar.dropdownHeader [ text "Heading" ]
                        , Navbar.dropdownItem
                            [ href "#", onClick (NewGame Easy) ]
                            [ text "Facile (9x9)" ]
                        , Navbar.dropdownItem
                            [ href "#", onClick (NewGame Medium) ]
                            [ text "Moyen (16x16)" ]
                        , Navbar.dropdownItem
                            [ href "#", onClick (NewGame Hard) ]
                            [ text "Difficile (16x30)" ]
                        , Navbar.dropdownDivider
                        , Navbar.dropdownItem
                            [ href "#" ]
                            [ text "Options..." ]
                        ]
                    }
                ]
            |> Navbar.view model.navbarState
        , model.gameState |> viewGameState
        ]


boardFromState : GameState -> GameBoard
boardFromState state =
    case state of
        NotStarted height width ->
            generateUninitializedBoard height width

        Playing board ->
            board

        GameOver board ->
            revealBoard board

        _ ->
            generateUninitializedBoard 0 0


viewGameState : GameState -> Html Msg
viewGameState gameState =
    div []
        [ Card.config []
            |> Card.block []
                [ Block.titleH4 [] [ titleFromGameState gameState |> text ]
                , Block.text [] [ subtitleFromGameState gameState |> text ]
                ]
            |> Card.view
        , gameState |> textFromGameState
        ]


titleFromGameState : GameState -> String
titleFromGameState state =
    case state of
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


textFromGameState : GameState -> Html Msg
textFromGameState =
    toTableRows >> Grid.container []


toTableRows : GameState -> List (Html Msg)
toTableRows state =
    state |> boardFromState |> mapRows (toTableRow state)


toTableRow : GameState -> Row -> Html Msg
toTableRow state row =
    let
        board =
            boardFromState state
    in
    Grid.row [] <| mapRowCells row (toTableCell state row) board


toTableCell : GameState -> Row -> Column -> GameCell -> Grid.Column Msg
toTableCell state row col gcell =
    let
        board =
            boardFromState state

        generationHandler =
            generatorFromBoard board
    in
    Grid.col
        []
        [ Button.button
            [ Button.attrs <| getButtonAttributes state row col ]
            [ textFromGameCell gcell ]
        ]


getButtonAttributes : GameState -> Row -> Column -> List (Html.Attribute Msg)
getButtonAttributes state row col =
    let
        generationHandler =
            generatorFromGameState state
    in
    case state of
        NotStarted _ _ ->
            [ onClick (generationHandler row col) ]

        Playing board ->
            if isRevealedCell board ( row, col ) then
                []

            else
                [ onClick (RevealCell row col) ]

        GameOver _ ->
            []

        Completed _ ->
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


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
