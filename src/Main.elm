port module Main exposing (GameState, Msg(..), init, main, update, view)

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
import Html.Attributes as HtmlAttr
import Html.Events exposing (preventDefaultOn)
import Json.Decode as Decode
import List.Extra exposing (cartesianProduct, groupsOf)
import Random
import Random.List exposing (shuffle)
import Time


-- PORTS


port celebrateVictory : () -> Cmd msg



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
    , elapsedTime : Int
    , gameStartTime : Maybe Time.Posix
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
    ( { gameState = LevelUnselected
      , elapsedTime = 0
      , gameStartTime = Nothing
      }
    , Cmd.none
    )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        Playing _ ->
            Time.every 1000 Tick

        _ ->
            Sub.none



---- UPDATE ----


type GameMode
    = Easy
    | Medium
    | Hard


type Msg
    = NewGame GameMode
    | BackToLevelSelection
    | GenerateEasy Row Column
    | GenerateMedium Row Column
    | GenerateHard Row Column
    | GeneratedBoard Row Column GameBoard
    | RevealCell Row Column
    | FlagCell Row Column
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | elapsedTime = model.elapsedTime + 1 }, Cmd.none )

        BackToLevelSelection ->
            ( { model | gameState = LevelUnselected, elapsedTime = 0, gameStartTime = Nothing }, Cmd.none )

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
                { model | gameState = Playing board, elapsedTime = 0 } |> update (RevealCell row col)

            else
                ( model, board |> randomGeneratorFromBoard |> Random.generate (GeneratedBoard row col) )

        NewGame mode ->
            let
                newState =
                    case mode of
                        Easy ->
                            notStartedEasy

                        Medium ->
                            notStartedMedium

                        Hard ->
                            notStartedHard
            in
            ( { model | gameState = newState, elapsedTime = 0, gameStartTime = Nothing }, Cmd.none )

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

        newGameState =
            gameStateFromBoard newBoard

        victoryCmd =
            case newGameState of
                Completed _ ->
                    celebrateVictory ()

                _ ->
                    Cmd.none
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
                    let
                        updatedModel =
                            { model | gameState = newGameState } |> revealEmptyCells
                    in
                    ( updatedModel, victoryCmd )

                BombNeighbor _ ->
                    ( { model | gameState = newGameState }, victoryCmd )


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
-- Color Palette


colors : { background : Element.Color, surface : Element.Color, surfaceLight : Element.Color, primary : Element.Color, primaryLight : Element.Color, success : Element.Color, danger : Element.Color, warning : Element.Color, textPrimary : Element.Color, textSecondary : Element.Color, cellUnrevealed : Element.Color, cellRevealed : Element.Color, border : Element.Color, borderLight : Element.Color }
colors =
    { background = rgb255 18 18 18
    , surface = rgb255 30 30 30
    , surfaceLight = rgb255 45 45 45
    , primary = rgb255 99 102 241
    , primaryLight = rgb255 129 140 248
    , success = rgb255 34 197 94
    , danger = rgb255 239 68 68
    , warning = rgb255 234 179 8
    , textPrimary = rgb255 243 244 246
    , textSecondary = rgb255 156 163 175
    , cellUnrevealed = rgb255 71 85 105
    , cellRevealed = rgb255 226 232 240
    , border = rgb255 71 85 105
    , borderLight = rgb255 100 116 139
    }


numberColors : { one : Element.Color, two : Element.Color, three : Element.Color, four : Element.Color, five : Element.Color, six : Element.Color, seven : Element.Color, eight : Element.Color }
numberColors =
    { one = rgb255 59 130 246
    , two = rgb255 34 197 94
    , three = rgb255 239 68 68
    , four = rgb255 30 58 138
    , five = rgb255 127 29 29
    , six = rgb255 6 182 212
    , seven = rgb255 17 24 39
    , eight = rgb255 107 114 128
    }


view : Model -> Browser.Document Msg
view =
    mainDisplay >> Element.layout [ Background.color colors.background ] >> List.singleton >> Browser.Document "Minesweeper"


mainDisplay : Model -> Element.Element Msg
mainDisplay model =
    case model.gameState of
        LevelUnselected ->
            levelSelectionScreen

        _ ->
            column [ width fill ]
                [ gameHeader model.gameState
                , notificationBar model
                , boardView model.gameState
                ]


gameHeader : GameState -> Element.Element Msg
gameHeader _ =
    row [ width fill, centerX ] [ gameTitle ]


gameTitle : Element.Element Msg
gameTitle =
    Element.el
        [ Font.color colors.primary
        , Font.size 48
        , Font.bold
        , Font.family
            [ Font.typeface "Segoe UI"
            , Font.typeface "Roboto"
            , Font.sansSerif
            ]
        , centerX
        , padding 30
        ]
        (Element.text "MINESWEEPER")


notificationBar : Model -> Element.Element Msg
notificationBar model =
    let
        gameState =
            model.gameState

        stats =
            boardStatsFromGameState gameState

        statusColor =
            case gameState of
                Completed _ ->
                    colors.success

                GameOver _ ->
                    colors.danger

                Playing _ ->
                    colors.primary

                _ ->
                    colors.textSecondary
    in
    row
        [ width fill
        , Background.color colors.surface
        , padding 20
        , spacing 30
        , Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
        , Border.color statusColor
        ]
        [ el
            [ Font.color statusColor
            , Font.size 20
            , Font.bold
            ]
            (titleFromGameState gameState |> Element.text)
        , case gameState of
            Playing _ ->
                row [ spacing 20 ]
                    [ timerDisplay model.elapsedTime
                    , statsDisplay stats
                    ]

            NotStarted _ _ ->
                Element.none

            Completed _ ->
                row [ spacing 20 ]
                    [ timerDisplay model.elapsedTime
                    , statsDisplay stats
                    ]

            GameOver _ ->
                row [ spacing 20 ]
                    [ timerDisplay model.elapsedTime
                    , statsDisplay stats
                    ]

            _ ->
                Element.none
        , case gameState of
            LevelUnselected ->
                Element.none

            _ ->
                newGameButton
        ]


timerDisplay : Int -> Element.Element Msg
timerDisplay seconds =
    let
        minutes =
            seconds // 60

        remainingSeconds =
            modBy 60 seconds

        timeString =
            String.padLeft 2 '0' (String.fromInt minutes)
                ++ ":"
                ++ String.padLeft 2 '0' (String.fromInt remainingSeconds)
    in
    row
        [ spacing 8
        , Background.color colors.surfaceLight
        , Border.rounded 6
        , padding 12
        ]
        [ el [ Font.size 18 ] (Element.text "⏱")
        , el
            [ Font.color colors.primary
            , Font.size 18
            , Font.bold
            , Font.family [ Font.monospace ]
            ]
            (Element.text timeString)
        ]


newGameButton : Element.Element Msg
newGameButton =
    Input.button
        [ Background.color colors.primary
        , Font.color colors.textPrimary
        , Font.size 14
        , Font.semiBold
        , Border.rounded 6
        , padding 12
        , alignRight
        , Element.mouseOver
            [ Background.color colors.primaryLight
            ]
        ]
        { onPress = Just BackToLevelSelection
        , label = Element.text "↺ New Game"
        }


statsDisplay : BoardStats -> Element.Element Msg
statsDisplay stats =
    row
        [ spacing 30
        , alignRight
        ]
        [ statItem "💣" (String.fromInt stats.bombsCount) colors.danger
        , statItem "🚩" (String.fromInt (stats.bombsCount - (stats.cellsCount - stats.unrevealedCellsCount - stats.bombsCount))) colors.warning
        , statItem "📦" (String.fromInt stats.unrevealedCellsCount) colors.textSecondary
        ]


statItem : String -> String -> Element.Color -> Element.Element Msg
statItem icon value color =
    row
        [ spacing 8
        , Background.color colors.surfaceLight
        , Border.rounded 6
        , padding 12
        ]
        [ el [ Font.size 18 ] (Element.text icon)
        , el
            [ Font.color color
            , Font.size 18
            , Font.bold
            ]
            (Element.text value)
        ]


levelSelectionScreen : Element.Element Msg
levelSelectionScreen =
    column
        [ width fill
        , height fill
        , centerX
        , centerY
        , spacing 60
        , padding 40
        ]
        [ -- Large Title
          el
            [ Font.color colors.primary
            , Font.size 72
            , Font.bold
            , Font.family
                [ Font.typeface "Segoe UI"
                , Font.typeface "Roboto"
                , Font.sansSerif
                ]
            , centerX
            , htmlAttribute (HtmlAttr.style "text-shadow" "0 4px 6px rgba(99, 102, 241, 0.3)")
            ]
            (Element.text "MINESWEEPER")
        , -- Subtitle
          el
            [ Font.color colors.textSecondary
            , Font.size 20
            , centerX
            , Font.light
            ]
            (Element.text "Choose your difficulty level")
        , -- Difficulty Cards
          Element.wrappedRow
            [ width fill
            , centerX
            , spacing 30
            , Element.htmlAttribute (HtmlAttr.style "justify-content" "center")
            , Element.htmlAttribute (HtmlAttr.style "max-width" "900px")
            , Element.htmlAttribute (HtmlAttr.style "margin" "0 auto")
            ]
            [ difficultyCard "EASY" "9×9" "10 mines" (NewGame Easy) colors.success "🙂"
            , difficultyCard "MEDIUM" "16×16" "40 mines" (NewGame Medium) colors.warning "😐"
            , difficultyCard "HARD" "16×30" "99 mines" (NewGame Hard) colors.danger "😰"
            ]
        ]


boardView : GameState -> Element.Element Msg
boardView gameState =
    drawBoard gameState
        |> column
            [ centerX
            , padding 20
            , htmlAttribute (HtmlAttr.style "max-width" "100vw")
            , htmlAttribute (HtmlAttr.style "overflow-x" "auto")
            ]
        |> List.singleton
        |> row
            [ width fill
            , htmlAttribute (HtmlAttr.style "overflow-x" "auto")
            ]


difficultyCard : String -> String -> String -> Msg -> Element.Color -> String -> Element.Element Msg
difficultyCard label gridSize minesInfo msg accentColor emoji =
    Input.button
        [ Background.color colors.surface
        , Font.color colors.textPrimary
        , Border.rounded 16
        , Border.width 3
        , Border.color colors.border
        , padding 35
        , width (px 240)
        , height (px 280)
        , Element.mouseOver
            [ Background.color colors.surfaceLight
            , Border.color accentColor
            , Border.glow accentColor 3
            ]
        , htmlAttribute (HtmlAttr.style "transition" "all 0.3s ease")
        , htmlAttribute (HtmlAttr.style "cursor" "pointer")
        , htmlAttribute (HtmlAttr.class "difficulty-card")
        ]
        { onPress = Just msg
        , label =
            column
                [ spacing 20
                , centerX
                , centerY
                , width fill
                ]
                [ -- Emoji Icon
                  el
                    [ Font.size 56
                    , centerX
                    ]
                    (Element.text emoji)
                , -- Label
                  el
                    [ Font.size 28
                    , Font.bold
                    , Font.color accentColor
                    , centerX
                    ]
                    (Element.text label)
                , -- Details
                  column
                    [ spacing 6
                    , Font.size 16
                    , Font.color colors.textSecondary
                    , centerX
                    , width fill
                    ]
                    [ el
                        [ centerX
                        , Font.semiBold
                        ]
                        (Element.text gridSize)
                    , el [ centerX ] (Element.text minesInfo)
                    ]
                ]
        }


difficultyButton : String -> Msg -> Element.Element Msg
difficultyButton label msg =
    Input.button
        [ Background.color colors.surface
        , Font.color colors.textPrimary
        , Font.size 18
        , Font.semiBold
        , Border.rounded 8
        , Border.width 2
        , Border.color colors.primary
        , padding 20
        , width (px 150)
        , Element.mouseOver
            [ Background.color colors.surfaceLight
            , Border.color colors.primaryLight
            ]
        ]
        { onPress = Just msg
        , label = el [ centerX ] (Element.text label)
        }


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
            "Click any cell to start"

        Playing _ ->
            "Good luck!"

        GameOver _ ->
            "💥 Game Over!"

        Completed _ ->
            "🎉 Victory!"


boardStatsFromGameState : GameState -> BoardStats
boardStatsFromGameState =
    boardFromState >> getBoardStats


subtitleFromGameState : GameState -> String
subtitleFromGameState state =
    case state of
        Completed _ ->
            "All mines cleared successfully!"

        GameOver _ ->
            "Better luck next time!"

        _ ->
            ""


drawBoard : GameState -> List (Element.Element Msg)
drawBoard state =
    let
        board =
            boardFromState state

        animationClass =
            case state of
                Completed _ ->
                    htmlAttribute (HtmlAttr.class "game-win")

                GameOver _ ->
                    htmlAttribute (HtmlAttr.class "game-loss")

                _ ->
                    htmlAttribute (HtmlAttr.class "")
    in
    board
        |> mapRows
            (\rowIndex ->
                toTableRow state rowIndex
                    |> Element.el
                        (if state == Completed board || state == GameOver board then
                            [ animationClass ]

                         else
                            []
                        )
            )


toTableRow : GameState -> Row -> Element.Element Msg
toTableRow state rowIndex =
    let
        board =
            boardFromState state
    in
    row [ padding 5, spacing 10 ] <| mapRowCells rowIndex (toTableCell state rowIndex) board


toTableCell : GameState -> Row -> Column -> GameCell -> Element.Element Msg
toTableCell state row col gcell =
    let
        -- Calculate responsive cell size based on board width
        board =
            boardFromState state

        boardWidth =
            getBoardWidth board

        cellSize =
            if boardWidth >= 30 then
                -- Hard mode: smaller cells for wide boards
                px 35

            else if boardWidth >= 16 then
                -- Medium mode
                px 40

            else
                -- Easy mode
                px 50

        baseStyles =
            [ width cellSize
            , height cellSize
            , Border.rounded 4
            , htmlAttribute (HtmlAttr.class "game-cell")
            ]

        cellStyles =
            case gcell of
                Unrevealed _ ->
                    [ Background.color colors.cellUnrevealed
                    , Border.width 3
                    , Border.color colors.borderLight
                    , Border.shadow
                        { offset = ( 0, 4 )
                        , size = 0
                        , blur = 0
                        , color = rgba255 0 0 0 0.3
                        }
                    ]

                Flagged _ ->
                    [ Background.color colors.cellUnrevealed
                    , Border.width 3
                    , Border.color colors.warning
                    , Border.shadow
                        { offset = ( 0, 4 )
                        , size = 0
                        , blur = 0
                        , color = rgba255 0 0 0 0.3
                        }
                    ]

                Revealed _ ->
                    [ Background.color colors.cellRevealed
                    , Border.width 1
                    , Border.color colors.border
                    , Border.shadow
                        { offset = ( 0, 1 )
                        , size = 0
                        , blur = 0
                        , color = rgba255 0 0 0 0.1
                        }
                    , htmlAttribute (HtmlAttr.class "cell-reveal")
                    ]

        hoverStyles =
            case state of
                Playing _ ->
                    case gcell of
                        Unrevealed _ ->
                            [ Element.mouseOver
                                [ Background.color colors.surfaceLight
                                , Border.glow colors.primary 2
                                ]
                            ]

                        Flagged _ ->
                            [ Element.mouseOver
                                [ Border.glow colors.warning 2
                                ]
                            ]

                        _ ->
                            []

                NotStarted _ _ ->
                    [ Element.mouseOver
                        [ Background.color colors.surfaceLight
                        , Border.glow colors.primary 2
                        ]
                    ]

                _ ->
                    []
    in
    el
        (getButtonAttributes state row col
            ++ baseStyles
            ++ cellStyles
            ++ hoverStyles
        )
        (elementFromGameCell gcell)


rgba255 : Int -> Int -> Int -> Float -> Element.Color
rgba255 r g b a =
    Element.rgba255 r g b a


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
                [ width fill, height fill, Background.color colors.cellRevealed ]
                (Element.text "")

        Bomb Unexploded ->
            fixedImage <| imageSrc "bomb.png"

        Bomb Exploded ->
            fixedImage <| imageSrc "explosion.png"

        BombNeighbor i ->
            Element.el
                [ width fill
                , height fill
                , Background.color colors.cellRevealed
                ]
                (el
                    [ centerX
                    , centerY
                    , Font.size 24
                    , Font.bold
                    , Font.color (colorForNumber i)
                    ]
                    (Element.text (String.fromInt i))
                )


colorForNumber : Int -> Element.Color
colorForNumber n =
    case n of
        1 ->
            numberColors.one

        2 ->
            numberColors.two

        3 ->
            numberColors.three

        4 ->
            numberColors.four

        5 ->
            numberColors.five

        6 ->
            numberColors.six

        7 ->
            numberColors.seven

        8 ->
            numberColors.eight

        _ ->
            colors.textPrimary



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
