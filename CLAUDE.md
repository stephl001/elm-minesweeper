# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Minesweeper game built with Elm 0.19.1. The application is a single-page browser game that uses elm-ui for rendering and includes three difficulty levels (Easy, Medium, Hard).

## Build Commands

This is a pure Elm project with no npm dependencies.

### Build for production
```bash
elm make src/Main.elm --optimize --output=main.compiled.js
```
Compiles with optimization enabled to `main.compiled.js`.

### Compile for development (with debug)
```bash
elm make src/Main.elm --output=main.compiled.js
```
Compiles without optimization for development.

### Format validation
```bash
elm-format --validate .
```
Validates code formatting standards (requires elm-format to be installed globally).

## Architecture

### Single Module Design
The entire game logic is contained in `src/Main.elm` (~890 lines). There are no separate modules - all types, functions, and view logic are in this single file.

### Core Type Hierarchy
The game uses a layered type system for board representation:

1. **PreGenerationBoardCell** - Initial board generation (Uninitialized | FutureBomb)
2. **Cell** - Computed cell type (Bomb BombState | BombNeighbor Int | Empty)
3. **GameCell** - Player-visible state (Unrevealed Cell | Revealed Cell | Flagged Cell)
4. **Board** - Generic 2D array using `tortus/elm-array-2d`
5. **GameState** - Top-level state (LevelUnselected | NotStarted Height Width | Playing GameBoard | GameOver GameBoard | Completed GameBoard)

### Board Generation Flow
1. User clicks a cell when in `NotStarted` state
2. Generates random board with bombs shuffled using `Random.List.shuffle`
3. Validates that first clicked cell is Empty (regenerates if not)
4. Calculates neighbor bomb counts for all cells
5. Transitions to `Playing` state and reveals the clicked cell

### Key Game Logic
- **Cell revelation**: When Empty cell is revealed, recursively reveals all adjacent unrevealed cells (flood fill)
- **Win condition**: Checked in `gameStateFromBoard` - game is won when unrevealed cells count equals bomb count
- **Bomb explosion**: Clicking a bomb sets it to Exploded state and reveals entire board

### UI Framework
Uses `mdgriffith/elm-ui` instead of raw HTML. The view renders:
- Game header with title
- Notification bar showing game state and stats
- Board grid with click handlers for reveal and right-click for flag

### Event Handling
- Left click: RevealCell message
- Right click: FlagCell message (uses `preventDefaultOn` to suppress context menu)
- Board generation triggered on first cell click in NotStarted state

## Testing

The project has a basic test setup in `tests/Tests.elm` but currently only contains placeholder tests. Tests would use `elm-explorations/test`.

## Deployment

The compiled JavaScript (`main.compiled.js`) is checked into git and can be deployed to GitHub Pages or any static hosting service. The `index.html` file loads this compiled output and initializes the Elm app with `Elm.Main.init({ flags: null })`.

### GitHub Pages Setup
1. Build the production version: `elm make src/Main.elm --optimize --output=main.compiled.js`
2. Commit the compiled JS file to git
3. Enable GitHub Pages in repository settings, pointing to the branch with your code
4. The game will be accessible at `https://<username>.github.io/<repository-name>/`

### Context7 ###

Always use context7 when I need code generation, setup or configuration steps, or
library/API documentation. This means you should automatically use the Context7 MCP
tools to resolve library id and get library docs without me having to explicitly ask.
