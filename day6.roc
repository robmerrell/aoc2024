app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pf.Stdout
import pf.File

Tile : [Floor, Object]
Guard : { state : [Active, Inactive], direction : [Right, Left, Up, Down], x : U64, y : U64 }
GameState : { map : List (List Tile), guards : List Guard, positions : List { x : U64, y : U64 } }

main =
    rulesInput = File.readUtf8! "input/day6.txt"
    gameState = solve (initGameState rulesInput)
    Stdout.line! "Total positions: $(Num.toStr (List.len gameState.positions))"

solve = \gameState ->
    newGameState = tick gameState

    # stop if all guards are inactive
    if List.any newGameState.guards \guard -> guard.state == Active then
        solve newGameState
    else
        newGameState

tick : GameState -> GameState
tick = \gameState ->
    (updatedGuards, updatedPositions) =
        List.walk gameState.guards ([], gameState.positions) \(guards, positions), guard ->
            (nextX, nextY) =
                when guard.direction is
                    Up -> (guard.x, guard.y - 1)
                    Down -> (guard.x, guard.y + 1)
                    Left -> (guard.x - 1, guard.y)
                    Right -> (guard.x + 1, guard.y)

            positionState =
                if List.any positions \pos -> pos.x == guard.x && pos.y == guard.y then
                    positions
                else
                    List.append positions { x: guard.x, y: guard.y }

            when mapTile gameState.map nextX nextY is
                Ok Floor ->
                    guardState = List.append guards { state: Active, direction: guard.direction, x: nextX, y: nextY }
                    (guardState, positionState)

                Ok Object ->
                    newDirection =
                        when guard.direction is
                            Up -> Right
                            Down -> Left
                            Left -> Up
                            Right -> Down

                    guardState = List.append guards { state: Active, direction: newDirection, x: guard.x, y: guard.y }
                    (guardState, positionState)

                Err OutOfBounds ->
                    guardState = List.append guards { state: Inactive, direction: guard.direction, x: guard.x, y: guard.y }
                    (guardState, positionState)

    { map: gameState.map, guards: updatedGuards, positions: updatedPositions }

initGameState : Str -> GameState
initGameState = \mapInput ->
    (parsedMap, parsedGuards) =
        Str.splitOn mapInput "\n"
        |> List.walkWithIndex ([], []) \(map, guards), row, y ->
            (parsedRowMap, parsedRowGuards) =
                List.walkWithIndex (toList row) ([], []) \(rowMap, rowGuards), char, x ->
                    when char is
                        "." -> (List.append rowMap Floor, rowGuards)
                        "#" -> (List.append rowMap Object, rowGuards)
                        "^" -> (List.append rowMap Floor, List.append rowGuards { state: Active, direction: Up, x: x, y: y })
                        _ -> crash "Unknown tile: $(char)"

            (List.append map parsedRowMap, List.append guards parsedRowGuards)

    uniqueGuards = flattenGuardList parsedGuards
    { map: parsedMap, guards: uniqueGuards, positions: [] }

mapTile : List (List Tile), U64, U64 -> Result Tile [OutOfBounds]
mapTile = \map, x, y ->
    row = List.get? map y
    tile = List.get? row x
    Ok tile

toList : Str -> List Str
toList = \input ->
    Str.walkUtf8 input [] \state, byte ->
        char = Result.withDefault (Str.fromUtf8 [byte]) ""
        List.append state char

flattenGuardList = \list ->
    nestedList = List.keepIf list \guard -> guard != []
    Result.withDefault (List.first nestedList) []

