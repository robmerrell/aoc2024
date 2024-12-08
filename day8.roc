app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pf.Stdout
import pf.File

Coords : { x : I64, y : I64 }
Map : { boundaries : Coords, nodes : Dict Str (List Coords) }

main =
    input = File.readUtf8! "input/day8.txt"
    answer =
        generateMap input
        |> findAntiNodes
        |> List.len

    map = generateMap input
    antiNodes = findAntiNodes map
    p = printableMap map antiNodes
    Stdout.line! p

    Stdout.line! "answer: $(Num.toStr answer)"

printableMap = \map, antiNodes ->
    base =
        List.map (List.range { start: At 0, end: Length ((Num.toU64 map.boundaries.y) + 1) }) \row ->
            List.map (List.range { start: At 0, end: Length ((Num.toU64 map.boundaries.x) + 1) }) \col ->
                when List.findFirst antiNodes \node -> node == { x: col, y: row } is
                    Ok _ -> "#"
                    Err _ -> "."

    List.walk base "" \state, row ->
        cols = List.walk row "" \colState, col -> Str.concat colState col
        Str.concat state "\n"
        |> Str.concat cols

findAntiNodes = \map ->
    allAntiNodes =
        Dict.walk map.nodes [] \state, _char, coordinates ->
            # walk the nodes for char twice so we can compare each of them together
            antiNodes =
                List.walkWithIndex coordinates [] \nodeState, coord, nodeIndex ->
                    cmp =
                        List.walkWithIndex coordinates [] \cmpState, cmpCoord, cmpIndex ->
                            if cmpIndex == nodeIndex then
                                cmpState
                            else
                                rise = cmpCoord.y - coord.y
                                run = cmpCoord.x - coord.x
                                cmpAntiNodes = [
                                    { x: coord.x - run, y: coord.y - rise },
                                    { x: cmpCoord.x + run, y: cmpCoord.y + rise },
                                ]
                                List.concat cmpState cmpAntiNodes

                    List.concat nodeState cmp

            # remove antinodes that are out of bounds
            inBoundsAntiNodes = List.keepIf antiNodes \antiNode -> inBounds map.boundaries antiNode
            List.concat state inBoundsAntiNodes

    List.walk allAntiNodes [] \uniqueState, node ->
        if List.contains uniqueState node then
            uniqueState
        else
            List.append uniqueState node

inBounds = \boundaries, coords ->
    (coords.x >= 0 && coords.x <= boundaries.x) && (coords.y >= 0 && coords.y <= boundaries.y)

generateMap : Str -> Map
generateMap = \input ->
    # boundaries
    rows =
        Str.trim input
        |> Str.splitOn "\n"
    first = Result.withDefault (List.first rows) ""

    xBoundary = (List.len (toList first)) - 1
    yBoundary = (List.len rows) - 1

    # nodes
    nodes =
        List.walk (parseNodes input) (Dict.empty {}) \state, node ->
            Dict.update state node.char \possibleValue ->
                when possibleValue is
                    Err Missing -> Ok [{ x: node.x, y: node.y }]
                    Ok value -> Ok (List.append value { x: node.x, y: node.y })

    { boundaries: { x: Num.toI64 xBoundary, y: Num.toI64 yBoundary }, nodes: nodes }

parseNodes : Str -> List { char : Str, x : I64, y : I64 }
parseNodes = \input ->
    Str.trim input
    |> Str.splitOn "\n"
    |> List.walkWithIndex [] \rowState, row, y ->
        rowNodes =
            List.walkWithIndex (toList row) [] \colState, char, x ->
                when char is
                    "#" -> colState
                    "." -> colState
                    _ -> List.append colState { char: char, x: Num.toI64 x, y: Num.toI64 y }

        List.concat rowState rowNodes

toList : Str -> List Str
toList = \input ->
    Str.trim input
    |> Str.walkUtf8 [] \state, byte ->
        char = Result.withDefault (Str.fromUtf8 [byte]) ""
        List.append state char

example =
    """
    ......#....#
    ...#....0...
    ....#0....#.
    ..#....0....
    ....0....#..
    .#....A.....
    ...#........
    #......#....
    ........A...
    .........A..
    ..........#.
    ..........#.
    """

# example =
#     """
#     ..........
#     ...#......
#     #.........
#     ....a.....
#     ........a.
#     .....a....
#     ..#.......
#     ......#...
#     ..........
#     ..........
#     """

# example =
#     """
#     ..........
#     ...#......
#     #.........
#     ....a.....
#     ........a.
#     .....a....
#     ..#.......
#     ......A...
#     ..........
#     ..........
#     """

expect
    answer = (generateMap example |> findAntiNodes |> List.len)
    answer == 14
