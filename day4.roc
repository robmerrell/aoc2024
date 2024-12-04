app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pf.Stdout
import pf.File

main =
    answer =
        File.readUtf8! "input/day4.txt"
        |> solve

    Stdout.line! "answer: $(Num.toStr answer)"

solve = \input ->
    listInput = toList input
    List.walkWithIndex listInput 0 \state, line, y ->
        lineCount =
            List.walkWithIndex line 0 \lineState, letter, x ->
                if letter == "X" then
                    lineState + (countWords listInput x y)
                else
                    lineState

        state + lineCount

toList = \input ->
    Str.splitOn input "\n"
    |> List.walk [] \state, line ->
        expandedLine =
            Str.walkUtf8 line [] \lineState, byte ->
                char = Result.withDefault (Str.fromUtf8 [byte]) ""
                List.append lineState char
        List.append state expandedLine

countWords = \puzzle, x, y ->
    right = wordInDirection puzzle x y 1 0
    rightdown = wordInDirection puzzle x y 1 1
    rightup = wordInDirection puzzle x y 1 -1
    left = wordInDirection puzzle x y -1 0
    leftdown = wordInDirection puzzle x y -1 1
    leftup = wordInDirection puzzle x y -1 -1
    down = wordInDirection puzzle x y 0 1
    up = wordInDirection puzzle x y 0 -1
    right + rightdown + rightup + left + leftdown + leftup + down + up

wordInDirection : List (List Str), U64, U64, I64, I64 -> U64
wordInDirection = \puzzle, x, y, stepX, stepY ->
    word =
        List.walk [1, 2, 3] [] \state, count ->
            lineOffset = Num.toI64 y + (stepY * count)
            charOffset = Num.toI64 x + (stepX * count)

            line = Result.withDefault (List.get puzzle (Num.toU64 lineOffset)) []
            char = Result.withDefault (List.get line (Num.toU64 charOffset)) ""

            List.append state char

    if word == ["M", "A", "S"] then
        1
    else
        0

example =
    """
    MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAMM
    MSAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX
    """

expect
    out = solve example
    out == 18
