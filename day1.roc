app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    aoc: "https://github.com/lukewilliamboswell/aoc-template/releases/download/0.2.0/tlS1ZkwSKSB87_3poSOXcwHyySe0WxWOWQbPmp7rxBw.tar.br",
}

import pf.Stdin
import pf.Stdout
import pf.Utc
import aoc.AoC {
    stdin: Stdin.readToEnd,
    stdout: Stdout.write,
    time: \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch,
}

example =
    """
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3
    """

main =
    AoC.solve {
        year: 2024,
        day: 1,
        title: "Historian Hysteria",
        part1,
        part2,
    }

part1 : Str -> Result Str Str
part1 = \input ->
    when parseInput input is
        Ok lists ->
            sortedLists = { left: List.sortAsc lists.left, right: List.sortAsc lists.right }
            distances = List.map2 sortedLists.left sortedLists.right \left, right -> Num.abs (right - left)
            total = List.sum distances

            Ok "Total distance: $(Num.toStr total)"

        Err msg ->
            Err msg

expect part1 example == Ok "Total distance: 11"
expect part1 "1   2\nblah" == Err "Failed to parse line: blah"

part2 : Str -> Result Str Str
part2 = \input ->
    when parseInput input is
        Ok lists ->
            total = List.walk lists.left 0 \state, leftItem ->
                countInRightList = List.countIf lists.right \r -> r == leftItem
                state + leftItem * (Num.toI64 countInRightList)

            Ok "Similarity score: $(Num.toStr total)"

        Err msg ->
            Err msg

expect
    out = part2 example
    out == Ok "Similarity score: 31"

# Parse the input into a record of {left, right} numbers
parseInput : Str -> Result { left : List I64, right : List I64 } Str
parseInput = \input ->
    input
    |> Str.splitOn "\n"
    |> List.walkTry { left: [], right: [] } \state, line ->
        if line != "" then
            split = line |> Str.trim |> Str.splitOn "   "

            first = Result.try (List.get split 0) Str.toI64
            last = Result.try (List.get split 1) Str.toI64

            when [first, last] is
                [Ok left, Ok right] -> Ok { left: List.append state.left left, right: List.append state.right right }
                _ -> Err "Failed to parse line: $(line)"
        else
            Ok state

