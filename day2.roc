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

main =
    AoC.solve {
        year: 2024,
        day: 2,
        title: "Red-Nosed Reports",
        part1,
        part2,
    }

example =
    """
    7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    8 6 4 4 1
    1 3 6 7 9
    """

part1 : Str -> Result Str Str
part1 = \input ->
    count =
        parseInput input
        |> countSafeReports

    Ok "safe reports: $(Num.toStr count)"

expect
    out = part1 example
    out == Ok "safe reports: 2"

part2 : Str -> Result Str Str
part2 = \_ -> Ok ""

# parse the input into reports of levels
parseInput = \input ->
    Str.splitOn input "\n"
    |> List.walk [] \state, line ->
        if line != "" then
            levels =
                Str.splitOn line " "
                |> List.map \level -> Result.withDefault (Str.toI64 level) 0

            List.append state levels
        else
            state

countSafeReports = \reports ->
    List.walk reports 0 \state, report ->
        # check if the list is in ascending or descending order
        inOrder = report == (List.sortAsc report) || report == (List.sortDesc report)

        if inOrder && (reportDistancesAreValid report) then
            state + 1
        else
            state

# return Bool.true if all level distances are between 1 and 3, Bool.false if otherwise
reportDistancesAreValid = \report ->
    List.walkWithIndex report Bool.true \distanceState, level, index ->
        if index == 0 then
            Bool.true
        else
            previous =
                when List.get report (index - 1) is
                    Ok l -> l
                    Err _ -> crash "This shouldn't happen"

            diff = Num.absDiff level previous
            distanceState && (diff >= 1 && diff <= 3)

