app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pf.Stdout
import pf.File

main =
    rulesInput = File.readUtf8! "input/day5_rules.txt"
    updatesInput = File.readUtf8! "input/day5_updates.txt"

    answer =
        validUpdates (buildRules rulesInput) updatesInput
        |> sumOfMiddlePages

    Stdout.line! "answer: $(Num.toStr answer)"

buildRules = \rulesInput ->
    Str.splitOn rulesInput "\n"
    |> List.walk (Dict.empty {}) \state, rule ->
        splitRules =
            Str.splitOn rule "|"
            |> List.keepOks Str.toI64

        before = Result.withDefault (List.get splitRules 0) 0
        after = Result.withDefault (List.get splitRules 1) 1
        Dict.update state before \possibleValue ->
            when possibleValue is
                Err Missing -> Ok [after]
                Ok value -> Ok (List.append value after)

validUpdates = \rules, updatesInput ->
    Str.trim updatesInput
    |> Str.splitOn "\n"
    |> List.walk [] \state, update ->
        if updateIsSorted rules update then
            List.append state update
        else
            state

sumOfMiddlePages = \updates ->
    List.walk updates 0 \sum, update ->
        updateAsList = toList update
        index = (Num.divCeil (List.len updateAsList) 2) - 1
        num = Result.withDefault (List.get updateAsList index) 0
        sum + num

expect
    updates = validUpdates (buildRules exampleRules) exampleUpdates
    out = sumOfMiddlePages updates
    out == 143

updateIsSorted = \rules, updateInput ->
    updateAsList = toList updateInput
    List.walk updateAsList Bool.true \updateState, checkPage ->
        pageRules = Result.withDefault (Dict.get rules checkPage) []
        numIsOrdered =
            List.walkBackwardsUntil updateAsList Bool.true \pageState, page ->
                if page == checkPage then
                    Break pageState
                else
                    contains = List.contains pageRules page
                    Continue (pageState && contains)

        updateState && numIsOrdered

expect updateIsSorted (buildRules exampleRules) "75,47,61,53,29" == Bool.true
expect updateIsSorted (buildRules exampleRules) "75,97,47,61,53" == Bool.false

toList : Str -> List I64
toList = \input ->
    Str.splitOn input ","
    |> List.keepOks Str.toI64

expect
    out = toList "75,47,61,53,29"
    out == [75, 47, 61, 53, 29]

exampleRules =
    """
    47|53
    97|13
    97|61
    97|47
    75|29
    61|13
    75|53
    29|13
    97|29
    53|29
    61|53
    97|53
    61|29
    47|13
    75|47
    97|75
    47|61
    75|61
    47|29
    75|13
    53|13
    """

exampleUpdates =
    """
    75,47,61,53,29
    97,61,53,29,13
    75,29,13
    75,97,47,61,53
    61,13,29
    97,13,75,29,47
    """
