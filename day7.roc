app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pf.Stdout
import pf.File

Equation : { answer : U64, nums : List U64 }

main =
    input = File.readUtf8! "input/day7.txt"
    answer =
        parseInput input
        |> solve

    Stdout.line! "answer: $(Num.toStr answer)"

solve = \parsedInput ->
    List.walk parsedInput 0 \state, equation ->
        if isValid equation.answer equation.nums 0 then
            state + equation.answer
        else
            state

isValid = \answer, nums, total ->
    when nums is
        [] -> total == answer
        [head, .. as tail] ->
            isValid answer tail (head + total)
            || isValid answer tail (head * total)

expect
    out = isValid 190 [10, 19] 0
    out == Bool.true

expect
    out = isValid 7290 [6, 8, 6, 15] 0
    out == Bool.false

parseInput : Str -> List Equation
parseInput = \input ->
    Str.trim input
    |> Str.splitOn "\n"
    |> List.walk [] \state, line ->
        answerSplit = Str.splitOn line ":"
        answerStr = Result.withDefault (List.first answerSplit) "0"
        numStr = Result.withDefault (List.last answerSplit) ""
        answer = Result.withDefault (Str.toU64 answerStr) 0

        nums =
            Str.trim numStr
            |> Str.splitOn " "
            |> List.map \num -> Result.withDefault (Str.toU64 num) 100

        List.append state { answer: answer, nums: nums }
