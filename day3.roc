app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pf.Stdout
import pf.File

acceptedChars = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ","]
example = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

main =
    answer =
        File.readUtf8! "input/day3.txt"
        |> solve

    Stdout.line! "answer: $(Num.toStr answer)"

solve : Str -> I64
solve = \input ->
    parseParameters input
    |> List.map parametersToPair
    |> List.walk 0 \state, pair ->
        (Pair first second) = pair
        state + (first * second)

expect solve example == 161

# parses out the inner parameters for 'mul(...)' as a string
# "mul(2,4)mul(5,8)" -> ["2,4", "5,8"]
parseParameters : Str -> List Str
parseParameters = \input ->
    Str.splitOn input "mul("
    |> List.dropIf \chunk -> !(Str.contains chunk ")")
    |> List.walk [] \state, chunk ->
        pairStr =
            List.walkUntil (strToList chunk) "" \chunkState, char ->
                if List.contains acceptedChars char then
                    Continue (Str.concat chunkState char)
                else if char == ")" then
                    Break chunkState
                else
                    Break ""

        if pairStr != "" then
            List.append state pairStr
        else
            state

expect parseParameters example == ["2,4", "5,5", "11,8", "8,5"]

# convert the string parameters to number pairs.
# "11,5" -> Pair 11 5
parametersToPair : Str -> [Pair I64 I64]
parametersToPair = \params ->
    split = Str.splitOn params ","

    first = Result.try (List.get split 0) Str.toI64
    second = Result.try (List.get split 1) Str.toI64

    when [first, second] is
        [Ok a, Ok b] -> Pair a b
        _ -> crash "Unable to convert parameters to pair $(params)"

expect parametersToPair "11,8" == Pair 11 8

# convert a string to a list of strings. I'm worried that since this isn't already
# in the stdlib this isn't a thing that I should be doing...
strToList : Str -> List Str
strToList = \str ->
    Str.walkUtf8 str [] \state, byte ->
        char = Result.withDefault (Str.fromUtf8 [byte]) "!"
        List.append state char
