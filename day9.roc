app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pf.Stdout
import pf.File

Block : [File U64, FreeSpace]

main =
    input = File.readUtf8! "input/day9.txt"
    answer =
        expandDiskMap input
        |> compact 0
        |> checksum

    Stdout.line! "answer: $(Num.toStr answer)"

expandDiskMap : Str -> List Block
expandDiskMap = \diskMap ->
    List.walkWithIndex (toList diskMap) [] \state, block, index ->
        if index % 2 == 0 then
            blockRange = List.range { start: At 0, end: Length block }
            file = Num.divCeil index 2
            blockContent = List.map blockRange \_ -> File file
            List.concat state blockContent
        else
            blockRange = List.range { start: At 0, end: Length block }
            blockContent = List.map blockRange \_ -> FreeSpace
            List.concat state blockContent

printableDisk = \expanded ->
    chars =
        List.map expanded \block ->
            when block is
                File id -> Num.toStr id
                FreeSpace -> "."
    Str.joinWith chars ""

compact = \expanded, offset ->
    len = (List.len expanded) - 1
    if offset == len || (compacted expanded) then
        expanded
    else
        index = len - offset
        block = Result.withDefault (List.get expanded index) (File 0)
        firstEmpty =
            List.findFirstIndex expanded \findBlock ->
                when findBlock is
                    FreeSpace -> Bool.true
                    _ -> Bool.false

        newExpanded =
            when firstEmpty is
                Ok emptyIndex ->
                    expanded
                    |> List.set emptyIndex block
                    |> List.set index FreeSpace

                Err _ ->
                    expanded

        compact newExpanded (offset + 1)

compacted = \expanded ->
    List.walkWithIndex expanded Bool.true \state, block, index ->
        when block is
            FreeSpace ->
                when List.get expanded (index + 1) is
                    Ok FreeSpace -> state && Bool.true
                    Ok _ -> state && Bool.false
                    Err _ -> state && Bool.true

            _ -> state && Bool.true

checksum = \expanded ->
    List.walkWithIndex expanded 0 \state, block, index ->
        when block is
            FreeSpace -> state
            File value -> state + (index * value)

expect
    out =
        expandDiskMap "12345"
        |> compact 0
        |> printableDisk

    out == "022111222......"

expect
    out = expandDiskMap "123"
    out == [File 0, FreeSpace, FreeSpace, File 1, File 1, File 1]

expect
    out = printableDisk (expandDiskMap "12345")
    out == "0..111....22222"

expect
    out = printableDisk (expandDiskMap "2333133121414131402")
    out == "00...111...2...333.44.5555.6666.777.888899"

toList : Str -> List U64
toList = \input ->
    Str.walkUtf8 input [] \state, byte ->
        char = Result.withDefault (Str.fromUtf8 [byte]) ""
        num = Result.withDefault (Str.toU64 char) 0
        List.append state num

