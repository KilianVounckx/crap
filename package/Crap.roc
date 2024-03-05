interface Crap
    exposes [
        parseArgs,
    ]
    imports []

# This package is heavily based on
# https://github.com/lukewilliamboswell/roc-json/blob/main/package/Core.roc

Crap := {}
    implements [
        DecoderFormatting {
            u8: decodeU8,
            u16: decodeU16,
            u32: decodeU32,
            u64: decodeU64,
            u128: decodeU128,
            i8: decodeI8,
            i16: decodeI16,
            i32: decodeI32,
            i64: decodeI64,
            i128: decodeI128,
            f32: decodeF32,
            f64: decodeF64,
            dec: decodeDec,
            bool: decodeBool,
            string: decodeString,
            list: decodeList,
            record: decodeRecord,
            tuple: decodeTuple,
        },
    ]

parseArgs = \args ->
    args
    |> prepareBytes
    |> Decode.fromBytes (@Crap {})

prepareBytes : List Str -> List U8
prepareBytes = \args ->
    args
    |> List.dropFirst 1 # drop process name
    |> List.joinMap \s ->
        s
        |> Str.toUtf8
        |> List.append 255

decodeU8 = Decode.custom \bytes, @Crap {} ->
    { taken, rest } = takeNumber bytes
    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toU8
        |> Result.mapErr \_ -> TooShort
    { result, rest }

decodeU16 = Decode.custom \bytes, @Crap {} ->
    { taken, rest } = takeNumber bytes
    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toU16
        |> Result.mapErr \_ -> TooShort
    { result, rest }

decodeU32 = Decode.custom \bytes, @Crap {} ->
    { taken, rest } = takeNumber bytes
    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toU32
        |> Result.mapErr \_ -> TooShort
    { result, rest }

decodeU64 = Decode.custom \bytes, @Crap {} ->
    { taken, rest } = takeNumber bytes
    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toU64
        |> Result.mapErr \_ -> TooShort
    { result, rest }

decodeU128 = Decode.custom \bytes, @Crap {} ->
    { taken, rest } = takeNumber bytes
    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toU128
        |> Result.mapErr \_ -> TooShort
    { result, rest }

decodeI8 = Decode.custom \bytes, @Crap {} ->
    { taken, rest } = takeNumber bytes
    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toI8
        |> Result.mapErr \_ -> TooShort
    { result, rest }

decodeI16 = Decode.custom \bytes, @Crap {} ->
    { taken, rest } = takeNumber bytes
    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toI16
        |> Result.mapErr \_ -> TooShort
    { result, rest }

decodeI32 = Decode.custom \bytes, @Crap {} ->
    { taken, rest } = takeNumber bytes
    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toI32
        |> Result.mapErr \_ -> TooShort
    { result, rest }

decodeI64 = Decode.custom \bytes, @Crap {} ->
    { taken, rest } = takeNumber bytes
    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toI64
        |> Result.mapErr \_ -> TooShort
    { result, rest }

decodeI128 = Decode.custom \bytes, @Crap {} ->
    { taken, rest } = takeNumber bytes
    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toI128
        |> Result.mapErr \_ -> TooShort
    { result, rest }

decodeDec = Decode.custom \bytes, @Crap {} ->
    { taken, rest } = takeNumber bytes
    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toDec
        |> Result.mapErr \_ -> TooShort
    { result, rest }

decodeF32 = Decode.custom \bytes, @Crap {} ->
    { taken, rest } = takeNumber bytes
    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toF32
        |> Result.mapErr \_ -> TooShort
    { result, rest }

decodeF64 = Decode.custom \bytes, @Crap {} ->
    { taken, rest } = takeNumber bytes
    result =
        taken
        |> Str.fromUtf8
        |> Result.try Str.toF64
        |> Result.mapErr \_ -> TooShort
    { result, rest }

decodeBool = Decode.custom \bytes, @Crap {} ->
    when bytes is
        ['f', 'a', 'l', 's', 'e', 255, .. as rest] -> { result: Ok Bool.false, rest }
        ['0', 255, .. as rest] -> { result: Ok Bool.false, rest }
        ['t', 'r', 'u', 'e', 255, .. as rest] -> { result: Ok Bool.true, rest }
        ['1', 255, .. as rest] -> { result: Ok Bool.true, rest }
        rest -> { result: Err TooShort, rest }

decodeString : Decoder Str Crap
decodeString = Decode.custom \bytes, @Crap {} ->
    helper : List U8, List U8 -> (List U8, List U8)
    helper = \leftOver, acc ->
        when leftOver is
            [first, .. as newRest] ->
                if first == 255 then
                    (acc, newRest)
                else
                    helper newRest (List.append acc first)

            [] -> crash "should not happen? (1)"
    (strBytes, rest) = helper bytes []
    str =
        when Str.fromUtf8 strBytes is
            Ok s -> s
            Err _ -> crash "should not happen? (2)"
    { result: Ok str, rest }

decodeRecord : state, (state, Str -> [Keep (Decoder state Crap), Skip]), (state -> Result val DecodeError) -> Decoder val Crap
decodeRecord = \initialState, stepField, finalizer -> Decode.custom \bytes, @Crap {} ->
        decodeName : Decoder Str Crap
        decodeName = Decode.custom \name, @Crap {} ->
            when name is
                ['-', '-', .. as following] ->
                    Decode.decodeWith following decodeString (@Crap {})

                [_] | [_, _, ..] -> { result: Err TooShort, rest: name }
                [] -> crash "should not happen? (3)"

        decodeFields = \recordState, leftOver ->
            { result: nameResult, rest: bytesAfterName } =
                Decode.decodeWith leftOver decodeName (@Crap {})

            when nameResult is
                Err TooShort -> { result: Err TooShort, rest: bytes }
                Ok name ->
                    { val: updatedRecord, rest: bytesAfterValue } <-
                        (
                            when stepField recordState name is
                                Skip -> { result: Err TooShort, rest: bytes }
                                Keep valueDecoder ->
                                    Decode.decodeWith bytesAfterName valueDecoder (@Crap {})
                        )
                        |> tryDecode
                    when bytesAfterValue is
                        [] ->
                            when finalizer updatedRecord is
                                Ok val -> { result: Ok val, rest: [] }
                                Err e -> { result: Err e, rest: [] }

                        _ -> decodeFields updatedRecord bytesAfterValue

        decodeFields initialState bytes

decodeTuple : state, (state, U64 -> [Next (Decoder state Crap), TooLong]), (state -> Result val DecodeError) -> Decoder val Crap
decodeList : Decoder elem Crap -> Decoder (List elem) Crap

# Helpers

tryDecode : DecodeResult a, ({ val : a, rest : List U8 } -> DecodeResult b) -> DecodeResult b
tryDecode = \{ result, rest }, mapper ->
    when result is
        Ok val -> mapper { val, rest }
        Err e -> { result: Err e, rest }

takeNumber : List U8 -> { taken : List U8, rest : List U8 }
takeNumber = \bytes ->
    when List.walkUntil bytes Start numberHelp is
        Finish n | Zero n | Integer n | FractionB n | ExponentC n ->
            taken =
                bytes
                |> List.sublist { start: 0, len: n }
                |> List.dropIf \b -> b == '+'
                |> List.map \b -> if b == 'E' then 'e' else b
            { taken, rest: List.dropFirst bytes (n + 1) }

        _ -> { taken: [], rest: bytes }

numberHelp : NumberState, U8 -> [Continue NumberState, Break NumberState]
numberHelp = \state, byte ->
    when (state, byte) is
        (Start, b) if b == '0' -> Continue (Zero 1)
        (Start, b) if b == '-' -> Continue (Minus 1)
        (Start, b) if isDigit1to9 b -> Continue (Integer 1)
        (Minus n, b) if b == '0' -> Continue (Zero (n + 1))
        (Minus n, b) if isDigit1to9 b -> Continue (Integer (n + 1))
        (Zero n, b) if b == '.' -> Continue (FractionA (n + 1))
        (Zero n, b) if isValidEnd b -> Break (Finish n)
        (Integer n, b) if isDigit0to9 b && n <= maxBytes -> Continue (Integer (n + 1))
        (Integer n, b) if b == '.' && n < maxBytes -> Continue (FractionA (n + 1))
        (Integer n, b) if isValidEnd b && n <= maxBytes -> Break (Finish n)
        (FractionA n, b) if isDigit0to9 b && n <= maxBytes -> Continue (FractionB (n + 1))
        (FractionB n, b) if isDigit0to9 b && n <= maxBytes -> Continue (FractionB (n + 1))
        (FractionB n, b) if b == 'e' || b == 'E' && n <= maxBytes -> Continue (ExponentA (n + 1))
        (FractionB n, b) if isValidEnd b && n <= maxBytes -> Break (Finish n)
        (ExponentA n, b) if b == '-' || b == '+' && n <= maxBytes -> Continue (ExponentB (n + 1))
        (ExponentA n, b) if isDigit0to9 b && n <= maxBytes -> Continue (ExponentC (n + 1))
        (ExponentB n, b) if isDigit0to9 b && n <= maxBytes -> Continue (ExponentC (n + 1))
        (ExponentC n, b) if isDigit0to9 b && n <= maxBytes -> Continue (ExponentC (n + 1))
        (ExponentC n, b) if isValidEnd b && n <= maxBytes -> Break (Finish n)
        _ -> Break Invalid

NumberState : [
    Start,
    Minus U64,
    Zero U64,
    Integer U64,
    FractionA U64,
    FractionB U64,
    ExponentA U64,
    ExponentB U64,
    ExponentC U64,
    Invalid,
    Finish U64,
]

# TODO confirm if we would like to be able to decode
# "340282366920938463463374607431768211455" which is MAX U128 and 39 bytes
maxBytes : U64
maxBytes = 21 # Max bytes in a double precision float

isDigit0to9 : U8 -> Bool
isDigit0to9 = \b -> b >= '0' && b <= '9'

isDigit1to9 : U8 -> Bool
isDigit1to9 = \b -> b >= '1' && b <= '9'

isValidEnd : U8 -> Bool
isValidEnd = \b ->
    b == 255

# Tests

# U8
expect
    input = ["", "255"]
    got = parseArgs input
    got == Ok 255u8

# U16
expect
    input = ["", "65535"]
    got = parseArgs input
    got == Ok 65535u16

# U32
expect
    input = ["", "4000000000"]
    got = parseArgs input
    got == Ok 4000000000u32

# U64
expect
    input = ["", "18446744073709551614"]
    got = parseArgs input
    got == Ok 18446744073709551614u64

# U128
expect
    input = ["", "1234567"]
    got = parseArgs input
    got == Ok 1234567u128

# I8
expect
    input = ["", "-125"]
    got = parseArgs input
    got == Ok -125i8

# I16
expect
    input = ["", "-32768"]
    got = parseArgs input
    got == Ok -32768i16

# I32
expect
    input = ["", "-2147483648"]
    got = parseArgs input
    got == Ok -2147483648i32

# I64
expect
    input = ["", "-9223372036854775808"]
    got = parseArgs input
    got == Ok -9223372036854775808i64

# F32
expect
    input = ["", "12.34e-5"]
    got : Result F32 _
    got = parseArgs input
    gotStr = Result.map got Num.toStr
    gotStr == Ok "0.00012339999375399202"

# F64
expect
    input = ["", "12.34e-5"]
    got : Result F64 _
    got = parseArgs input
    gotStr = Result.map got Num.toStr
    gotStr == Ok "0.0001234"

# F64
expect
    input = ["", "12.0034"]
    got = parseArgs input
    got == Ok 12.0034dec

# Bool
expect
    input1 = ["", "false"]
    got1 = parseArgs input1
    check1 = got1 == Ok Bool.false

    input2 = ["", "true"]
    got2 = parseArgs input2
    check2 = got2 == Ok Bool.true

    check1 && check2

# string
expect
    input = ["", "Hello, World!"]
    got = parseArgs input
    got == Ok "Hello, World!"

# record with one numeric field
expect
    input = ["", "--hi", "42"]
    got = parseArgs input
    got == Ok { hi: 42 }

# record with mixed fields
expect
    input = ["", "--name", "Frodo", "--age", "50"]
    got = parseArgs input
    got == Ok { name: "Frodo", age: 50 }
