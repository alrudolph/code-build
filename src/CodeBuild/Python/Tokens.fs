namespace CodeBuild.Python

open CodeBuild.Shared

// TODO: instead of 
//  | Number of string
// do
//   type TextRange = struct
//     val Start: int
//     val Length: int
//   end

//   type Token =
//     | Number of TextRange
//     | Name of TextRange

// TODO: instead of
//    Tok list
// do
//    ResizeArray<Tok>   // or array

// TODO: structs avoid heap allocation
// [<Struct>]
// type SyntaxToken =
//     val Kind: TokenKind
//     val Range: TextRange
//     val LeadingTriviaCount: int
//     val TrailingTriviaCount: int

// TODO: ADD LEX STATE
type LexState =
    | Default
    // TODO: HANDLE r, f, t etc...
    | InString of quote: char * isTriple: bool
    | InComment

// let mutable state = Default
// while i < input.Length do
//     match state, input.[i] with
//     | Default, '#' ->
//         state <- InComment
//     | InComment, '\n' ->
//         state <- Default
//     | Default, '"' ->
//         state <- InString('"', false)
//     | InString(q, _), c when c = q ->
//         state <- Default
//     | _ ->
//         ()
//     i <- i + 1

module Tokens =

    [<Struct>]
    type StringModifier(range : TextRange) =

        member _.Range = range

        member _.Extract (source : string) = 
            range.Extract source


    type Token =
        | Whitespace of TextRange
        | Newline of TextRange
        | Number of TextRange
        | Identifier of TextRange
        | Operator of TextRange
        | Comment of TextRange
        | String of StringModifier
    with 
        member this.Range =
            match this with
            | Whitespace s
            | Newline s
            | Number s
            | Identifier s
            | Operator s
            | Comment s -> s
            | String s -> s.Range

        member this.AsString(source : string) : string =
            match this with
            | Whitespace s -> s.Extract source
                            |> Seq.map (fun x -> match x with
                                                    | ' ' -> "[space]"
                                                    | '\t' -> "[tab]"
                                                    | s -> string s)
                            |> Seq.fold (fun acc x -> acc + x) ""
            | Newline s -> String.replicate s.Length.Value "\\n"
            | Number s -> s.Extract source
            | Identifier s -> sprintf "\"%s\"" (s.Extract source)
            | Operator s -> s.Extract source
            | Comment s -> s.Extract source
            | String s -> s.Extract source
                
    let private makeToken ctor (start, length) =
        match TextSize.create start, TextSize.create length with
        | Ok s, Ok l ->
            Ok (TextRange(s, l)) |> Result.map ctor
        | Error _, Error _ ->
            Error "Start and length must be non-negative"
        | Error _, _ ->
            Error "Start must be non-negative"
        | _, Error _ ->
            Error "Length must be non-negative"

    let whitespace = makeToken Whitespace
    let newline    = makeToken Newline
    let number     = makeToken Number
    let identifier = makeToken Identifier
    let operator   = makeToken Operator
    let comment = makeToken Comment

    let parseIdent (input : string, i : int ) = 
        let j =
            input
            |> Seq.skip i
            |> Seq.takeWhile (fun x -> System.Char.IsLetter x || x = '_' || System.Char.IsDigit x)
            |> Seq.length

        identifier (i, j)

    let lex (input: string) : Result<Token list, string> =
        let rec loop i acc : Result<Token list, string> =
            if i >= input.Length then
                Ok (List.rev acc)
            else
                match input.[i] with
                | '#' ->
                    let j =
                        input
                        |> Seq.skip i
                        |> Seq.takeWhile (fun x -> x <> '\n')
                        |> Seq.length

                    comment (i, j)
                        |> Result.bind (fun n -> loop (i + j) (n :: acc))
             
                | '"'
                | '\''
                // | 'f' 
                // | 'F'
                // | 'r'
                // | 'R'
                // | 'b'
                // | 'B'
                // | 'u'
                // | 'U'
                // | 't'
                // | 'T' 
                    ->
                    let tripled = String.replicate 3 (string(input.[i])) 
                    
                    let isTriple = 
                        input.Substring(i, 3) = tripled

                    if isTriple then
                        let j =
                            (
                                [ i + 5 .. input.Length - 1 ]
                                |> Seq.takeWhile (fun n -> input.Substring(n - 2, 3) <> tripled && input.[n - 3] <> '\\')
                                |> Seq.length
                            ) + 6

                        match TextSize.create i, TextSize.create j with
                        | Ok s, Ok l ->
                            Ok (String (StringModifier (TextRange(s, l))))
                        | Error _, Error _ ->
                            Error "Start and length must be non-negative"
                        | Error _, _ ->
                            Error "Start must be non-negative"
                        | _, Error _ ->
                            Error "Length must be non-negative"
                        |> Result.bind (fun n -> loop (i + j) (n :: acc))
                    else 
                        // TODO: or newline?
                        let j =
                            (
                                [ i + 1 .. input.Length - 1 ]
                                |> Seq.takeWhile (fun n -> input.[n] <> input.[i] && input.[n - 1] <> '\\')
                                |> Seq.length
                            ) + 2

                        match TextSize.create i, TextSize.create j with
                        | Ok s, Ok l ->
                            Ok (String (StringModifier (TextRange(s, l))))
                        | Error _, Error _ ->
                            Error "Start and length must be non-negative"
                        | Error _, _ ->
                            Error "Start must be non-negative"
                        | _, Error _ ->
                            Error "Length must be non-negative"
                        |> Result.bind (fun n -> loop (i + j) (n :: acc))
                    
                | s when s = ' ' || s = '\t' || s = '\n' ->
                    let j =
                        input
                        |> Seq.skip i
                        |> Seq.takeWhile (fun x -> x = s)
                        |> Seq.length

                    if s = '\n' then
                        newline (i, j)
                        |> Result.bind (fun n -> loop (i + j) (n :: acc))
                    else 
                        whitespace (i, j)
                        |> Result.bind (fun n -> loop (i + j) (n :: acc))

                | c when System.Char.IsLetter c || c = '_' ->
                    let iden = parseIdent (input, i)

                    iden
                    |> Result.bind (fun n -> loop n.Range.End.Value (n :: acc))
                    
                | c when System.Char.IsDigit c || c = '.' || c = '-' || c = '+' ->
                    let nextThree = (sprintf "%s  " input).Substring(i, 3)

                    match [| string(nextThree.[0]) ; string(nextThree.[1]) ; string(nextThree.[2]) |] with
                    | [| "." ; "." ; "." |] ->
                        operator (i, 3)
                        |> Result.bind (fun n -> loop (i + 3) (n :: acc))
                    | [| "+" ; "=" ; _ |]
                    | [| "-" ; "=" ; _ |]
                    | [| "-" ; ">" ; _ |] ->
                        operator (i, 2)
                        |> Result.bind (fun n -> loop (i + 2) (n :: acc))
                    // TODO: WHAT IF ITS JUST A - OR + OR . AND NOT A NUMBER 
                    | _ -> 
                        let mutable j = i
                        let mutable seenDot = false
                        let mutable seenExp = false
                        let mutable stop = false

                        while j < input.Length && not stop do
                            let ch = input.[j]
                            if System.Char.IsDigit ch then
                                j <- j + 1
                            elif ch = '.' && not seenDot && not seenExp then
                                seenDot <- true
                                j <- j + 1
                            elif (ch = 'e' || ch = 'E') && not seenExp then
                                seenExp <- true
                                j <- j + 1
                                // optionally consume '+' or '-' immediately after 'e'/'E'
                                if j < input.Length && (input.[j] = '+' || input.[j] = '-') then
                                    j <- j + 1
                            else
                                stop <- true

                        if i <= j - 1 then
                            operator (i, j - i)
                            |> Result.bind (fun n -> loop (i + j - i) (n :: acc))
                        else
                            number (i, j - i)
                            |> Result.bind (fun n -> loop (i + j - i) (n :: acc))

                | _ ->
                    let nextThree = (sprintf "%s  " input).Substring(i, 3)

                    match [| string(nextThree.[0]) ; string(nextThree.[1]) ; string(nextThree.[2]) |] with
                    // TODO: plugin for custom tokens (sql comes to mind)
                    | [| "'" ; "'" ; "'" |]
                    | [| "\"" ; "\"" ; "\"" |]
                    | [| "." ; "." ; "." |]
                    | [| "/" ; "/" ; "=" |]
                    | [| ">" ; ">" ; "=" |]
                    | [| "<" ; "<" ; "=" |]
                    | [| "*" ; "*" ; "=" |] ->
                        operator (i, 3)
                        |> Result.bind (fun n -> loop (i + 3) (n :: acc))
                    | [| "=" ; "=" ; _ |]
                    | [| "<" ; "=" ; _ |]
                    | [| "=" ; ">" ; _ |]
                    | [| "-" ; ">" ; _ |]
                    | [| "-" ; "=" ; _ |]
                    | [| "+" ; "=" ; _ |]
                    | [| "*" ; "=" ; _ |]
                    | [| "&" ; "=" ; _ |]
                    | [| "|" ; "=" ; _ |]
                    | [| "^" ; "=" ; _ |]
                    | [| "/" ; "=" ; _ |]
                    | [| "%" ; "=" ; _ |]
                    | [| "/" ; "/" ; _ |]
                    | [| "*" ; "*" ; _ |]
                    | [| "<" ; "<" ; _ |]
                    | [| ">" ; ">" ; _ |] ->
                        operator (i, 2)
                        |> Result.bind (fun n -> loop (i + 2) (n :: acc))
                    | _ ->
                        operator (i, 1)
                        |> Result.bind (fun n -> loop (i + 1) (n :: acc))

        loop 0 []
