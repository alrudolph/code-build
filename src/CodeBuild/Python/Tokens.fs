namespace CodeBuild.Python

module Tokens =

    type TriviaToken =
        | Whitespace of string
        | Newline of string
    with
        member this.AsString() : string =
            match this with
                | Whitespace s -> s
                                |> Seq.map (fun x -> match x with
                                                        | ' ' -> "[space]"
                                                        | '\t' -> "[tab]"
                                                        | s -> string s)
                                |> Seq.fold (fun acc x -> acc + x) ""
                | Newline s -> String.replicate s.Length "\\n"

    type LanguageToken = 
        | Number of string
        | Text of string
        | Operator of string
    with
        member this.AsString() : string =
            match this with
                | Number s -> s
                | Text s -> sprintf "\"%s\"" s
                | Operator s -> s

    type Token =
        | TriviaTok of TriviaToken
        | TokenTok of LanguageToken
    with 
        member this.AsString() =
            match this with
            | TriviaTok s -> s.AsString()
            | TokenTok s -> s.AsString()

    let lex (input: string) : Token list =
        let rec loop i acc : Token list =
            if i >= input.Length then
                List.rev acc
            else
                match input.[i] with
                    | s when s = ' ' || s = '\t' || s = '\n' ->
                        let j =
                            input
                            |> Seq.skip i
                            |> Seq.takeWhile (fun x -> x = s)
                            |> Seq.length

                        let name = input.Substring(i, j)

                        if s = '\n' then
                            loop (i + j) (TriviaTok (Newline name) :: acc)
                        else 
                            loop (i + j) (TriviaTok (Whitespace name) :: acc)

                    | c when System.Char.IsLetter c || c = '_' ->
                        let j =
                            input
                            |> Seq.skip i
                            |> Seq.takeWhile (fun x -> System.Char.IsLetter x || x = '_' || System.Char.IsDigit x)
                            |> Seq.length

                        let name = input.Substring(i, j)

                        loop (i + j) (TokenTok (Text name) :: acc)
                
                    | c when System.Char.IsDigit c || c = '.' || c = '-' || c = '+' ->
                        let nextThree = (sprintf "%s  " input).Substring(i, 3)

                        match [| string(nextThree.[0]) ; string(nextThree.[1]) ; string(nextThree.[2]) |] with
                            | [| "." ; "." ; "." |] ->
                                loop (i + 3) (TokenTok (Operator "...") :: acc)
                            | [| "+" ; "=" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "+=") :: acc)
                            | [| "-" ; "=" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "-=") :: acc)
                            | [| "-" ; ">" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "->") :: acc)
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

                                let v = input.Substring(i, j - i)

                                if i <= j - 1 then
                                    loop j (TokenTok (Operator v):: acc)
                                else
                                    loop j (TokenTok (Number v) :: acc)

                    | _ ->
                        let nextThree = (sprintf "%s  " input).Substring(i, 3)

                        match [| string(nextThree.[0]) ; string(nextThree.[1]) ; string(nextThree.[2]) |] with
                            // TODO: plugin for custom tokens (sql comes to mind)
                            | [| "'" ; "'" ; "'" |] ->
                                loop (i + 3) (TokenTok (Operator "'''") :: acc)
                            | [| "\"" ; "\"" ; "\"" |] ->
                                loop (i + 3) (TokenTok (Operator "\"\"\"") :: acc)
                            | [| "." ; "." ; "." |] ->
                                loop (i + 3) (TokenTok (Operator "...") :: acc)
                            | [| "/" ; "/" ; "=" |] ->
                                loop (i + 3) (TokenTok (Operator "//=") :: acc)
                            | [| ">" ; ">" ; "=" |] ->
                                loop (i + 3) (TokenTok (Operator ">>=") :: acc)
                            | [| "<" ; "<" ; "=" |] ->
                                loop (i + 3) (TokenTok (Operator "<<=") :: acc)
                            | [| "*" ; "*" ; "=" |] ->
                                loop (i + 3) (TokenTok (Operator "**=") :: acc)
                            | [| "=" ; "=" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "==") :: acc)
                            | [| "<" ; "=" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "<=") :: acc)
                            | [| "=" ; ">" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "=>") :: acc)
                            | [| "-" ; ">" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "->") :: acc)
                            | [| "-" ; "=" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "-=") :: acc)
                            | [| "+" ; "=" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "+=") :: acc)
                            | [| "*" ; "=" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "*=") :: acc)
                            | [| "&" ; "=" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "&=") :: acc)
                            | [| "|" ; "=" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "|=") :: acc)
                            | [| "^" ; "=" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "=") :: acc)
                            | [| "/" ; "=" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "/=") :: acc)
                            | [| "%" ; "=" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "%=") :: acc)
                            | [| "/" ; "/" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "//") :: acc)
                            | [| "*" ; "*" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "**") :: acc)
                            | [| "<" ; "<" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator "<<") :: acc)
                            | [| ">" ; ">" ; _ |] ->
                                loop (i + 2) (TokenTok (Operator ">>") :: acc)
                            | x ->
                                loop (i + 1) (TokenTok (Operator x[0]) :: acc)

        loop 0 []
