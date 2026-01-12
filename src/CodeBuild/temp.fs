// For more information see https://aka.ms/fsharp-console-apps

type Trivia =
    | Whitespace of string
    | Comment of string
    | Newline of string

type Token =
    | Name of string
    | Number of string
    | String of string
    | Op of string
    | Keyword of string
    | Indent
    | Dedent
    | NewlineToken

type TokenWithTrivia = {
    Leading : Trivia list
    Token : Token
    Trailing : Trivia list
}

type CSTNode =
    | Module of CSTNode list
    | Expr of CSTNode
    | Stmt of CSTNode
    | TokenNode of TokenWithTrivia

type Expr =
    | Atom of CSTNode
    | Binary of Expr * Token * Expr
    | Unary of Token * Expr

type FunctionDef = {
    DefKeyword : TokenWithTrivia
    Name : TokenWithTrivia
    LParen : TokenWithTrivia
    // Parameters : Parameter list
    RParen : TokenWithTrivia
    Colon : TokenWithTrivia
    Body : CSTNode list
}
let printTrivia (t: Trivia) =
    printfn "some trivia"

let printToken (t: Token) =
    printfn "token: %s" (t.ToString())


let rec printNode node =
    match node with
    | TokenNode t ->
        t.Leading |> List.iter printTrivia
        printToken t.Token
        t.Trailing |> List.iter printTrivia
    | Module nodes ->
        nodes |> List.iter printNode
    | n ->
        printfn "unknown node %s" (n.ToString())

module Tok =
    let t token =
        { Leading = []; Token = token; Trailing = [] }

    let kw s = t (Keyword s)
    let name s = t (Name s)
    let op s = t (Op s)

    let ws s =
        { Leading = [Whitespace s]; Token = Name ""; Trailing = [] } // for testing

    let Number n =
        t(Number n)

    let NewlineToken =
        { Leading = []; Token = NewlineToken; Trailing = [] }

let lex (input: string) : Token list =
    let isLetter c = System.Char.IsLetter c
    let isDigit c = System.Char.IsDigit c

    let rec loop i acc =
        if i >= input.Length then
            List.rev acc
        else
            match input.[i] with
            | ' ' | '\t' ->
                loop (i + 1) acc

            | '\n' ->
                loop (i + 1) (NewlineToken :: acc)

            | '=' ->
                loop (i + 1) (Op "=" :: acc)

            | c when isLetter c ->
                let j =
                    input
                    |> Seq.skip i
                    |> Seq.takeWhile isLetter
                    |> Seq.length
                let name = input.Substring(i, j)
                loop (i + j) (Name name :: acc)

            | c when isDigit c ->
                let j =
                    input
                    |> Seq.skip i
                    |> Seq.takeWhile isDigit
                    |> Seq.length
                let number = input.Substring(i, j)
                loop (i + j) (Number number :: acc)

            | c ->
                failwithf "Unexpected character: %c" c

    loop 0 []

let xAssign =
    Module [
        Stmt (
            Expr (
                TokenNode (Tok.name "x")
            )
        )
        TokenNode (Tok.op "=")
        TokenNode (Tok.Number "1")
        TokenNode Tok.NewlineToken
    ]

printNode xAssign

let parseAssignment tokens =
    match tokens with
    | Name name :: Op "=" :: Number value :: NewlineToken :: rest ->
        let node =
            Module [
                Stmt (
                    Expr (
                        TokenNode (Tok.name name)
                    )
                )
                TokenNode (Tok.op "=")
                TokenNode (Tok.Number value)
                TokenNode Tok.NewlineToken
            ]

        node, rest

    | _ ->
        failwith "Expected: NAME = NUMBER NEWLINE"

let parse input =
    let tokens = lex input
    let ast, remaining = parseAssignment tokens

    if not remaining.IsEmpty then
        failwith "Unconsumed tokens"
    ast

let t = lex "x = 1\n"


printNode (parse "x = 2")
