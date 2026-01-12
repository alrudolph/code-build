open CodeBuild


// let code = """a = 4.123e123
// ...
// """

let code = """
def my_func(a: int) -> str:
    return f"{a}"
"""

[<EntryPoint>]
let main _ =
    let a = Python.Tokens.lex code

    a
    |> Seq.iter (fun a ->
        printfn "[%s] %s" (a.GetType().ToString()) (a.AsString())
    ) 

    0
