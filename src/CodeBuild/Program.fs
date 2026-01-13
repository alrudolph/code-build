open CodeBuild


// let code = """a = 4.123e123
// ...
// """

let code = """
def my_func(a: int) -> str:  # hello there [some](markdown)
    '''My docstring

    other paren nested 
    '''
    name = 'hello "alex"'
    
    return f"{a}"

# end ()
"""

[<EntryPoint>]
let main _ =
    let a = Python.Tokens.lex code

    match a with
        | Ok(v) ->
            v
            |> Seq.iter (fun a ->
                printfn "[%s] %s" (a.GetType().ToString()) (a.AsString code)
            ) 
        | _ ->
            printfn "could not run"

    0
