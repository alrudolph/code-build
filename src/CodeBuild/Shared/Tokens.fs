namespace CodeBuild.Shared

[<Struct>]
type TextSize = private TextSize of int
with
    member this.Value =
        let (TextSize v) = this
        v

module TextSize =
    let create v =
        if v < 0 then Error "Negative"
        else Ok (TextSize v)

    let value (TextSize v) = v

    let add (TextSize a) (TextSize b) =
        TextSize (a + b)


[<Struct>]
type TextRange(start : TextSize, length : TextSize) = 

    member _.Start = start

    member _.End = TextSize.add start length

    member _.Length = length

    member _.Extract (source : string) =
        source.Substring(start.Value, length.Value)

// TODO: size type as non-negative int
