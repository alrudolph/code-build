namespace CodeBuild.Shared

type LexState =
    | Default
    | InString
    | InComment
