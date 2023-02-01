type pos = {
    line: int,
    column: int
}

type t = {
    start: pos,
    end: pos
}

type located<'a> = {
    token: 'a,
    location: t
}

type obj = {
    "start": {
        "line": int,
        "column": int
    },
    "end": {
        "line": int,
        "column": int
    }
}

let toObject = t =>
    {
        "start": {
            "line": t.start.line
            "column": t.start.column
        },
        "end": {
            "line": t.end.line
            "column": t.end.column
        }
    }