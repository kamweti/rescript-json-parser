type pos = {
    line: int,
    column: int
}

type t = {
    start: pos,
    end: pos
}

type located = {
    token: Token.t,
    location: t
}