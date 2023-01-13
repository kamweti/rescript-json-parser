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