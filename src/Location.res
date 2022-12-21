type pos = {
    line: int,
    col: int
}

type t = {
    start: pos,
    end: pos
}

type located<'a> = {
    val: 'a,
    location: t
}

let dummy = {
    let dummyPos = { line: 0, col: 0}

    {
        start: dummyPos,
        end: dummyPos
    }
}