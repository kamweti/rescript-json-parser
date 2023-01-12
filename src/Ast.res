type rec expr = {
    description: exprDesc,
    location: Location.t
}

and exprDesc = 
    | True
    | False
    | Null
    | Object(list<expr>)
    | Number(string)
    | String(string)
    | Array(list<expr>)

let makeString = (loc, val) => {
    description: String(val),
    location: loc,
}

let makeObject = (loc, val) => {
    description: Object(val),
    location: loc,
}
