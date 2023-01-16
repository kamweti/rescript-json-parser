type rec json = list<member>

and member = (key, value)

and key = (string, Location.t) 

and value = (v, Location.t)

and v = 
    | Object(json)
    | Array(list<v>)
    | NumberLiteral(string)
    | String(string)
    | Boolean(bool)
    | Null


let makeKey = (loc, value) => (value, loc)

let makeStringLiteral = (loc, value) => (String(value), loc)

let makeObjectLiteral = (loc, value) => (Object(value), loc)