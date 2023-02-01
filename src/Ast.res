type rec member = (key, value)

and key = (string, Location.t) 

and value = 
    | Object(list<member>, Location.t)
    | Array(list<value>, Location.t)
    | Number(float, Location.t)
    | String(string, Location.t)
    | Boolean(bool, Location.t)
    | Null(Location.t)


let makeIdentifier = (loc, binding) => (binding, loc)

let makeLiteralString = (loc, binding) => String(binding, loc)

let makeObject = (loc, binding) => Object(binding, loc)

let makeLiteralBoolean = (loc, binding) => Boolean(binding, loc)

let makeLiteralNull = (loc) => Null(loc)

let makeLiteralNumber = (loc, binding) => Number(binding, loc)