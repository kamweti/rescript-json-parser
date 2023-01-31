type rec root = (list<member>, Location.t)

and member = (key, value)

and key = Identifier(string, Location.t) 

and value = (binding, Location.t)

and binding = 
    | Object(list<member>)
    | Array(list<value>)
    | Number(float)
    | String(string)
    | Boolean(bool)
    | Null

let makeIdentifier = (loc, binding) => Identifier(binding, loc)

let makeLiteralString = (loc, binding) => (String(binding), loc)

let makeObject = (loc, binding) => (Object(binding), loc)

let makeLiteralBoolean = (loc, binding) => (Boolean(binding), loc)

let makeLiteralNull = (loc) => (Null, loc)

let makeLiteralNumber = (loc, binding) => (Number(binding), loc)

let makeRoot = (loc, binding) => (binding, loc)