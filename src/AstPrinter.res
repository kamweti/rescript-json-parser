type rec exp_value = 
    | ExpString({
        "type": string,
        "value": string,
        "raw" : string,
        "loc": Location.obj
    })
    | ExpBoolean({
        "type": string,
        "value": bool,
        "raw" : string,
        "loc": Location.obj
    })
    | ExpNull({
        "type": string,
        "value": Js.Nullable.t<string>,
        "raw" : string,
        "loc": Location.obj
    })
    | ExpNumber({
        "type": string,
        "value": float,
        "raw" : string,
        "loc": Location.obj
    })
    | ExpObj({
        "type": string,
        "members": list<exp_member>,
        "loc": Location.obj
    })

and exp_member = {
    "type": string,
    "key": exp_key,
    "value": exp_value
}
and exp_key = {
    "type" : string,
    "value": string,
    "loc": Location.obj
}

let rec printObject = (members, loc) =>
    ExpObj({
        "type": "Object",
        "members": printMembers(members),
        "loc" : Location.toObject(loc)
    })
and printMembers = members => members->List.map( member => {

    let (key, val) = member

    {
        "type": "Member",
        "key": printKey(key),
        "value": printValue(val)
    }
})

and printKey = ((str, loc)) => {
    {
        "type": "Identifier",
        "value": str,
        "loc": Location.toObject(loc)
    }
}
and printValue = (value) => {
    open Ast
    
    switch value {
    | Boolean(x, loc) =>
        ExpBoolean({
            "type": "Literal",
            "value": x,
            "raw" : x ? "true" : "false",
            "loc": Location.toObject(loc)
        })
    | Null(loc) => 
        ExpNull({
            "type": "Literal",
            "value": Js.Nullable.null,
            "raw" : "null",
            "loc": Location.toObject(loc)
        })
    | Number(x, loc) => 
        ExpNumber({
            "type": "Literal",
            "value": x,
            "raw" : x->Js.Float.toString,
            "loc": Location.toObject(loc)
        })
    | Object(members, loc) => printObject(members, loc)
    | Array(_, loc) =>
        ExpString({
            "type": "Literal",
            "value": "test",
            "raw": "test",
            "loc": Location.toObject(loc)
        })
    | String(x, loc) => 
        ExpString({
            "type": "Literal",
            "value": x,
            "raw": x,
            "loc": Location.toObject(loc)
        })
    }
}