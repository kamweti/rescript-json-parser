// open Ast

let rec printObject = (members, _) =>
    {
        "type": "Object",
        "children": printMember(members)
    }
and printMember = members => {
    members->List.map((_, _) => 
        {
            "kind": "Member"
        }
    )
}