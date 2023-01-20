exception Syntax_Error(string)

type t = 
    | ArrayOpen
    | ArrayClose
    | ObjectOpen
    | ObjectClose
    | Comma
    | Colon
    | Boolean(bool)
    | Null
    | String(string)
    | NumberLiteral(float)
    | Eof