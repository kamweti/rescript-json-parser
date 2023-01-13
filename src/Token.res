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
    | NumberLiteral(string)
    | Eof

let toString = (token) => 
    switch token {
    | ArrayOpen => "["
    | ArrayClose => "]"
    | ObjectOpen => "{"
    | ObjectClose => "}"
    | Comma => ","
    | Colon => ":"
    | Boolean(value) => value ? "true" : "false"
    | Null => "null"
    | String(value) => value
    | NumberLiteral(value) => value
    | Eof => "eof"
    }