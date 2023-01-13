exception Syntax_Error(string)

type t = 
    | ArrayOpen
    | ArrayClose
    | ObjectOpen
    | ObjectClose
    | Comma
    | Colon
    | Boolean(bool)
    | NullValue
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
    | NullValue => "null"
    | String(value) => value
    | NumberLiteral(value) => value
    | Eof => "eof"
    }