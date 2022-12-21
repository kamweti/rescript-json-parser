exception Syntax_Error(string)

type t = 
    | LeftSquareBracket
    | RightSquareBracket
    | LeftCurlyBracket
    | RightCurlyBrace
    | Comma
    | Colon
    | True
    | False
    | Null
    | String(string)
    | NumberLiteral(float)
    | Eof

let toString = (token) => 
    switch token {
    | LeftSquareBracket => "["
    | RightSquareBracket => "]"
    | LeftCurlyBracket => "{"
    | RightCurlyBrace => "}"
    | Comma => ","
    | Colon => ":"
    | True => "true"
    | False => "false"
    | Null => "null"
    | String(value) => value
    | NumberLiteral(value) => Js.Float.toString(value)
    | Eof => "eof"
    }
