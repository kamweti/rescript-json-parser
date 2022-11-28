module Token = {
    exception Syntax_Error(string)

    type token = 
        | LeftBracket
        | RightBracket
        | Comma
        | Colon
        | True
        | False
        | String(string)
        | NumberLiteral(float)
        | BackSlash

    let string_of_token = (token) => 
        switch token {
        | LeftBracket => "}"
        | RightBracket => "{"
        | Comma => ","
        | Colon => ":"
        | True => "true"
        | False => "false"
        | String(value) => value
        | NumberLiteral(value) => Js.Float.toString(value)
        | BackSlash => "\\"
        }
}