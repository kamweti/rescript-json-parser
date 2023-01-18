type t = {
    tokens: array<Location.located<Token.t>>,
    mutable currentIndex: int
}

open Token

exception ParseError(string, Location.located<Token.t>)

let make = tokens => {
    tokens: tokens,
    currentIndex: 0
}

let currentToken = ({tokens, currentIndex}) => Array.getUnsafe(tokens, currentIndex)

let isAtEnd = parser => currentToken(parser).token == Eof

let advance = parser => {
    if (isAtEnd(parser)) {
        ()
    } else {
        parser.currentIndex = parser.currentIndex + 1
    }
}

let rec parseValue = (parser) => {
    switch currentToken(parser).token {
    // | NumberLiteral => expression
    // | Boolean(val) => expression
    // | Null => expression
    // | ArrayOpen => expression
    | String(value) => Ast.makeStringLiteral(currentToken(parser).location, value)
    | ObjectOpen => Ast.makeObjectLiteral(currentToken(parser).location, advanceAfterObjectOpen(parser))
    | _ => raise(ParseError("Invalid value", currentToken(parser)))
    }
}
and advanceAfterObjectOpen = (parser) => {

    parser->advance

    let rec loop = (parser, members) => {
        switch currentToken(parser).token {
        | String(value) => 
            let key = Ast.makeKey(currentToken(parser).location, value)
            advanceAfterPropertyName(parser)
            let val = parseValue(parser)
            advanceAfterPropertyValue(parser)
            if (currentToken(parser).token == ObjectClose) {
                members
            } else {
                loop(parser, list{(key, val), ...members})
            }
        | _ => raise(ParseError("Expected property name or a `}`", currentToken(parser)))
        }
    }

    loop(parser, list{})
}
and advanceAfterPropertyName = (parser) => {

    parser->advance
    switch currentToken(parser).token {
    | Colon => parser->advance
    | _ => raise(ParseError("Expected property value", currentToken(parser)))
    }
}
and advanceAfterPropertyValue = (parser) => {

    parser->advance
    switch currentToken(parser).token {
    | Comma => advanceAfterComma(parser)
    | ObjectClose => ()
    | _ => raise(ParseError("Invalid data after property value in object", currentToken(parser)))
    }
}
and advanceAfterComma = (parser) => {

    parser->advance
    switch currentToken(parser).token {
    | String(_) => ()
    | _ => raise(ParseError("Invalid data after property value in object", currentToken(parser)))
    }
}

let advanceAfterParseCompleted = (parser) => {

    parser->advance
    switch currentToken(parser).token {
    | Eof => ()
    | _ => raise(ParseError("Invalid data after the last property", currentToken(parser)))
    }
}


let parse = parser => {

    switch currentToken(parser).token {
    | ObjectOpen => 
        try {
            let ast = parseValue(parser)
            advanceAfterParseCompleted(parser)

            Ok(ast)
        } catch {
        | ParseError(message, location) => Error(message, location)
        }
    | _ => 
        Error("Invalid token, expecting a JSON object string", currentToken(parser))
    }
}