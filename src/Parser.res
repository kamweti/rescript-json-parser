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

let rec parseLiteralValue = (parser) => {
    switch currentToken(parser).token {
    | NumberLiteral(val) => Ast.makeLiteralNumber(currentToken(parser).location, val)
    | Boolean(val) => Ast.makeLiteralBoolean(currentToken(parser).location, val)
    | Null => Ast.makeLiteralNull(currentToken(parser).location)
    // | ArrayOpen => expression
    | String(value) => Ast.makeLiteralString(currentToken(parser).location, value)
    | ObjectOpen => Ast.makeObject(currentToken(parser).location, advanceAfterObjectOpen(parser))
    | _ => raise(ParseError("Invalid value", currentToken(parser)))
    }
}
and advanceAfterObjectOpen = (parser) => {

    parser->advance

    let rec loop = (parser, members) => {
        switch currentToken(parser).token {
        | String(value) => 
            let key = Ast.makeIdentifier(currentToken(parser).location, value)
            advanceAfterIdentifier(parser)
            let val = parseLiteralValue(parser)
            advanceAfterLiteralValue(parser)
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
and advanceAfterIdentifier = (parser) => {

    parser->advance
    switch currentToken(parser).token {
    | Colon => parser->advance
    | _ => raise(ParseError("Expected property value", currentToken(parser)))
    }
}
and advanceAfterLiteralValue = (parser) => {

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
            let ast = parseLiteralValue(parser)
            advanceAfterParseCompleted(parser)

            Ok(ast)
        } catch {
        | ParseError(message, location) => Error(message, location)
        }
    | _ => 
        Error("Invalid token, expecting a JSON object string", currentToken(parser))
    }
}