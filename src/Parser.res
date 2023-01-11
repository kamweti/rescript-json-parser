type t = {
    tokens: array<Location.located>,
    mutable currentIndex: int
}

let make = tokens => {
    tokens: tokens,
    currentIndex: 0
}

let peek = ({tokens, currentIndex}) => Array.getUnsafe(tokens, currentIndex)

let isAtEnd = parser => peek(parser).token == Token.Eof

let advance = parser => {
    if (isAtEnd(parser)) {
        ()
    } else {
        parser.currentIndex = parser.currentIndex + 1
    }
}

let parse = parser => {

    let rec loop = accumulator => {
        switch peek(parser).token {
        | Token.String(val) => 
            let ast = parseString(val, parser)
            parser->advance
            loop(list{ast, ...accumulator})
        | Token.LeftCurlyBracket
        | Token.Colon
        | Token.RightCurlyBrace =>
            parser->advance
            loop(accumulator)
        | Token.Eof
        | _ => 
            accumulator
        }
    }
    and parseString = (val, parser) => {
        let {location} = peek(parser)

        Ast.makeString(location, val)
    }


    Ok(loop(list{}))
}