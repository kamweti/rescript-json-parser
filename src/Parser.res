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
        | Token.String(val) => loop(list{parseString(parser, val), ...accumulator})
        | Token.LeftCurlyBracket => loop(list{parseObject(parser), ...accumulator})
        | Token.Colon => 
            parser->advance
            loop(accumulator)
        | Token.RightCurlyBrace
        | Token.Eof
        | _ => 
            accumulator->List.reverse
        }
    }
    and parseString = (parser, val) => {
        let {location} = peek(parser)

        parser->advance
        Ast.makeString(location, val)
    }
    and parseObject = (parser) => {

        let {location} = peek(parser)

        parser->advance
        let members = loop(list{})
        Ast.makeObject(location, members)
    }


    Ok(loop(list{}))
}