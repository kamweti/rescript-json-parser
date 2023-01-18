type t = {
    source: string,
    mutable startIndex: int,
    mutable startLine: int,
    mutable startColumn: int,
    mutable currentIndex: int,
    mutable currentColumn: int,
    mutable currentLine: int,
}

type rec lexerError = {
    loc: Location.t,
    desc: lexerErrorDesc
}
and lexerErrorDesc =
    | UknownCharacter
    | UnterminatedString
    | InvalidCharInString
    | InvalidNumber
    | InvalidLiteralName

type singleScanResult =
    | LocatedToken(Location.located<Token.t>)
    | SkippedCharacter
    | ScanError(lexerError)
    | ReachedTheEnd

let makeLexer = (str) => {
    {
        source: str,
        startIndex: 0,
        currentIndex: 0,
        startLine: 1,
        startColumn: 1,
        currentColumn: 1,
        currentLine: 1
    }
}

let isDigit = character => {
    switch character {
    | '0' .. '9' => true
    | _ => false
    }
}

let emitCurrentLocation = (lexer) => {
    let currentloc: Location.t = {
        start: {
            line: lexer.startLine,
            column: lexer.startColumn
        },
        end: {
            line: lexer.currentLine,
            column: lexer.currentColumn
        }
    }

    currentloc
}

let emitLocatedToken = (token, lexer) => LocatedToken({
    token: token,
    location: emitCurrentLocation(lexer)
})

let currentChar = (lexer) => {
    switch String.get(lexer.source, lexer.currentIndex) {
    | c => Some(c)
    | exception _ => None
    }
}

let nextChar = lexer =>
  switch String.get(lexer.source, lexer.currentIndex + 1) {
  | c => Some(c)
  | exception _ => None
  }

let consumeCurrentChar = lexer =>
  switch currentChar(lexer) {
  | Some(_) as maybeCurrentChar =>
    lexer.currentIndex = lexer.currentIndex + 1
    lexer.currentColumn = lexer.currentColumn + 1
    maybeCurrentChar
  | None => None
  }

let consumeCurrentAndDiscard = lexer => {
  let _: option<char> = consumeCurrentChar(lexer)
}


let consumeCurrentChar2 = lexer => {
    lexer.currentIndex = lexer.currentIndex + 2
    lexer.currentColumn = lexer.currentColumn + 2
    ()
}

let consumeCurrentCharLineOnly = lexer => {
    lexer.currentLine = lexer.currentLine + 1
    lexer.currentColumn = 1
}

let rec scanString = lexer => {

    switch lexer->currentChar {
    | None => ScanError({ desc: UnterminatedString, loc: emitCurrentLocation(lexer)})
    | Some('"') => 
        let value = Js.String.substring(
            ~from=lexer.startIndex + 1, 
            ~to_=lexer.currentIndex,
            lexer.source
        );

        lexer->consumeCurrentAndDiscard
        emitLocatedToken(Token.String(value), lexer)
    | Some('\\') =>
        switch lexer->nextChar {
        | Some('"')
        | Some('\\')
        | Some('/')
        | Some('b')
        | Some('f')
        | Some('n')
        | Some('r')
        | Some('t')
        | Some('u') =>
            lexer->consumeCurrentAndDiscard
            lexer->scanString
        | None | Some(_) => ScanError({ desc: InvalidCharInString, loc: emitCurrentLocation(lexer)})
        }
    | Some('\n') => 
        lexer->consumeCurrentCharLineOnly
        lexer->scanString
    | Some(_) => 
        lexer->consumeCurrentAndDiscard
        lexer->scanString
    }
}

let rec scanNumber = lexer => {

    switch lexer->currentChar { 
    | Some('.') 
    | Some('E')
    | Some('e')
    | Some('-')
    | Some('0' .. '9') => 
        lexer->consumeCurrentAndDiscard
        scanNumber(lexer)
    | None | Some(_) =>
        let value = Js.String.substring(
            ~from=lexer.startIndex, 
            ~to_=lexer.currentIndex,
            lexer.source
        );

        emitLocatedToken(Token.NumberLiteral(value), lexer)
    }
}

let rec scanLiteralName = lexer => {
    switch lexer->currentChar { 
    | Some('a' .. 'z') =>
        lexer->consumeCurrentAndDiscard
        scanLiteralName(lexer)
    | None | Some(_) =>
        let value = Js.String.substring(
            ~from=lexer.startIndex, 
            ~to_=lexer.currentIndex,
            lexer.source
        );

        switch value {
        | "true" => emitLocatedToken(Token.Boolean(true), lexer)
        | "false" => emitLocatedToken(Token.Boolean(false), lexer)
        | "null" => emitLocatedToken(Token.NullValue, lexer)
        | _ => ScanError({ desc: InvalidLiteralName, loc: emitCurrentLocation(lexer)})
        }
    }
}

let scanToken = (lexer) => {
    lexer
        ->consumeCurrentChar
        ->Option.mapWithDefault(ReachedTheEnd, character =>
            switch character {
            | '{' => emitLocatedToken(Token.ObjectOpen, lexer)
            | '}' => emitLocatedToken(Token.ObjectClose, lexer)
            | ',' => emitLocatedToken(Token.Comma, lexer)
            | ':' => emitLocatedToken(Token.Colon, lexer)
            | '[' => emitLocatedToken(Token.ArrayOpen, lexer)
            | ']' => emitLocatedToken(Token.ArrayClose, lexer)
            | ' ' | '\t' | '\r'=> SkippedCharacter
            | '\n' => 
                lexer->consumeCurrentCharLineOnly 
                SkippedCharacter
            | '"' => lexer->scanString
            | '-' =>
                switch lexer->currentChar {
                | Some(x) if isDigit(x) =>
                    lexer->scanNumber
                | _ => ScanError({ desc: UknownCharacter, loc: emitCurrentLocation(lexer)})
                }
            | ch if isDigit(ch) => lexer->scanNumber
            | 'a' .. 'z' => lexer->scanLiteralName
            | _ => ScanError({ desc: UknownCharacter, loc: emitCurrentLocation(lexer)})
            }
        )
}

let scan = source => {
    let lexer = makeLexer(source)

    let rec loop = (accumulatedTokens) => {
        lexer.startIndex = lexer.currentIndex
        lexer.startLine = lexer.currentLine
        lexer.startColumn = lexer.currentColumn

        switch scanToken(lexer) {
        | SkippedCharacter => loop(accumulatedTokens)
        | LocatedToken(token) => loop(list{token, ...accumulatedTokens})
        | ScanError(error) => Error(error, accumulatedTokens)
        | ReachedTheEnd => 
            let eof: Location.located<Token.t> = {
                token: Token.Eof,
                location: emitCurrentLocation(lexer)
            }
            
            let tokens = list{eof, ...accumulatedTokens}->List.reverse

            Ok(tokens)
        }
    }

    loop(list{})
}