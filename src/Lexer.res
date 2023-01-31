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

let errorToString = e => {
    switch e {
    | UknownCharacter => "Uknown character"
    | UnterminatedString => "Unterminated string"
    | InvalidCharInString => "Invalid character in string"
    | InvalidNumber => "Invalid number"
    | InvalidLiteralName => "Invalid literal name"
    }
}
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

let previousChar = (lexer) => {
    switch String.get(lexer.source, lexer.currentIndex - 1) {
    | c => Some(c)
    | exception _ => None
    }
}

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

let scanLiteralNumber = lexer => {

    exception ScanLiteralNumberException
    
    let rec loopOrRaise = (lexer) => switch lexer->previousChar {
    | Some('0') => loopZero(lexer)
    | Some('1' .. '9') => loopDigits(lexer)
    | _ => ()
    }
    and loopZero = (lexer) => switch lexer->currentChar {
        | Some('.') => 
            lexer->consumeCurrentAndDiscard
            loopFractional(lexer)
        | Some('E') | Some('e') => 
            lexer->consumeCurrentAndDiscard
            loopExponent(lexer)
        | _ => raise(ScanLiteralNumberException)
    }
    and loopFractional = (lexer) => switch lexer->currentChar {
        | Some('0' .. '9') => 
            lexer->consumeCurrentAndDiscard
            loopFractional(lexer)
        | Some('E') | Some('e') => 
            lexer->consumeCurrentAndDiscard
            loopExponent(lexer)
        | _ => ()
    }
    and loopDigits = (lexer) => switch lexer->currentChar {
        | Some('0' .. '9') => 
            lexer->consumeCurrentAndDiscard
            loopDigits(lexer)
        | Some('.') => 
            lexer->consumeCurrentAndDiscard
            loopFractional(lexer)
        | Some('E') | Some('e') => 
            lexer->consumeCurrentAndDiscard
            loopExponent(lexer)
        | _ => ()
    }
    and loopExponent = (lexer) => switch lexer->currentChar {
        | Some('0' .. '9')
        | Some('+')
        | Some('-') =>
            lexer->consumeCurrentAndDiscard
            loopExponent(lexer)
        | _ => ()
    }

    try {
        loopOrRaise(lexer)

        let maybeRawValue = Js.String.substring(
            ~from=lexer.startIndex, 
            ~to_=lexer.currentIndex,
            lexer.source
        )->Float.fromString

        switch maybeRawValue {
        | Some(value) => emitLocatedToken(Token.NumberLiteral(value), lexer)
        | _ => ScanError({ desc: InvalidNumber, loc: emitCurrentLocation(lexer)})
        }
    } catch {
    | ScanLiteralNumberException => ScanError({ desc: InvalidNumber, loc: emitCurrentLocation(lexer)})
    }
}

let rec scanLiteralBoolOrNull = lexer => {
    switch lexer->currentChar { 
    | Some('a' .. 'z') =>
        lexer->consumeCurrentAndDiscard
        scanLiteralBoolOrNull(lexer)
    | None | Some(_) =>
        let value = Js.String.substring(
            ~from=lexer.startIndex, 
            ~to_=lexer.currentIndex,
            lexer.source
        );

        switch value {
        | "true" => emitLocatedToken(Token.Boolean(true), lexer)
        | "false" => emitLocatedToken(Token.Boolean(false), lexer)
        | "null" => emitLocatedToken(Token.Null, lexer)
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
                    lexer->scanLiteralNumber
                | _ => ScanError({ desc: UknownCharacter, loc: emitCurrentLocation(lexer)})
                }
            | ch if isDigit(ch) => lexer->scanLiteralNumber
            | 'a' .. 'z' => lexer->scanLiteralBoolOrNull
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
        | ScanError(error) => Error(error)
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