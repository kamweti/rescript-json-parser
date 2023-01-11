type t = {
    source: string,
    mutable startIndex: int,
    mutable startLine: int,
    mutable startColumn: int,
    mutable currentIndex: int,
    mutable currentColumn: int,
    mutable currentLine: int,
}

type scanErrorType =
    | UknownCharacter
    | UnterminatedString
    | InvalidNumber
    | InvalidKeyword

type singleScanResult =
    | LocatedToken(Location.located)
    | SkippedCharacter
    | ScanError(scanErrorType)
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

let  emitLocatedToken = (token, lexer) => LocatedToken({
    token: token,
    location: {
        start: {
            line: lexer.startLine,
            column: lexer.startColumn
        },
        end: {
            line: lexer.currentLine,
            column: lexer.currentColumn
        }
    }
})

let getCharacterAtIndex = (lexer, index) => {
    switch String.get(lexer.source, index) {
    | char =>
        Some(char)
    | exception _ => None
    }
}

let getCurrentCharacter = lexer => {
    getCharacterAtIndex(lexer, lexer.currentIndex)
}

let getNextCharacter = lexer => {
    getCharacterAtIndex(lexer, lexer.currentIndex + 1)
}

let getNextCharacter2 = lexer => {
    getCharacterAtIndex(lexer, lexer.currentIndex + 2)
}

let advance = lexer => {
    lexer.currentIndex = lexer.currentIndex + 1
    lexer.currentColumn = lexer.currentColumn + 1
    ()
}

let advance2 = lexer => {
    lexer.currentIndex = lexer.currentIndex + 2
    lexer.currentColumn = lexer.currentColumn + 2
    ()
}

let advanceToTheNextLine = lexer => {
    lexer.currentLine = lexer.currentLine + 1
    lexer.currentColumn = 1
}

let rec scanString = lexer => {

    switch lexer->getCurrentCharacter {
    | None => ScanError(UnterminatedString)
    | Some('"') => 
        let value = Js.String.substring(
            ~from=lexer.startIndex + 1, 
            ~to_=lexer.currentIndex,
            lexer.source
        );

        emitLocatedToken(Token.String(value), lexer)
    | Some('\\') if lexer->getNextCharacter == Some('"') =>
        lexer->advance2
        lexer->scanString
    | Some('\n') => 
        lexer->advanceToTheNextLine
        lexer->scanString
    | Some(_) => 
        lexer->advance
        lexer->scanString
    }
}

let rec scanNumber = lexer => {

    switch lexer->getCurrentCharacter { 
    | Some('.') 
    | Some('E')
    | Some('e')
    | Some('-')
    | Some('0' .. '9') => 
        lexer->advance
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

let rec scanKeyword = lexer => {
    switch lexer->getCurrentCharacter { 
    | Some('a' .. 'z') =>
        lexer->advance
        scanKeyword(lexer)
    | None | Some(_) =>
        let value = Js.String.substring(
            ~from=lexer.startIndex, 
            ~to_=lexer.currentIndex,
            lexer.source
        );

        switch value {
        | "true" => emitLocatedToken(Token.True, lexer)
        | "false" => emitLocatedToken(Token.False, lexer)
        | "null" => emitLocatedToken(Token.Null, lexer)
        | _ => ScanError(InvalidKeyword)
        }
    }
}

let scanToken = (lexer) => {
    lexer->advance

    lexer
        ->getCurrentCharacter
        ->Option.mapWithDefault(ReachedTheEnd, character =>
            switch character {
            | '{' => emitLocatedToken(Token.LeftCurlyBracket, lexer)
            | '}' => emitLocatedToken(Token.RightCurlyBrace, lexer)
            | ',' => emitLocatedToken(Token.Comma, lexer)
            | ':' => emitLocatedToken(Token.Colon, lexer)
            | '[' => emitLocatedToken(Token.LeftSquareBracket, lexer)
            | ']' => emitLocatedToken(Token.RightSquareBracket, lexer)
            | ' ' | '\t' | '\r'=> SkippedCharacter
            | '\n' => 
                lexer->advanceToTheNextLine 
                SkippedCharacter
            | '"' => 
                lexer->advance
                lexer->scanString
            | '-' =>
                switch lexer->getNextCharacter {
                | Some(x) if isDigit(x) =>
                    lexer->advance
                    lexer->scanNumber
                | _ => ScanError(UknownCharacter)
                }
            | ch if isDigit(ch) => lexer->scanNumber
            | 'a' .. 'z' => lexer->scanKeyword
            | _ =>
                ScanError(UknownCharacter)
            }
        )
}

let scan = source => {
    let lexer = makeLexer(source)

    let rec loop = (accumulatedTokens, accumulatedErrors) => {
        lexer.startIndex = lexer.currentIndex
        lexer.startLine = lexer.currentLine
        lexer.startColumn = lexer.currentColumn

        switch scanToken(lexer) {
        | SkippedCharacter => loop(accumulatedTokens, accumulatedErrors)
        | ScanError(error) => loop(accumulatedTokens, list{error, ...accumulatedErrors})
        | LocatedToken(token) => loop(list{token, ...accumulatedTokens}, accumulatedErrors)
        | ReachedTheEnd => 

            let eof: Location.located = {
                token: Token.Eof,
                location: {
                    start: {
                        line: lexer.currentLine,
                        column: lexer.currentColumn,
                    },
                    end: {
                        line: lexer.currentLine,
                        column: lexer.currentColumn
                    },
                }
            }
            
            let tokens = list{eof, ...accumulatedTokens}->List.reverse
            let errors = accumulatedErrors->List.reverse

            switch accumulatedErrors {
            | list{} => Ok(tokens)
            | _ => Error(errors, tokens)
            }
        }
    }

    loop(list{}, list{})
}