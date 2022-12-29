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

type singleScanResult =
    | FoundToken(Token.t)
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

let getCharacterAtIndex = (lexer, index) => {
    switch String.get(lexer.source, index) {
    | char =>
        Some(char)
    | exception _ => None
    }
}

let getCharacterAtCurrentIndex = lexer => {
    getCharacterAtIndex(lexer, lexer.currentIndex)
}

let getCharacterAtNextIndex = lexer => {
    getCharacterAtIndex(lexer, lexer.currentIndex + 1)
}

let advance = lexer => {
    lexer.currentIndex = lexer.currentIndex + 1
    lexer.currentColumn = lexer.currentColumn + 1
}

let advanceAndIgnoreResult = lexer => {
    let _ = lexer->advance
}

let advanceToTheNextLine = lexer => {
    lexer.currentLine = lexer.currentLine + 1
    lexer.currentColumn = 1
}

let rec scanString = lexer => {
    switch lexer->getCharacterAtCurrentIndex {
    | None => ScanError(UnterminatedString)
    | Some('"') => 
        let value = Js.String.substring(
            ~from=lexer.startIndex + 1, 
            ~to_=lexer.currentIndex,
            lexer.source
        );

        lexer->advanceAndIgnoreResult
        FoundToken(String(value))
    | Some(c) => 
        if (c === '\n') {
            lexer->advanceToTheNextLine
        }

        lexer->advanceAndIgnoreResult
        lexer->scanString
    }
}

let scanNumber = lexer => {
    let rec peekTheNextNumber = lexer => {
        lexer->advanceAndIgnoreResult

        switch lexer->getCharacterAtCurrentIndex {
        | Some(c) if isDigit(c)=> peekTheNextNumber(lexer)
        | Some(_) | None => ()
        }
    }

    peekTheNextNumber(lexer)

    let value = Js.String.substring(
        ~from=lexer.startIndex, 
        ~to_=lexer.currentIndex,
        lexer.source
    );

    switch Float.fromString(value) { 
    | Some(c) => FoundToken(NumberLiteral(c))
    | None => ScanError(InvalidNumber)
    }
}


let scanToken = (lexer) => {
    lexer->advance

    lexer
        ->getCharacterAtCurrentIndex
        ->Option.mapWithDefault(ReachedTheEnd, character =>
            switch character {
            | '{' => FoundToken(Token.LeftCurlyBracket)
            | '}' => FoundToken(Token.RightCurlyBrace)
            | ',' => FoundToken(Token.Comma)
            | ':' => FoundToken(Token.Colon)
            | ' ' | '\t' | '\r'=> SkippedCharacter
            | '\n' => 
                lexer->advanceToTheNextLine 
                SkippedCharacter
            | '"' => 
                lexer->advanceAndIgnoreResult
                lexer->scanString
            | '-' =>
                switch lexer->getCharacterAtNextIndex {
                | Some(c) if isDigit(c) => 
                    lexer->scanNumber
                | Some(_) | None => ScanError(UknownCharacter)
                }
            | character if isDigit(character) =>
                lexer->scanNumber
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
        | FoundToken(token) => loop(list{token, ...accumulatedTokens}, accumulatedErrors)
        | ReachedTheEnd => 
            
            let tokens = list{Token.Eof, ...accumulatedTokens}->List.reverse
            let errors = accumulatedErrors->List.reverse

            switch accumulatedErrors {
            | list{} => Ok(tokens)
            | _ => Error(errors, tokens)
            }
        }
    }

    loop(list{}, list{})
}