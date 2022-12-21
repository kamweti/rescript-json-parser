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

let advance = lexer => {

    switch String.get(lexer.source, lexer.currentIndex) {
    | char =>
        lexer.currentIndex = lexer.currentIndex + 1
        lexer.currentColumn = lexer.currentColumn + 1
        Some(char)
    | exception _ => None
    }
}

let advanceToTheNextLine = lexer => {
    lexer.currentLine = lexer.currentLine + 1
    lexer.currentColumn = 1
}

let scanToken = (lexer) => {
    lexer
        ->advance
        ->Option.mapWithDefault(ReachedTheEnd, character =>
            switch character {
            | '{' => FoundToken(Token.LeftCurlyBracket)
            | '}' => FoundToken(Token.RightCurlyBrace)
            | ',' => FoundToken(Token.Comma)
            | ':' => FoundToken(Token.Colon)
            | ' ' | '\t' | '\r'=> SkippedCharacter
            | '\n' => 
                advanceToTheNextLine(lexer) 
                SkippedCharacter
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