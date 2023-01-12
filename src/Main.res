let run = () => {
    let source = ` { "name" : "morio" }`

    switch Lexer.scan(source) {
    | Ok(tokens) => 

        let parser = Parser.make(tokens->List.toArray)
        switch Parser.parse(parser) {
        | Ok(ast) => 
            Js.log(ast->List.toArray)
            ast->List.forEach(item => Js.log(item))
        | _ => ()
        }
    | _ => ()
    }
}

run()