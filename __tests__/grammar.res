open! Jest
open! Expect

let isValidJson = (jsonString) => {
    switch Lexer.scan(jsonString) {
    | Ok(_) => true
    | _ => false
    }
}

describe("Jest", () => {
    test("test the lexer", () => {
        
        let jsonString = `{}`

        expect(isValidJson(jsonString))->toBe(true)

    })
})