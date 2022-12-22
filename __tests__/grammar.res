open! Jest
open! Expect

let isValidJson = (jsonString) => {
    switch Lexer.scan(jsonString) {
    | Ok(_) => true
    | _ => false
    }
}

describe("Jest", () => {

    test("can scan a string", () => {

        let jsonString = `{
            "city" : "bujumbura"
        }`

        expect(isValidJson(jsonString))->toBe(true)
    })
})