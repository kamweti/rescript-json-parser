open! Jest
open! Expect

let isValidJson = (jsonString) => {
    switch Lexer.scan(jsonString) {
    | Ok(_) => true
    | _ => false
    }
}

describe("Jest", () => {

    test("can scan strings", () => {

        let jsonString = `
        {
            "weather" : "winter"
        }`

        expect(isValidJson(jsonString))->toBe(true)
    })

    test("can scan positive number", () => {

        let jsonString = `
        {
            "age" : 23
        }`

        expect(isValidJson(jsonString))->toBe(true)
    })

    test("can scan negative number", () => {
        let jsonString = `
        {
            "age" : -84
        }`

        expect(isValidJson(jsonString))->toBe(true)
    })
})