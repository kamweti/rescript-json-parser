// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Lexer = require("../src/Lexer.bs.js");

function isValidJson(jsonString) {
  var match = Lexer.scan(jsonString);
  if (match.TAG === /* Ok */0) {
    return true;
  } else {
    return false;
  }
}

describe("Jest", (function () {
        test("can scan strings", (function () {
                return expect(isValidJson("\n        {\n            \"weather\" : \"winter\"\n        }")).toBe(true);
              }));
        test("can scan positive number", (function () {
                return expect(isValidJson("\n        {\n            \"age\" : 23\n        }")).toBe(true);
              }));
        test("can scan negative number", (function () {
                return expect(isValidJson("\n        {\n            \"age\" : -84\n        }")).toBe(true);
              }));
      }));

exports.isValidJson = isValidJson;
/*  Not a pure module */
