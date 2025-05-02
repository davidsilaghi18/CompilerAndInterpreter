#r "nuget: FsLexYacc.Runtime, 10.2.0"
#r "vm.dll"
#r "asm.dll"

#load "syntax.fs"

#load "parser.fs"
#load "lexer.fs"
#load "parse.fs"
#load "compiler.fs"

#load "interpreter.fs"

open Syntax
open Interpreter

//#load "tests.fs"   here u can delete the //  but look down farst !!
//open Tests         also here

// Add more F# code here
let env = Map.ofList [ ("x", IntVal 10) ]


let expr1 = INT 42
let result1 = interp env expr1
printfn "Result1: %A" result1  // Should print: IntVal 42

let expr2 = BOOL true
let result2 = interp env expr2
printfn "Result2: %A" result2  // Should print: BoolVal true

let expr3 = VAR "x"
let result3 = interp env expr3
printfn "Result3: %A" result3  // Should throw: Runtime error: variable 'x' not found
// IF test: if true then 1 else 2
let exprIf = IF (BOOL true, INT 1, INT 2)
let resultIf = interp env exprIf
printfn "Result IF: %A" resultIf  // Should print: IntVal 1

// LET test: let x = 5 in x + 3
let exprLet = LET ("x", INT 5, ADD (VAR "x", INT 3))
let resultLet = interp env exprLet
printfn "Result LET: %A" resultLet  // Should print: IntVal 8

// AND test: true && false = false
let exprAnd = AND (BOOL true, BOOL false)
let resultAnd = interp env exprAnd
printfn "Result AND: %A" resultAnd  // Should print: BoolVal false

// OR test: false || true = true
let exprOr = OR (BOOL false, BOOL true)
let resultOr = interp env exprOr
printfn "Result OR: %A" resultOr  // Should print: BoolVal true


let comps s = Asm.asm (Compiler.compProg (Parse.fromString s));;
let compf f = Asm.asm (Compiler.compProg (Parse.fromFile f));;

let testWithMessage msg f x =
  printf "[%s]\n" msg
  let result = f x
  result

let test s = 
  printf "Testing program:\n"
  printf "      %s\n" s
  let ast  = testWithMessage "Parse.fromString" Parse.fromString s
  let trg  = testWithMessage "Compiler.compProg" Compiler.compProg ast
  let code = testWithMessage "Asm.asm" Asm.asm trg 
  let v    = testWithMessage "VM.exec" VM.exec code 
  printf "  Result = %d\n\n" v 

// Below here if u delete the // (also up I let coments) then u can test automatly the testing from test.fs

// printfn "\n=== Running tests from tests.fs ===\n"       
// for s in tests do
//     try
//         test s
//     with ex ->
//         printfn "Test failed: %s\n-> %s\n" s ex.Message

