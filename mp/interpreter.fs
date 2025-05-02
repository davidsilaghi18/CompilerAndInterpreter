

module Interpreter

open Syntax

type value =
    | IntVal of int
    | BoolVal of bool


let rec interp (env: Map<string, value>) (e: exp) : value =
    match e with
    | INT i -> IntVal i
    | BOOL b -> BoolVal b
    | VAR x ->
        match Map.tryFind x env with
        | Some v -> v
        | None -> failwithf "Runtime error: variable '%s' not found" x
    | ADD (e1, e2) ->
        let v1 = interp env e1
        let v2 = interp env e2
        match (v1, v2) with
        | (IntVal i1, IntVal i2) -> IntVal (i1 + i2)
        | _ -> failwith "Runtime error: '+' requires two integers"
    | SUB (e1, e2) ->
        let v1 = interp env e1
        let v2 = interp env e2
        match (v1, v2) with
        | (IntVal i1, IntVal i2) -> IntVal (i1 - i2)
        | _ -> failwith "Runtime error: '-' requires two integers"
    | MUL (e1, e2) ->
        let v1 = interp env e1
        let v2 = interp env e2
        match (v1, v2) with
        | (IntVal i1, IntVal i2) -> IntVal (i1 * i2)
        | _ -> failwith "Runtime error: '*' requires two integers"
    | DIV (e1, e2) ->
        let v1 = interp env e1
        let v2 = interp env e2
        match (v1, v2) with
        | (IntVal _, IntVal 0) -> failwith "Runtime error: division by zero"
        | (IntVal i1, IntVal i2) -> IntVal (i1 / i2)
        | _ -> failwith "Runtime error: '/' requires two integers"
    | MOD (e1, e2) ->
        let v1 = interp env e1
        let v2 = interp env e2
        match (v1, v2) with
        | (IntVal _, IntVal 0) -> failwith "Runtime error: modulo by zero"
        | (IntVal i1, IntVal i2) -> IntVal (i1 % i2)
        | _ -> failwith "Runtime error: '%' requires two integers"
    | EQ (e1, e2) ->
        let v1 = interp env e1
        let v2 = interp env e2
        match (v1, v2) with
        | (IntVal i1, IntVal i2) -> BoolVal (i1 = i2)
        | (BoolVal b1, BoolVal b2) -> BoolVal (b1 = b2)
        | _ -> failwith "Runtime error: '==' requires values of same type"
    | NEQ (e1, e2) ->
        let v1 = interp env e1
        let v2 = interp env e2
        match (v1, v2) with
        | (IntVal i1, IntVal i2) -> BoolVal (i1 <> i2)
        | (BoolVal b1, BoolVal b2) -> BoolVal (b1 <> b2)
        | _ -> failwith "Runtime error: '!=' requires values of same type"
    | LT (e1, e2) ->
        let v1 = interp env e1
        let v2 = interp env e2
        match (v1, v2) with
        | (IntVal i1, IntVal i2) -> BoolVal (i1 < i2)
        | _ -> failwith "Runtime error: '<' requires integers"
    | LTE (e1, e2) ->
        let v1 = interp env e1
        let v2 = interp env e2
        match (v1, v2) with
        | (IntVal i1, IntVal i2) -> BoolVal (i1 <= i2)
        | _ -> failwith "Runtime error: '<=' requires integers"
    | GT (e1, e2) ->
        let v1 = interp env e1
        let v2 = interp env e2
        match (v1, v2) with
        | (IntVal i1, IntVal i2) -> BoolVal (i1 > i2)
        | _ -> failwith "Runtime error: '>' requires integers"
    | GTE (e1, e2) ->
        let v1 = interp env e1
        let v2 = interp env e2
        match (v1, v2) with
        | (IntVal i1, IntVal i2) -> BoolVal (i1 >= i2)
        | _ -> failwith "Runtime error: '>=' requires integers"
    | IF (cond, e_then, e_else) ->
        let v = interp env cond
        match v with
        | BoolVal true -> interp env e_then
        | BoolVal false -> interp env e_else
        | _ -> failwith "Runtime error: IF condition must be a boolean"
    | LET (x, e1, e2) ->
        let v1 = interp env e1
        let env' = env.Add(x, v1)
        interp env' e2
    | AND (e1, e2) ->
        let v1 = interp env e1
        match v1 with
        | BoolVal false -> BoolVal false
        | BoolVal true ->
            let v2 = interp env e2
            match v2 with
            | BoolVal b -> BoolVal b
            | _ -> failwith "Runtime error: AND requires booleans"
        | _ -> failwith "Runtime error: AND requires booleans"
    | OR (e1, e2) ->
        let v1 = interp env e1
        match v1 with
        | BoolVal true -> BoolVal true
        | BoolVal false ->
            let v2 = interp env e2
            match v2 with
            | BoolVal b -> BoolVal b
            | _ -> failwith "Runtime error: OR requires booleans"
        | _ -> failwith "Runtime error: OR requires booleans"
    | _ -> failwith "Not implemented yet"

