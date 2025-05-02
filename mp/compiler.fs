module Compiler

// Find the position of variable x on the runtime stack:
let rec varpos x = function
    | []       -> failwith ("unbound: " + x)
    | y :: env -> if x = y then 0 else 1 + varpos x env

// Generate fresh labels for IF expressions and jumps
let mutable labelCounter = 0
let newLabel() =
    let this = labelCounter
    labelCounter <- this + 1
    "L" + string(this)

// Compile an expression:
let rec comp env = function
    | Syntax.INT i           -> [Asm.IPUSH i]
    | Syntax.VAR x           -> [Asm.ILOAD (varpos x env)]

    // Implemented: support for boolean constants
    // true = 1, false = 0
    | Syntax.BOOL true       -> [Asm.IPUSH 1]
    | Syntax.BOOL false      -> [Asm.IPUSH 0]

    // Arithmetic operators
    | Syntax.ADD (e1, e2)    -> comp env e1 @ comp ("" :: env) e2 @ [Asm.IADD]
    | Syntax.SUB (e1, e2)    -> comp env e1 @ comp ("" :: env) e2 @ [Asm.ISUB]
    | Syntax.MUL (e1, e2)    -> comp env e1 @ comp ("" :: env) e2 @ [Asm.IMUL]
    | Syntax.DIV (e1, e2)    -> comp env e1 @ comp ("" :: env) e2 @ [Asm.IDIV]
    | Syntax.MOD (e1, e2)    -> comp env e1 @ comp ("" :: env) e2 @ [Asm.IMOD]

    // Unary negation: -e becomes 0 - e
    | Syntax.NEG e           -> [Asm.IPUSH 0] @ comp env e @ [Asm.ISUB]

    // Comparison: == equal
    | Syntax.EQ (e1, e2)     -> comp env e1 @ comp (""::env) e2 @ [Asm.IEQ]

    // Comparison: != using double negation trick !(a == b) = a == b -> 0 -> 1
    | Syntax.NEQ (e1, e2)    ->
        comp env e1 @
        comp (""::env) e2 @
        [Asm.IEQ] @
        [Asm.IPUSH 0] @
        [Asm.IEQ]

    //  FIXED Comparison: a < b
    | Syntax.LT (e1, e2) ->
        comp env e1 @
        comp ("" :: env) e2 @
        [Asm.ILT]

    //  FIXED Comparison: a > b
    | Syntax.GT (e1, e2) ->
        comp env e2 @
        comp ("" :: env) e1 @
        [Asm.ILT]

    //  FIXED Comparison: a <= b
    | Syntax.LTE (e1, e2) ->
        comp env e2 @
        comp ("" :: env) e1 @
        [Asm.ILT] @
        [Asm.IPUSH 0] @
        [Asm.IEQ]

    //  FIXED Comparison: a >= b
    | Syntax.GTE (e1, e2) ->
        comp env e1 @
        comp ("" :: env) e2 @
        [Asm.ILT] @
        [Asm.IPUSH 0] @
        [Asm.IEQ]

    // Let binding
    | Syntax.LET (x, e1, e2) ->
        comp env e1 @
        comp (x :: env) e2 @
        [Asm.ISWAP] @ [Asm.IPOP]

    // IF expression without using IJZ (unsupported in asm.dll)
    // Simulated using jumps:
    // If e1 == 0 → jump to else block
    // Otherwise → execute then block and jump past else block
    | Syntax.IF (e1, e2, e3) ->
        let l_else = newLabel ()  // Label for the else branch
        let l_end = newLabel ()   // Label for the end of the if-expression
        comp env e1 @
        [Asm.IPUSH 0] @           // Push 0 to compare against e1
        [Asm.IEQ] @               // If e1 == 0 (false), jump to else
        [Asm.IJMP l_else] @
        comp env e2 @            // Compile then branch
        [Asm.IJMP l_end] @
        [Asm.ILAB l_else] @      // Else label
        comp env e3 @            // Compile else branch
        [Asm.ILAB l_end]         // End label


    // Short-circuit AND:
    // If e1 is false (0), we skip evaluating e2 and directly return false (0).
    // Otherwise, we evaluate e2 and return its result (either 0 or 1).
    | Syntax.AND (e1, e2) ->
        let l_false = newLabel()  // Label for jumping if e1 is false
        let l_end = newLabel()    // Label for the end of the AND expression
        comp env e1 @             // Compile e1 and leave its value on the stack
        [Asm.IPUSH 0] @           // Push 0 for comparison
        [Asm.IEQ] @               // Check if e1 == 0
        [Asm.IJMP l_false] @      // If e1 is false, jump to l_false
        comp env e2 @             // Otherwise, evaluate e2
        [Asm.IJMP l_end] @        // Jump to the end after e2
        [Asm.ILAB l_false] @      // Label if e1 was false
        [Asm.IPUSH 0] @           // Push 0 as the result (false)
        [Asm.ILAB l_end]          // Label at the end of the AND expression


    // Short-circuit OR:
    // If e1 is true (1), we skip evaluating e2 and directly return true (1).
    // Otherwise, we evaluate e2 and return its result (either 0 or 1).
    | Syntax.OR (e1, e2) ->
        let l_true = newLabel()   // Label for jumping if e1 is true
        let l_end = newLabel()    // Label for the end of the OR expression
        comp env e1 @             // Compile e1 and leave its value on the stack
        [Asm.IPUSH 1] @           // Push 1 for comparison
        [Asm.IEQ] @               // Check if e1 == 1
        [Asm.IJMP l_true] @       // If e1 is true, jump to l_true
        comp env e2 @             // Otherwise, evaluate e2
        [Asm.IJMP l_end] @        // Jump to the end after e2
        [Asm.ILAB l_true] @       // Label if e1 was true
        [Asm.IPUSH 1] @           // Push 1 as the result (true)
        [Asm.ILAB l_end]          // Label at the end of the OR expression


    // Function call with one argument
    | Syntax.CALL (f, [e1]) ->
        comp env e1 @
        [Asm.ICALL f] @
        [Asm.ISWAP] @
        [Asm.IPOP]

    // Updated CALL case to correctly handle any number of arguments
    | Syntax.CALL (f, es) ->
    // First compile all the arguments
    let compiledArgs = List.collect (comp env) es
    compiledArgs @ [Asm.ICALL f] @
    
    // After ICALL:
    // Swap the return value with each argument one by one
    (List.replicate (List.length es) Asm.ISWAP) @

    // Then pop each argument from the stack
    (List.replicate (List.length es) Asm.IPOP) @

    // Finally pop the return address
    [Asm.IPOP]

    // IO implemented: READ gets input, WRITE prints result
    | Syntax.READ -> [Asm.IREAD]  
    | Syntax.WRITE e -> comp env e @ [Asm.IWRITE] // Fixed: use IDUP so value stays on stack after IWRITE


    | _ -> failwith "missing: comp operator"

// Compile the full program: function definitions + main expression
let rec compProg = function
    // If there are no functions, just compile the main expression
    | ([], e1) ->
        comp [] e1 @ [Asm.IHALT]

    // This handles function definitions with ANY number of parameters (0, 1, 2, 3, ...)
    | ((f, (xs, e)) :: funcs, e1) ->
        // First compile the rest of the program (other function definitions and the main expression)
        compProg (funcs, e1) @
        // Create a label for the function
        [Asm.ILAB f] @
        // Add "" as return address marker, and reverse the argument list for correct stack order
        comp ("" :: List.rev xs) e @
        // Return from the function
        [Asm.IRETN]
