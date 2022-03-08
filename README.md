# Hand written Parser and Interpreter for Stack base Language  

As Source code we have two examples .

Correct one, to run it use  `src/Lib.compileAndRunExample` 

    "LOAD_VAL 1",
    
    "WRITE_VAR 'X'",
    
    "LOAD_VAL 2",
    
    "WRITE_VAR 'Y'",
    
    "READ_VAR 'X'",
    
    "LOAD_VAL 1",
    
    "ADD",
    
    "READ_VAR 'Y'",
    
    "MULTIPLY",
    
    "RETURN_VALUE"
    

Incorrect one, to run it use  `src/Lib.compileAndRunErrorExample`

    "LOAD_VAL 1",
    
    "WRITE_VAR 'X'",
    
    "LOAD_VAL 2",
    
    "WRITE_VAR 'Y'",
    
    "READ_VAR 'Z'",
    
    "LOAD_VAL 1",
    
    "ADD",
    
    "READ_VAR 'X'",
    
    "MULTIPLY",
    
    "RETURN_VALUE",
    
    "MULTIPLY"


## Functionality and design choices

**Data types**

I have assumed that each bytecode program is going to start from `LOAD_VAL` and end on `RETURN_VALUE` and in between zero or more instructions.

so I have made first `Load_VAL` explicit by making it a field `getLoadVal` in `ByteCode`,

same goes with instructions `getInstsAndOp` as list

    type Ident = String
    
    data InstsAndOps
    
    = LoadVal Int
    
    | WriteVar Ident
    
    | ReadVar Ident
    
    | Add
    
    | Multiply
    
    deriving (Show, Eq)
    
    data ByteCode = ByteCode {getLoadVal :: Int, getInstsAndOp :: [InstsAndOps]}
    
    deriving (Show, Eq)

For `RETURN_VALUE` I have made `runByteCode` return type as `Either String Int` so It will always return an `Int`, or run time Error

    runByteCode :: ByteCode -> Either String Int


**Storage**

To store identifiers and respective values `Map`, and for stack `List` have been used.

    type Stack = [Int]
    
    type InternalState = (Map.Map Ident Int, Stack)

**State Management**

Clearly we have to manage two things

1- maintain internal state of program

2 - Instead of throwing error when program is in bad state we need to return Error Result

So `ExpectT` Monad Transformer and State Monad have been used

    type Program a = ExceptT Error (State InternalState) a

**Basic Memory operations**

For most basic memory operations following operations are the bread and butter of all
instructions.

    push :: Int -> Program ()
    
    pop :: Program Int
    
    writeVar :: Ident -> Program ()
    
    readVar :: Ident -> Program ()

The good part is we can add more mathematical operations like divide, Sum, Power etc and we don't have to add or update these basic memory operations.

**Byte Code Execution**

Following Two Methods are mainly responsible for Code executions

    runByteCodeInternal :: ByteCode -> Program Int

    runInstructionAndOperation :: InstsAndOps -> Program ()

`runByteCodeInternal` uses `getLoadVal` to get value and push it into InternalState, then

iterate over `getLoadVal` using `mapM_` and pop stack value and return it.

`runInstructionAndOperation` uses pattern matching on individual `InstsAndOps` and using

basic memory operations, executes it at a time, for example

    runInstructionAndOperation Add = do
    
    x <- pop
    
    y <- pop
    
    push $ x + y