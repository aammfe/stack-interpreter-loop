module Interpreter where

import Control.Monad.Trans.Class (lift)
import Control.Monad(when)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import qualified Data.Map as Map

type Ident = String

data InstsAndOps
  = LoadVal Int
  | WriteVar Ident
  | ReadVar Ident
  | Add
  | Multiply
  | Loop [InstsAndOps]
  deriving (Show, Eq)

data ByteCode = ByteCode {getLoadVal :: Int, getInstsAndOp :: [InstsAndOps]}
  deriving (Show, Eq)

type Stack = [Int]
type InternalState = (Map.Map Ident Int, Stack)

emptyInternalState :: InternalState
emptyInternalState = (Map.empty, [])

type Error = String

type Program a = ExceptT Error (State InternalState) a

getl :: Program InternalState
getl = lift get

putl :: InternalState -> Program ()
putl = lift . put

push :: Int -> Program ()
push value = do
  (m, stack) <- getl
  putl (m, value : stack)

pop :: Program Int
pop = do
  (m, stack) <- getl
  popMe (m, stack)
  where
    popMe (_, []) = throwE "can not pop, stack is already empty"
    popMe (m, x : xs) = do
      putl (m, xs)
      return x

writeVar :: Ident -> Program ()
writeVar ident = do
  poped <- pop
  (m, stack) <- getl
  let m' = Map.insert ident poped m
  putl (m', stack)

readVar :: Ident -> Program ()
readVar ident = do
  (m, stack) <- getl
  readMe (Map.lookup ident m)
  where
    readMe Nothing = throwE $ ident ++ " not found"
    readMe (Just x) = push x

runInstructionAndOperation :: InstsAndOps -> Program ()
runInstructionAndOperation (LoadVal i) = push i
runInstructionAndOperation (WriteVar ident) = writeVar ident
runInstructionAndOperation (ReadVar ident) = readVar ident
runInstructionAndOperation Add = do
  x <- pop
  y <- pop
  push $ x + y
runInstructionAndOperation Multiply = do
  x <- pop
  y <- pop
  push $ x * y

runInstructionAndOperation loop@(Loop insts) = do
    x <- pop
    push x
    when (x > 0) $ do
                mapM_ runInstructionAndOperation insts  
                runInstructionAndOperation loop

runByteCodeInternal :: ByteCode -> Program Int
runByteCodeInternal (ByteCode i instsAndOps) = do
  push i
  mapM_ runInstructionAndOperation instsAndOps
  pop

runByteCode :: ByteCode -> Either String Int
runByteCode = fst . (`runState` emptyInternalState) . runExceptT . runByteCodeInternal
