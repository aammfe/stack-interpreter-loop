module Lib
  ( someFunc,
  )
where

import Data.Either.Combinators (maybeToRight)
import Interpreter
import Parser


compileAndRunExample :: Either String Int
compileAndRunExample = compileAndRun exampleCode

compileAndRunErrorExample :: Either String Int
compileAndRunErrorExample = compileAndRun errorCode

compileAndRun :: String -> Either String Int
compileAndRun code = do
  parsed <- maybeToRight "Compilation Error" $ parseCode code
  runByteCode (fst parsed)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
