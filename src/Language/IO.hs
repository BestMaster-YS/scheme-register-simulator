module Language.IO where

import System.IO.Unsafe
import Language.Syntax
import Language.Parser
import Parser


unsafePrompt :: String -> ()
unsafePrompt = unsafePerformIO . putStrLn

-- read, 在 scheme 中，read 以读取一个整的 scheme 表达式为单元，而不是以行数为单元
readExp :: String -> IO String
readExp exps = do
  exp <- getLine
  let res = runParser parseScheme (exps ++ "\n" ++ exp)
  case res of
    ParseResult a _ -> do
      return (exps ++ "\n" ++ exp)
    ParseError e a -> do
      error ("scheme syntax error: " ++ a)
    ParsePart -> do
      readExp (exps ++ "\n" ++ exp)
