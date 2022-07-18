module Main where

import Control.Exception
import Data.Either
import TokenLexer
import Ast
import System.Environment
import System.IO

import OutputLexer



-- outputLex s =  writeFile "example.txt" $ show(alexScanTokens s)

    
main = do
    args <- getArgs 
    if args == [] then
        error "Usage: ./a2 file"
    else do 
        let fn = head args
        file <- openFile fn ReadMode
        s <- hGetContents file
        print(alexScanTokens s)
        let t = gettokens s
        case t of
            Left e  -> lexingError e 
            Right l -> do
                let (pt, toks) = parse $ tokenUnwrap l 
                putStrLn ("\n"++show ( outParseTree pt) )
