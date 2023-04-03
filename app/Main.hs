module Main where

import System.IO
import System.Console.Isocline

import Eval
import Sugar
import Exp
import Parsing
import Printing
import REPLCommand

main :: IO ()
main = do 
  Load s -> putStrLn ("notImplemented") >> main
                        Eval l -> case parse exprParser "<input>" l of
                            Left err -> print err >> main
                            Right c -> (putStrLn . showExp . sugarExp . normalize . desugarExp $ c) >> main