module Main

import MathML
import MathMLExamples
import Expr

import End2end

-- Function to check the expected output of the pretty printer




-- Main function with test cases
main : IO ()
main = do


  --let out = case runTests of
  --          Left s => (printParseErrors "d") -- returns a string, doesnt actually prints
  --          Right text => (printParseErrors text) -- returns a string, doesnt actually prints
  
  --let s = convertMathExprToMathMLString
  --let out = either show (printParseErrors s) show (parse2 s)
  
 
  putStrLn (tryComplete "1+9")