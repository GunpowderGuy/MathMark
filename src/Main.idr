module Main

import MathML
import MathMLExamples
import Expr

import End2end

-- Function to check the expected output of the pretty printer




-- Main function with test cases
main : IO ()
main = do


  let out : String = case runTests of
            Left s => "error"
            Right text => text

 
  putStrLn out