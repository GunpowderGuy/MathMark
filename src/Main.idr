module Main

import MathML
import MathMLExamples
import Expr

import End2end

-- Function to check the expected output of the pretty printer


public export
mathTestCases2 : List String
mathTestCases2 =
  [ 
    "+",
    "5 + 2",
    "5 * 2",
    "5 - (3 + 2)",
    "(5 + 2) * (3 - 1)",
    "x * (y + z)",
    "x + y * z",
    "dfsdfdhj error"
  ]

-- Main function with test cases
main : IO ()
main = do
  
 
  putStrLn (tryComplete "summation(1,2,3,4)")
  putStrLn (tryComplete  "x + y * z")
  putStrLn (tryComplete "5 + 2")
  putStrLn (tryComplete "x * (y + z)")
  putStrLn (tryComplete "error")
  

  