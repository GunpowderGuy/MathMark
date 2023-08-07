module Main

import MathML
import MathMLExamples
import Expr

import End2end

-- Function to check the expected output of the pretty printer

checkPrettyPrint : MathML -> String -> IO ()
checkPrettyPrint expr expected =
  let actual = prettyPrintMathML expr
  in if actual == expected
     then putStrLn "Pass"
     else putStrLn ("Fail: Expected: " ++ expected ++ ", Actual: " ++ actual)

-- Example usage

{- 
main : IO ()
main = do
  --checkPrettyPrint fracExample "<mfrac><mn>1</mn><mn>2</mn></mfrac>"
  --checkPrettyPrint sqrtExample "<msqrt><mrow><mo>√</mo><mrow><mo>(</mo><mrow><mi>x</mi><mo>+</mo><mn>1</mn></mrow><mo>)</mo></mrow></mrow></msqrt>"
  --checkPrettyPrint subExample "<msub><mi>a</mi><mn>1</mn></msub>"
  --checkPrettyPrint supExample "<msup><mi>x</mi><mn>2</mn></msup>"
  --checkPrettyPrint fracSupExample "<mfrac><msup><mi>a</mi><mn>2</mn></msup><msup><mi>b</mi><mn>3</mn></msup></mfrac>"
  --testParse2 jsonStr
  testParse2 "(pi)/2"
-}


-- Example usage
exampleMathExpr : MathExpr
exampleMathExpr = Add2 (Lit3 2.0) (Mul2 (Var2 "x") (Lit3 3.0))

exampleMathML : MathML
exampleMathML = mathExprToMathML exampleMathExpr

-- Main function with test cases
main : IO ()
main = do
  putStrLn "Example MathExpr AST:"
  putStrLn (show exampleMathExpr)
  putStrLn "Converted MathML:"
  putStrLn (show exampleMathML)

  -- Additional test cases
  let test1 = Add2 (Lit3 5.0) (Mul2 (Var2 "x") (Lit3 2.0))
  let test2 = Div2 (Lit3 10.0) (Sub2 (Var2 "y") (Lit3 3.0))

  putStrLn "\nTest case 1 MathExpr AST:"
  putStrLn (show test1)
  putStrLn "Converted MathML:"
  putStrLn (show (mathExprToMathML test1))

  putStrLn "\nTest case 2 MathExpr AST:"
  putStrLn (show test2)
  putStrLn "Converted MathML:"
  putStrLn (show (mathExprToMathML test2))