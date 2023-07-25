module Main

import MathML
import MathMLExamples
import MathExpr

-- Function to check the expected output of the pretty printer

checkPrettyPrint : MathML -> String -> IO ()
checkPrettyPrint expr expected =
  let actual = prettyPrintMathML expr
  in if actual == expected
     then putStrLn "Pass"
     else putStrLn ("Fail: Expected: " ++ expected ++ ", Actual: " ++ actual)

-- Example usage

jsonStr : String
jsonStr = "{ \"name\": \"John\", \"age\": 30, \"city\": \"New York\" }"





main : IO ()
main = do
  checkPrettyPrint fracExample "<mfrac><mn>1</mn><mn>2</mn></mfrac>"
  checkPrettyPrint sqrtExample "<msqrt><mrow><mo>√</mo><mrow><mo>(</mo><mrow><mi>x</mi><mo>+</mo><mn>1</mn></mrow><mo>)</mo></mrow></mrow></msqrt>"
  checkPrettyPrint subExample "<msub><mi>a</mi><mn>1</mn></msub>"
  checkPrettyPrint supExample "<msup><mi>x</mi><mn>2</mn></msup>"
  checkPrettyPrint fracSupExample "<mfrac><msup><mi>a</mi><mn>2</mn></msup><msup><mi>b</mi><mn>3</mn></msup></mfrac>"
  testParse2 jsonStr
