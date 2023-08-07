module End2end

import Parser
import MathML
import Expr

import Data.List1
import Text.Parse



-- Combine parsing, conversion to MathML, and conversion to string
convertMathExprToMathMLString : String -> Either (List1 (FileContext, JSParseErr)) MathML
--convertMathExprToMathMLString formula = (parse2 formula) <*> pure ( mathExprToMathML second) 
convertMathExprToMathMLString formula = mathExprToMathML <$> parse2 formula

