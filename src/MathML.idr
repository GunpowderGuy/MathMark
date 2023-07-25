module MathML

import Data.List
import Derive.Prelude



%default total
%language ElabReflection


public export
data MathML : Type where
  Mrow : List MathML -> MathML
  Mfrac : MathML -> MathML -> MathML
  Msqrt : MathML -> MathML
  Mroot : MathML -> MathML -> MathML -> MathML
  Msub : MathML -> MathML -> MathML
  Msup : MathML -> MathML -> MathML
  Msubsup : MathML -> MathML -> MathML -> MathML
  Munder : MathML -> MathML -> MathML
  Mover : MathML -> MathML -> MathML
  Munderover : MathML -> MathML -> MathML -> MathML
  Mo : String -> MathML
  Mi : String -> MathML
  Mn : String -> MathML

%runElab derive "MathML" [Show,Eq]


public export
partial
prettyPrintMathML : MathML -> String
prettyPrintMathML (Mrow children) = "<mrow>" ++ concatMap prettyPrintMathML children ++ "</mrow>"
prettyPrintMathML (Mfrac num denom) = "<mfrac>" ++ prettyPrintMathML num ++ prettyPrintMathML denom ++ "</mfrac>"
prettyPrintMathML (Msqrt expr) = "<msqrt>" ++ prettyPrintMathML expr ++ "</msqrt>"
prettyPrintMathML (Mroot base degree expr) = "<mroot>" ++ prettyPrintMathML base ++ prettyPrintMathML degree ++ prettyPrintMathML expr ++ "</mroot>"
prettyPrintMathML (Msub base subscript) = "<msub>" ++ prettyPrintMathML base ++ prettyPrintMathML subscript ++ "</msub>"
prettyPrintMathML (Msup base superscript) = "<msup>" ++ prettyPrintMathML base ++ prettyPrintMathML superscript ++ "</msup>"
prettyPrintMathML (Msubsup base subscript superscript) = "<msubsup>" ++ prettyPrintMathML base ++ prettyPrintMathML subscript ++ prettyPrintMathML superscript ++ "</msubsup>"
prettyPrintMathML (Munder base underscript) = "<munder>" ++ prettyPrintMathML base ++ prettyPrintMathML underscript ++ "</munder>"
prettyPrintMathML (Mover base overscript) = "<mover>" ++ prettyPrintMathML base ++ prettyPrintMathML overscript ++ "</mover>"
prettyPrintMathML (Munderover base underscript overscript) = "<munderover>" ++ prettyPrintMathML base ++ prettyPrintMathML underscript ++ prettyPrintMathML overscript ++ "</munderover>"
prettyPrintMathML (Mo op) = "<mo>" ++ op ++ "</mo>"
prettyPrintMathML (Mi ident) = "<mi>" ++ ident ++ "</mi>"
prettyPrintMathML (Mn num) = "<mn>" ++ num ++ "</mn>"

