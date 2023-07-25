module MathExpr

import Data.List1
import Derive.Prelude
import Text.Lex
import Text.Parse
import Text.Parse.Manual
--import Profile

import Data.String.Parser



data Expr
  = Number Double
  | Variable String
  | Add Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr
  | FunctionCall String Expr


public export
data JSToken : Type where
  Symbol   : Char -> JSToken
  Lit      : Expr -> JSToken
  Space    : JSToken

--%runElab derive "JSToken" [Show,Eq]

public export %inline
fromChar : Char -> JSToken
fromChar = Symbol

numberLit : Lexer
numberLit
  = let sign  = is '-'
        whole = is '0' <|> range '1' '9' <+> many digit
        frac  = is '.' <+> digits
        exp   = like 'e' <+> opt (oneOf ['+','-']) <+> digits in
        opt sign <+> whole <+> opt frac <+> opt exp


jsstring : Lexer
jsstring = quote (is '"') jsonChar
  where
    jsonChar : Lexer
    jsonChar =
          (is '\\' <+> oneOf ['\\','"','n','f','b','r','t','/'])
      <|> (exact "\\u" <+> exactly 4 (pred isHexDigit))
      <|> non (pred isControl <|> is '"' <|> is '\\')

