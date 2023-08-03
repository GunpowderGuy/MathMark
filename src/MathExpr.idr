module MathExpr

import Data.List1
import Derive.Prelude
import Text.Lex
import Text.Parse
import Text.Parse.Manual
import Data.Either

--%default total
%language ElabReflection

public export
data MathExpr : Type where
  Lit3 : Double -> MathExpr
  Add2 : MathExpr -> MathExpr -> MathExpr
  Sub2 : MathExpr -> MathExpr -> MathExpr
  Mul2 : MathExpr -> MathExpr -> MathExpr
  Div2 : MathExpr -> MathExpr -> MathExpr
  Var2 : String -> MathExpr

%runElab derive "MathExpr" [Show, Eq]

public export
data MathToken : Type where
  Symbol   : Char -> MathToken
  Lit      : MathExpr -> MathToken
  Space    : MathToken

%runElab derive "MathToken" [Show, Eq]

public export
data JSErr : Type where
  ExpectedString  : JSErr

%runElab derive "JSErr" [Show, Eq]

public export %inline
fromChar : Char -> MathToken
fromChar = Symbol

export
Interpolation MathToken where
  interpolate (Symbol c) = show c
  interpolate (Lit x)    = "'\{show x}'"
  interpolate Space      = "<spaces>"

export
Interpolation JSErr where
  interpolate ExpectedString  = "Expected string literal"

public export %tcinline
0 JSParseErr : Type
JSParseErr = ParseError MathToken JSErr


--
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

--Compared to these two lexers, the rest is very simple. All we have to do is to collect the lexers in a TokenMap, where lexers are paired with functions for converting the corresponding lexemes to values of type MathToken:

jsonTokenMap : TokenMap MathToken
jsonTokenMap =
  [ (spaces, const Space)
  , (is '+', const (Symbol '+'))
  , (is '-', const (Symbol '-'))
  , (is '*', const (Symbol '*'))
  , (is '/', const (Symbol '/'))
  , (is '(', const (Symbol '('))
  , (is ')', const (Symbol ')'))
  , (numberLit, Lit . Lit3 . cast . cast {to = String})
  , (jsstring, Lit . Var2 . cast)
  ]

tokJSON2 :
     String
  -> Either (Bounded $ ParseError Void Void) (List $ Bounded MathToken)
tokJSON2 = lexManual (first jsonTokenMap)

-- Grammar for mathematical expressions

0 Rule2 : Bool -> Type -> Type
Rule2 b t = Grammar b () MathToken JSErr t


lit : Rule2 True MathExpr

sub : Rule2 True MathExpr

sum : Rule2 True MathExpr

mul : Rule2 True MathExpr

var : Rule2 True MathExpr

div : Rule2 True MathExpr


-- Recursive parser for mathematical expressions
partial
value2 : Rule2 True MathExpr
value2 = lit <|> sub <|> sum <|> mul <|> div <|> var


lit = terminal $ \case Lit j => Just j; _ => Nothing

var = Var2 <$> terminal (\case Lit (Var2 v) => Just v; _ => Nothing)
atom = lit <|> var <|> is '(' *> sum <* is ')'
div = foldl Div2 <$> atom <*> many (is '/' *> atom)
mul = foldl Mul2 <$> div <*> many (is '*' *> div)
sub = foldl Sub2 <$> mul <*> many (is '-' *> mul)
sum = foldl Add2 <$> sub <*> many (is '+' *> sub)

{-
sub = Sub2 <$> lit <*> (is '-' *> value2)

add2 : Rule2 True MathExpr
add2 = Add2 <$> lit <*> (is '+' *> value2)

sum = add2 <|> value2

mul = Mul2 <$> lit <*> (is '*' *> value2)

div = Div2 <$> lit <*> (is '/' *> value2)

var = Var2 <$> terminal (\case Lit (Var2 v) => Just v; _ => Nothing)
-}

-- Parser entry point
parse2 : String -> Either (List1 (FileContext, JSParseErr)) MathExpr
parse2 s = case tokJSON2 s of
  Left x  => Left (singleton $ fromBounded Virtual $ map fromVoid x)
--Right x => case parse value2 () x of
  Right x => case parse sum () x of
    Left es                => Left (fromBounded Virtual <$> es)
    Right ((), res, [])    => Right res
    Right ((), _, (x::xs)) => Left (singleton $ fromBounded Virtual $ Unexpected . Right <$> x)


public export
testParse2 : String -> IO ()
testParse2 s = putStrLn $ either (printParseErrors s) show (parse2 s)

public export
runTestCases : List String -> IO ()
runTestCases [] = putStrLn "All test cases passed!"
runTestCases (t::ts) = do
  putStrLn ("Running test case: " ++ show t)
  --testParse2 (concatMap show t) -- Convert the list of MathToken to a String
  putStrLn ""
  runTestCases ts


public export
mathTestCases : List String
mathTestCases =
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
