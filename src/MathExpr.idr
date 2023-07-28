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
data JsonTree : Type where
  Lit3 : Double -> JsonTree
  Add2 : JsonTree -> JsonTree -> JsonTree
  Sub2 : JsonTree -> JsonTree -> JsonTree
  Mul2 : JsonTree -> JsonTree -> JsonTree
  Div2 : JsonTree -> JsonTree -> JsonTree
  Var2 : String -> JsonTree

%runElab derive "JsonTree" [Show, Eq]

public export
data JSToken : Type where
  Symbol   : Char -> JSToken
  Lit      : JsonTree -> JSToken
  Space    : JSToken

%runElab derive "JSToken" [Show, Eq]

public export
data JSErr : Type where
  ExpectedString  : JSErr

%runElab derive "JSErr" [Show, Eq]

public export %inline
fromChar : Char -> JSToken
fromChar = Symbol

export
Interpolation JSToken where
  interpolate (Symbol c) = show c
  interpolate (Lit x)    = "'\{show x}'"
  interpolate Space      = "<spaces>"

export
Interpolation JSErr where
  interpolate ExpectedString  = "Expected string literal"

public export %tcinline
0 JSParseErr : Type
JSParseErr = ParseError JSToken JSErr


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

--Compared to these two lexers, the rest is very simple. All we have to do is to collect the lexers in a TokenMap, where lexers are paired with functions for converting the corresponding lexemes to values of type JSToken:

jsonTokenMap : TokenMap JSToken
jsonTokenMap =
  [ (spaces, const Space)
  , (is '+', const ',')
  , (is '-', const ':')
  , (is '*', const '[')
  , (is '/', const ']')
 --  , (is '{', const '{')
 --  , (is '}', const '}')
 -- , (exact "null", const $ Lit JNull)
 -- , (exact "true", const $ Lit (JBool True))
  --, (exact "false", const $ Lit (JBool False))
  , (numberLit, Lit . Lit3 . cast . cast {to = String})
  , (jsstring, Lit . Var2 . cast)
  ]

tokJSON2 :
     String
  -> Either (Bounded $ ParseError Void Void) (List $ Bounded JSToken)
tokJSON2 = lexManual (first jsonTokenMap)

-- Grammar for mathematical expressions

0 Rule2 : Bool -> Type -> Type
Rule2 b t = Grammar b () JSToken JSErr t


lit : Rule2 True JsonTree

sub : Rule2 True JsonTree

sum : Rule2 True JsonTree

mul : Rule2 True JsonTree

var : Rule2 True JsonTree

div : Rule2 True JsonTree


-- Recursive parser for mathematical expressions
partial
value2 : Rule2 True JsonTree
value2 = lit <|> sub <|> sum <|> mul <|> div <|> var


lit = terminal $ \case Lit j => Just j; _ => Nothing


sub = Sub2 <$> lit <*> (is '-' *> value2)

add2 : Rule2 True JsonTree
add2 = Add2 <$> lit <*> (is '+' *> value2)


sum = add2 <|> value2


mul = Mul2 <$> lit <*> (is '*' *> value2)


div = Div2 <$> lit <*> (is '/' *> value2)


var = Var2 <$> terminal (\case Lit (Var2 v) => Just v; _ => Nothing)

-- Parser entry point
parse2 : String -> Either (List1 (FileContext, JSParseErr)) JsonTree
parse2 s = case tokJSON2 s of
  Left x  => Left (singleton $ fromBounded Virtual $ map fromVoid x)
  Right x => case parse value2 () x of
    Left es                => Left (fromBounded Virtual <$> es)
    Right ((), res, [])    => Right res
    Right ((), _, (x::xs)) => Left (singleton $ fromBounded Virtual $ Unexpected . Right <$> x)


testParse2 : String -> IO ()
testParse2 s = putStrLn $ either (printParseErrors s) show (parse2 s)

public export
runTestCases : List (List JSToken) -> IO ()
runTestCases [] = putStrLn "All test cases passed!"
runTestCases (t::ts) = do
  putStrLn ("Running test case: " ++ show t)
  testParse2 (concatMap show t) -- Convert the list of JSToken to a String
  putStrLn ""
  runTestCases ts


public export
mathTestCases : List (List JSToken)
mathTestCases =
  [ 
    [Symbol '+'],                 
    [Symbol '-'],                 
    [Symbol '*'],                 
    [Symbol '/'],                
    [Lit (Lit3 42.0)],           
    [Lit (Var2 "x")],           
    [Lit (Lit3 3.14), Symbol '*', Lit (Var2 "x")],
    [Lit (Var2 "x"), Symbol '+', Lit (Var2 "y")],
    [Lit (Lit3 10.0), Symbol '+', Lit (Lit3 20.0), Symbol '*', Lit (Lit3 5.0)], 
    [Symbol '(', Lit (Lit3 10.0), Symbol '+', Lit (Lit3 20.0), Symbol ')', Symbol '*', Lit (Lit3 5.0)],
    [Symbol '(', Lit (Lit3 10.0), Symbol '+', Symbol '('], 
    [Lit (Lit3 10.0), Symbol '+' ] 
  ]
