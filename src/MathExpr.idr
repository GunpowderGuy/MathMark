module MathExpr

import Data.List1
import Derive.Prelude
import Text.Lex
import Text.Parse
import Text.Parse.Manual
import Data.Either


%default total
%language ElabReflection

public export
data JsonTree : Type where
  Lit3 : Double -> JsonTree
  Add2 : JsonTree -> JsonTree -> JsonTree
  Sub2 : JsonTree -> JsonTree -> JsonTree
  Mul2 : JsonTree -> JsonTree -> JsonTree
  Div2 : JsonTree -> JsonTree -> JsonTree
  Var2 : String -> JsonTree

%runElab derive "JsonTree" [Show,Eq]


public export
data JSToken : Type where
  Symbol   : Char -> JSToken
  Lit      : JsonTree -> JSToken
  Space    : JSToken

%runElab derive "JSToken" [Show,Eq]


public export
data JSErr : Type where
  ExpectedString  : JSErr

%runElab derive "JSErr" [Show,Eq]


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
 

tokJSON2 :
     String
  -> Either (Bounded $ ParseError Void Void) (List $ Bounded JSToken)


--START
--START

0 Rule2 : Bool -> Type -> Type
Rule2 b t = Grammar b () JSToken JSErr t

covering
sub : Rule2 True JsonTree

covering
sum : Rule2 True JsonTree

lit : Rule2 True JsonTree
lit = terminal $ \case Lit j => Just j; _ => Nothing


covering
value2 : Rule2 True JsonTree
value2 = lit <|> sub <|> sum

partial
subs : Rule2 True JsonTree

sub = subs <|> value2

subs = Sub2 <$> lit <*> (is '-' *> between (is '(') (is ')' ) value2)

partial
add2 : Rule2 True JsonTree

sum = add2 <|> value2

add2 = Add2 <$> lit <*> (is '+' *> between (is '(') (is ')' ) value2)



covering
parse2 : String -> Either (List1 (FileContext,JSParseErr)) JsonTree
parse2 s = case tokJSON2 s of
  Left x  => Left (singleton $ fromBounded Virtual $ map fromVoid x)
  Right x => case parse value2 () x of
    Left es                => Left (fromBounded Virtual <$> es)
    Right ((),res,[])      => Right res
    Right ((),res,(x::xs)) =>
      Left (singleton $ fromBounded Virtual $ Unexpected . Right <$> x)


covering
testParse2 : String -> IO ()
testParse2 s = putStrLn $ either (printParseErrors s) show (parse2 s)


jsonStr : String
jsonStr = #"{"tree":{"name":true,"kids":[{"name":false,"kids":[{"name":null,"kids":[{"name":"pkg","kids":[{"name":"exp","kids":[{"name":"draw","kids":[{"name":"Makefile","kids":[],"cl_weight":1,"touches":1,"min_t":1258062920,"max_t":1258062920,"mean_t":1258062920}],"cl_weight":1,"touches":1,"min_t":1258062920,"max_t":1258062920,"mean_t":1258062920}],"cl_weight":2,"touches":2,"min_t":1258062920,"max_t":1316289444,"mean_t":1287176182}],"cl_weight":172.302597402597,"touches":174,"min_t":1254251724,"max_t":1316289444,"mean_t":1283150599}],"cl_weight":176.4999999999996,"touches":177,"min_t":1254251724,"max_t":1316289444,"mean_t":1282723881},{"name":"misc","kids":[],"cl_weight":3,"touches":3,"min_t":1255542979,"max_t":1264539389,"mean_t":1261000371},{"name":"doc","kids":[{"name":"effective_go.html","kids":[],"cl_weight":1,"touches":1,"min_t":1258401378,"max_t":1258401378,"mean_t":1258401378},{"name":"install.html","kids":[],"cl_weight":1,"touches":1,"min_t":1257967097,"max_t":1257967097,"mean_t":1257967097},{"name":"go-logo-black.png","kids":[],"cl_weight":0.2,"touches":1,"min_t":1257452334,"max_t":1257452334,"mean_t":1257452334},{"name":"video-snap.jpg","kids":[],"cl_weight":0.2,"touches":1,"min_t":1257452334,"max_t":1257452334,"mean_t":1257452334},{"name":"root.html","kids":[],"cl_weight":0.45,"touches":2,"min_t":1257307185,"max_t":1257452334,"mean_t":1257379759},{"name":"style.css","kids":[],"cl_weight":0.45,"touches":2,"min_t":1257307185,"max_t":1257452334,"mean_t":1257379759},{"name":"go-logo-blue.png","kids":[],"cl_weight":0.25,"touches":1,"min_t":1257307185,"max_t":1257307185,"mean_t":1257307185}],"cl_weight":3.5500000000000007,"touches":4,"min_t":1257307185,"max_t":1258401378,"mean_t":1257781998},{"name":"lib","kids":[{"name":"godoc","kids":[{"name":"godoc.html","kids":[],"cl_weight":0.45,"touches":2,"min_t":1257307185,"max_t":1257452334,"mean_t":1257379759}],"cl_weight":0.45,"touches":2,"min_t":1257307185,"max_t":1257452334,"mean_t":1257379759}],"cl_weight":0.45,"touches":2,"min_t":1257307185,"max_t":1257452334,"mean_t":1257379759}],"cl_weight":0,"touches":0,"min_t":0,"max_t":0,"mean_t":0}],"cl_weight":0,"touches":0,"min_t":0,"max_t":0,"mean_t":0},"username":"agl"}"#

--covering
--bench : Benchmark Void
--bench = Group "all" [
 --   Group "lex" [
 --     Single "manual"     (basic tokJSON jsonStr)
  --  , Single "combinator" (basic tokJSON2 jsonStr)
   -- ]
 -- , Group "parse" [
  --    Single "manual"     (basic parse1 jsonStr)
   -- , Single "combinator" (basic parse2 jsonStr)
   -- ]
 -- ]

--covering
--main : IO ()
--main = runDefault (const True) Table show bench