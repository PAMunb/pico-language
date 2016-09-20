{-# LANGUAGE OverloadedStrings #-}
module Pico.Parser where

import Prelude hiding ((>>=), return, fail, GT, LT)
import Pico.Syntax 
import Pico.YAHCL


-- | Parser for a PICO program. This parser is
-- implemented using the YAHCL module, another monodaic
-- combinator library. 
parsePicoProgram :: Parser Program
parsePicoProgram =
  token "begin"       >>= \_     ->
  token "declare" >>= \_     -> 
  declarations        >>= \decls ->
  statements          >>= \stmts ->
  token "end."        >>= \_     -> 
  return (Program decls [])

-- | A parser for a list of variable declarations in PICO. 
declarations = separator ',' declaration >>= \decls -> token ";" >>= \_ -> return decls

-- | A parser for a list of PICO statements. 
statements   = separator ';' statement

-- | A parser for a PICO statement. 
statement :: Parser Statement
statement = assignment <|> ifThenElse <|> ifThen <|> while

-- | A parser for an assignment statement. 
assignment :: Parser Statement
assignment = identifier >>= \var ->
             token ":="  >>= \_   -> 
             expression >>= \exp -> return (Assignment var exp)

-- | A parser for an IfThenElse statement
ifThenElse :: Parser Statement
ifThenElse =
  token "if" >>= \_   ->
  token "(" >>= \_    -> 
  expression >>= \cond ->
  token ")" >>= \_    -> 
  token "then" >>= \_ ->
  statements >>= \thenStatements ->
  token "else" >>= \_ ->
  statements >>= \elseStatements ->
  token "endif" >>= \_ -> 
  return $ IfThenElse cond (Block thenStatements) (Block elseStatements)

-- | A parser for an IfThen statement
ifThen :: Parser Statement
ifThen = 
  token "if" >>= \_   ->
  token "(" >>= \_    -> 
  expression >>= \cond ->
  token ")" >>= \_    -> 
  token "then" >>= \_ ->
  statements >>= \thenStatements ->
  token "endif" >>= \_ -> 
  return $ IfThen cond (Block thenStatements)

while :: Parser Statement
while =
  token "while" >>= \_  ->
  token "(" >>= \_      -> 
  expression >>= \cond   ->
  token ")" >>= \_      -> 
  token "do" >>= \_     ->
  blanks >>= \_          -> 
  statements >>= \stmts  ->
  token "od" >>= \_      -> 
  return $ While cond (Block stmts)

-- | A parser for a single declaration. 
declaration :: Parser Declaration
declaration =
  blanks         >>= \_ -> 
  identifier     >>= \v ->
  blanks         >>= \_ -> 
  token ":"      >>= \_ ->
  blanks         >>= \_ -> 
  (nat <|> str)  >>= \t ->
  blanks         >>= \_ ->
  return (v, t) 

identifier :: Parser Id
identifier = (letter >>= \c -> many (letter <|> digit) >>= \cs -> return (c:cs))

-- | A parser for Expressions in the PICO Language
-- Due to the challenges of building decendent recursive
-- parsers for left-recursive grammars, I have decided
-- to change the concrete syntax of binary expressions, which
-- should be written using the prefix notation. 

-- expression :: Parser Expression
-- expression =  value <|> var <|> binExp 

-- expression :: Parser Expression
-- expression = binExp

value :: Parser Expression
value =  (natural >>= \v -> return $ ExpValue (NATValue v))
     <|> (text    >>= \v -> return $ ExpValue (STRValue v))

var :: Parser Expression
var = blanks >>= \_       ->
      identifier >>= \var ->
      blanks >>= \_       ->
      return (Var var)



expression :: Parser Expression
expression =
  exp1 >>= \e   ->
  ((blanks >>= \_ ->
   (symbol '>' <|> symbol '<') >>= \opr ->
   blanks >>= \_ ->
   expression >>= \exp ->
   return $ (consExp opr) e exp) <|> return e) 
 where
   consExp '>' = GT
   consExp '<' = LT

exp1 :: Parser Expression
exp1 =
  term >>= \ter   ->
  ((blanks >>= \_ ->
   (symbol '+' <|> symbol '-') >>= \opr ->
   blanks >>= \_ ->
   exp1 >>= \exp ->
   return $ (consExp opr) ter exp) <|> return ter) 
 where
   consExp '+' = Add
   consExp '-' = Sub

   
term :: Parser Expression
term =
  factor >>= \fac -> 
  ((blanks >>= \_ ->
   (symbol '*' <|> symbol '/' <|> symbol '^' <|> symbol '|') >>= \opr ->
    blanks >>= \_ -> 
    term >>= \ter ->
    return $ (consExp opr) fac ter) <|> return fac)
 where
   consExp '*' = Mult
   consExp '/' = Div
   consExp '|' = Concat
   consExp '^' = Pow
   
factor :: Parser Expression
factor = value <|> var <|> (token "(" >>= \_ -> expression >>= \exp -> token ")" >>= \_ -> return exp)

-- binExp :: Parser Expression
-- binExp =
--       blanks >>=       \_         ->
--       char   >>=       \opr       ->
--       blanks >>=       \_         ->
--       sat (== '(') >>= \_         ->
--       blanks >>=       \_         ->
--       expression >>=   \exp1      ->
--       blanks >>=       \_         ->
--       sat (== ',') >>= \_         ->
--       blanks >>=       \_         -> 
--       expression >>=   \exp2      ->
--       blanks >>=       \_         ->
--       sat (== ')') >>= \_         -> 
--       return ((cons opr) exp1 exp2)
--  where
--    cons '+' = Add
--    cons '-' = Sub
--    cons '*' = Mult
--    cons '^' = Pow
--    cons '/' = Div
--    cons '|' = Concat
    

-- binExp :: Parser Expression
-- binExp =
--       blanks >>=       \_         ->
--       char >>=         \opr       ->
--       blanks >>=       \_         ->
--       sat (== '(') >>= \_         ->
--       blanks >>=       \_         ->
--       expression >>=   \exp1      ->
--       blanks >>=       \_         ->
--       sat (== ',') >>= \_         ->
--       blanks >>=       \_         -> 
--       expression >>=   \exp2      ->
--       blanks >>=       \_         ->
--       sat (== ')') >>= \_         -> 
--       return ((cons opr) exp1 exp2)
--  where
--    cons '+' = Add
--    cons '-' = Sub
--    cons '*' = Mult
--    cons '^' = Pow
--    cons '/' = Div
--    cons '<' = Min
--    cons '>' = Max
--    cons '|' = Concat

-- >>>>>>> 0f24e6861f8c80b8bdf1c96a9991f89d1ce2ebf9

-- | A parser for PICO natural keyword.       
nat :: Parser Type 
nat = string "natural" >>= \_ -> return TNatural 

-- | A parser for PICO string keyword. 
str :: Parser Type
str = string "string" >>= \_ -> return TString 

-- | A parser for natural values.                                 
natural :: Parser Integer
natural = (many1 digit >>= \v -> return (read v :: Integer)) 

-- | A parser for string values.
text :: Parser String
text = sat (== '\"')           >>= \_ ->
       many1 (letter <|> digit) >>= \s ->
       sat (== '\"')           >>= \_ ->
       return s
