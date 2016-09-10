{-|
Module      : Pico.YAHCL
Description : Another Haskell combinator library for parsing
Copyright   : (c) Rodrigo Bonifaico, 2016
License     : -
Maintainer  : rbonifacio@unb.br
Stability   : experimental
Portability : POSIX

A simple combinator library for parsing. It is based
on existing works, such as:

   * Thinking Functionally with Haskell (Richard Bird)
   * Monadic Parsing in Haskell (Graham Hutton / Erik Meijer)
   * Monads for functional programming (Philip Wadler)

This parser, although quite easy to use, is still not suitable
for production usage. Particularly because it does not deal
with errors in a user friendly way. Nevertheless, it is easy
to learn and use in academic settings. One of the reasons
for implementing this parser is that the PARSEC implementation
has changed substantially in the last years. In this way,
it became hard to use. 
-}
module Pico.YAHCL where

import Prelude hiding ((>>=), return, fail)
import Data.Char


-- | The @Parser@ data type.

-- A parser is any
-- function that receives a String and returns
-- either an empty list (when the parser fails)
-- or a list with a single element. This element is a
-- tuple of type (String, a): the first is the string
-- that has not been consumed by the parser and the
-- second is the value resulting from the parser. 
type Parser a = String -> [(String, a)]

-- | The parser that allways fails. 
fail :: Parser Char
fail = \s -> []

-- | The parser that consumes the first
-- character of the input String. It fails
-- only when de input string is empty. 
char :: Parser Char
char []     = []
char (c:cs) = [(cs, c)]

-- | This is the satisfy parser. It receives a predicate (a function of
-- type Char -> Bool) and succeeds only when the application of the predicate
-- to the first character of the input string leads to a non-empty value. 
sat :: (Char -> Bool) -> Parser Char
sat p = \s -> case s of
               (c:cs) -> if p c then char s else []
               otherwise -> []

symbol :: Char -> Parser Char
symbol c = sat (== c)

-- | Parser for a letter.
-- This parser only succeeds if the input string
-- starts with a letter, such as "haskell 193";
-- but fails when the input string starts with a
-- special symbol or a number. 
letter :: Parser Char
letter = sat isLetter

-- | Parser for a digit.
-- This parser only succeeds if the input strign
-- starts with a digit, such as "193 haskell";
-- but fails when the input string starts with
-- a letter of a special symbol. 
digit :: Parser Char
digit = sat isDigit 


-- | A combinator for alternative parsers. If the first parser fails,
-- than it tries the second one.

(<|>) :: Parser a -> Parser a -> Parser a
m <|> k = \s ->
 let res = m s
 in  case res of
      [] -> k s
      otherwsie -> res 

-- | A combinator for representing sequencing.
-- Note that this combinator corresponds to the
-- bind component of a monad. 
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
m >>= k = \s ->
  let res = m s
  in case res of
     [] -> []
     [(sub, a)] -> k a sub

-- | A parser that allways suceeds. 
return :: a -> Parser a
return a = \s -> [(s, a)] 

-- | Applies a given parser many times,
-- until it fails. 
many :: Parser a -> Parser [a]
many p = \s -> case p s of
                [] -> [(s, [])]
                [(s1, a)] -> [(s2, (a:as)) | (s2,as) <- many p s1]

many1 :: Parser a -> Parser [a]
many1 p = \s -> case p s of
                 [] -> []
                 [(s1, a)] -> [(s2, (a:as)) | (s2, as) <- many p s1]

-- | Applies a given parser many times,
-- whenever it finds a separator (the first argument)

separator :: Char -> Parser a -> Parser [a]
separator d p = (blanks >>= \_ ->
                 p >>= \a -> ((blanks >>= \_ ->
                 sat (==d) >>= \_ ->
                 separator d p >>= \as ->
                 return (a:as)) <|> return [a])) 
                <|> return [] 

-- | A parser for a single digit.
number = digit >>= \c -> return $ (ord c) - ord('0')  

-- | A parser that recognizes a specific string
string :: String -> Parser String
string [] = \s -> [(s, [])]
string (x:xs) = \(a:as) ->
  if(x == a)
   then let res = string xs as
        in case res of
         [] -> [((a:as), [])]
         [(rem, str)] -> [(rem, (a:str))]
   else []
                  
-- | A parser that succeeds when it finds a space or a new line
blank :: Parser Char 
blank = space <|> endOfLine <|> tab

-- | A parser that succeeds when it finds a space 
space :: Parser Char 
space = sat (== ' ')

-- | A parser that suceeds when it finds a new line 
endOfLine :: Parser Char 
endOfLine = sat (== '\n')

-- | A parser that succeeds when it finds a tab
tab :: Parser Char
tab = sat (== '\t')

-- | A parser that ignores many blanks
blanks = many blank >>= \_ -> return ()  

-- | A parser for a single token. 
token :: String -> Parser String
token s = blanks >>= \_ -> string s >>= \_ -> blanks >>= \_ -> return s
