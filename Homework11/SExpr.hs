-- http://www.cis.upenn.edu/~cis194/spring13/hw/10-applicative.pdf
{-# OPTIONS_GHC -Wall #-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = alt ( (:) <$> p <*> zeroOrMore p) (pure [])

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> (zeroOrMore (satisfy isAlphaNum))

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseAtom :: Parser Atom
parseAtom = alt n i
   where n = (\_ z -> N z) <$> spaces <*> posInt
         i = (\ _ z -> I z) <$> spaces <*> ident

parseAAtom :: Parser SExpr
parseAAtom = fmap (\x -> A x) parseAtom         

-- (bar (foo) 3 5 874)
-- see http://stackoverflow.com/questions/27894888/parsing-s-expression for its Haskell representation

parseInnerParens :: Parser SExpr
parseInnerParens = ( spaces *> (char '(') *> spaces *> (alt one two) <* spaces <* (char ')') <* spaces )
  where 
  	one = f <$> (oneOrMore parseAAtom) <*> (zeroOrMore parseInnerParens) <*> (zeroOrMore parseAAtom)
  	two = f <$> (zeroOrMore parseAAtom) <*> (zeroOrMore parseInnerParens) <*> (oneOrMore parseAAtom)
  	f   = \a1 cs a2 -> Comb $ a1 ++ cs ++ a2
