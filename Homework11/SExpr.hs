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

parseComb :: Parser SExpr
parseComb = (\_ _ _ x _ _ _ -> x) <$> spaces <*> (char '(') <*> spaces <*> (alt parseAAtom parseComb) <*> spaces <*> (char ')') <*> spaces

parseCombElements :: Parser [SExpr]
parseCombElements = oneOrMore parseComb

parseSExpr :: Parser SExpr
parseSExpr = alt parseAAtom (fmap (\x -> Comb x) parseCombElements)
