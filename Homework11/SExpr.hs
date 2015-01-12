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
   where n = (\_ _ z -> N z) <$> (char 'N') <*> spaces <*> posInt
         i = (\_ _ z -> I z) <$> (char 'I') <*> spaces <*> ident

parseAAtom :: Parser SExpr
parseAAtom = fmap (\x -> A x) parseAtom         

parseComb :: Parser SExpr
parseComb = (\_ x _ -> (: x)) <$> ((zeroOrMore spaces) *> (char '(')) <*> (alt parseAAtom parseComb) <*> ((zeroOrMore spaces) *> (char ')'))

--parseSExpr :: Parser SExpr
--parseSExpr = alt parseAAtom parseComb