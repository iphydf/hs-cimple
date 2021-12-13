{-# LANGUAGE StrictData #-}
module Language.Cimple.TranslationUnit
  ( TranslationUnit
  ) where

import           Language.Cimple.AST   (Node)
import           Language.Cimple.Lexer (Lexeme)

type TranslationUnit a = (FilePath, [Node (Lexeme a)])
