{-# LANGUAGE StrictData #-}
module Language.Cimple.TranslationUnit
  ( TranslationUnit
  ) where

import           Data.Functor.Identity (Identity)
import           Language.Cimple.AST   (Node)
import           Language.Cimple.Lexer (Lexeme)

type TranslationUnit text = (FilePath, [Node Identity () (Lexeme text)])
