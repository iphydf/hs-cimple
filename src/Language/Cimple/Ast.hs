{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData         #-}
module Language.Cimple.Ast
    ( AssignOp (..)
    , BinaryOp (..)
    , UnaryOp (..)
    , LiteralType (..)
    , Node, NodeF (..)
    , Scope (..)
    , CommentStyle (..)
    ) where

import           Data.Aeson                   (FromJSON, FromJSON1, ToJSON,
                                               ToJSON1)
import           Data.Fix                     (Fix)
import           Data.Functor.Classes         (Eq1, Read1, Show1)
import           Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import           GHC.Generics                 (Generic, Generic1)

data NodeF lexeme a
    -- Preprocessor
    = PreprocInclude lexeme
    | PreprocDefine lexeme
    | PreprocDefineConst lexeme a
    | PreprocDefineMacro lexeme [a] a
    | PreprocIf a [a] a
    | PreprocIfdef lexeme [a] a
    | PreprocIfndef lexeme [a] a
    | PreprocElse [a]
    | PreprocElif a [a] a
    | PreprocUndef lexeme
    | PreprocDefined lexeme
    | PreprocScopedDefine a [a] a
    | MacroBodyStmt a
    | MacroBodyFunCall a
    | MacroParam lexeme
    | StaticAssert a lexeme
    -- Comments
    | LicenseDecl lexeme [a]
    | CopyrightDecl lexeme (Maybe lexeme) [lexeme]
    | Comment CommentStyle lexeme [lexeme] lexeme
    | CommentBlock lexeme
    | Commented a a
    -- Namespace-like blocks
    | ExternC [a]
    -- Statements
    | CompoundStmt [a]
    | Break
    | Goto lexeme
    | Continue
    | Return (Maybe a)
    | SwitchStmt a [a]
    | IfStmt a a (Maybe a)
    | ForStmt a a a a
    | WhileStmt a a
    | DoWhileStmt a a
    | Case a a
    | Default a
    | Label lexeme a
    -- Variable declarations
    | VLA a lexeme a
    | VarDecl a a
    | Declarator a (Maybe a)
    | DeclSpecVar lexeme
    | DeclSpecArray a (Maybe a)
    -- Expressions
    | InitialiserList [a]
    | UnaryExpr UnaryOp a
    | BinaryExpr a BinaryOp a
    | TernaryExpr a a a
    | AssignExpr a AssignOp a
    | ParenExpr a
    | CastExpr a a
    | CompoundExpr a a
    | SizeofExpr a
    | SizeofType a
    | LiteralExpr LiteralType lexeme
    | VarExpr lexeme
    | MemberAccess a lexeme
    | PointerAccess a lexeme
    | ArrayAccess a a
    | FunctionCall a [a]
    | CommentExpr a a
    -- Type definitions
    | EnumConsts (Maybe lexeme) [a]
    | EnumDecl lexeme [a] lexeme
    | Enumerator lexeme (Maybe a)
    | Typedef a lexeme
    | TypedefFunction a
    | Struct lexeme [a]
    | Union lexeme [a]
    | MemberDecl a a (Maybe lexeme)
    | TyConst a
    | TyPointer a
    | TyStruct lexeme
    | TyFunc lexeme
    | TyStd lexeme
    | TyUserDefined lexeme
    -- Functions
    | FunctionDecl Scope a
    | FunctionDefn Scope a a
    | FunctionPrototype a lexeme [a]
    | FunctionParam a a
    | Ellipsis
    -- Constants
    | ConstDecl a lexeme
    | ConstDefn Scope a lexeme a
    deriving (Show, Read, Eq, Generic, Generic1, Functor, Foldable, Traversable)
    deriving (Show1, Read1, Eq1) via FunctorClassesDefault (NodeF lexeme)

type Node lexeme = Fix (NodeF lexeme)

instance FromJSON lexeme => FromJSON1 (NodeF lexeme)
instance ToJSON lexeme => ToJSON1 (NodeF lexeme)

data AssignOp
    = AopEq
    | AopMul
    | AopDiv
    | AopPlus
    | AopMinus
    | AopBitAnd
    | AopBitOr
    | AopBitXor
    | AopMod
    | AopLsh
    | AopRsh
    deriving (Show, Read, Eq, Generic)

instance FromJSON AssignOp
instance ToJSON AssignOp

data BinaryOp
    = BopNe
    | BopEq
    | BopOr
    | BopBitXor
    | BopBitOr
    | BopAnd
    | BopBitAnd
    | BopDiv
    | BopMul
    | BopMod
    | BopPlus
    | BopMinus
    | BopLt
    | BopLe
    | BopLsh
    | BopGt
    | BopGe
    | BopRsh
    deriving (Show, Read, Eq, Generic)

instance FromJSON BinaryOp
instance ToJSON BinaryOp

data UnaryOp
    = UopNot
    | UopNeg
    | UopMinus
    | UopAddress
    | UopDeref
    | UopIncr
    | UopDecr
    deriving (Show, Read, Eq, Generic)

instance FromJSON UnaryOp
instance ToJSON UnaryOp

data LiteralType
    = Char
    | Int
    | Bool
    | String
    | ConstId
    deriving (Show, Read, Eq, Generic)

instance FromJSON LiteralType
instance ToJSON LiteralType

data Scope
    = Global
    | Static
    deriving (Show, Read, Eq, Generic)

instance FromJSON Scope
instance ToJSON Scope

data CommentStyle
    = Regular
    | Doxygen
    | Block
    deriving (Show, Read, Eq, Generic)

instance FromJSON CommentStyle
instance ToJSON CommentStyle