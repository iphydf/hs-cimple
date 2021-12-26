{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData         #-}
module Language.Cimple.AST
    ( AssignOp (..)
    , BinaryOp (..)
    , UnaryOp (..)
    , LiteralType (..)
    , Node (..)
    , Scope (..)
    , CommentStyle (..)
    ) where

import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Functor.Identity (Identity)
import           GHC.Generics          (Generic)

data Node f attr lexeme
    = Attr attr (f (Node f attr lexeme))
    -- Preprocessor
    | PreprocInclude lexeme
    | PreprocDefine lexeme
    | PreprocDefineConst lexeme (f (Node f attr lexeme))
    | PreprocDefineMacro lexeme ([f (Node f attr lexeme)]) (f (Node f attr lexeme))
    | PreprocIf (f (Node f attr lexeme)) [f (Node f attr lexeme)] (f (Node f attr lexeme))
    | PreprocIfdef lexeme [f (Node f attr lexeme)] (f (Node f attr lexeme))
    | PreprocIfndef lexeme [f (Node f attr lexeme)] (f (Node f attr lexeme))
    | PreprocElse [f (Node f attr lexeme)]
    | PreprocElif (f (Node f attr lexeme)) [f (Node f attr lexeme)] (f (Node f attr lexeme))
    | PreprocUndef lexeme
    | PreprocDefined lexeme
    | PreprocScopedDefine (f (Node f attr lexeme)) [f (Node f attr lexeme)] (f (Node f attr lexeme))
    | MacroBodyStmt (f (Node f attr lexeme))
    | MacroBodyFunCall (f (Node f attr lexeme))
    | MacroParam lexeme
    | StaticAssert (f (Node f attr lexeme)) lexeme
    -- Comments
    | LicenseDecl lexeme [f (Node f attr lexeme)]
    | CopyrightDecl lexeme (Maybe lexeme) [lexeme]
    | Comment CommentStyle lexeme [lexeme] lexeme
    | CommentBlock lexeme
    | Commented (f (Node f attr lexeme)) (f (Node f attr lexeme))
    -- Namespace-like blocks
    | ExternC [f (Node f attr lexeme)]
    | Class Scope lexeme [f (Node f attr lexeme)] [f (Node f attr lexeme)]
    | Namespace Scope lexeme [f (Node f attr lexeme)]
    -- Statements
    | CompoundStmt [f (Node f attr lexeme)]
    | Break
    | Goto lexeme
    | Continue
    | Return (Maybe (f (Node f attr lexeme)))
    | SwitchStmt (f (Node f attr lexeme)) [f (Node f attr lexeme)]
    | IfStmt (f (Node f attr lexeme)) (f (Node f attr lexeme)) (Maybe (f (Node f attr lexeme)))
    | ForStmt (f (Node f attr lexeme)) (f (Node f attr lexeme)) (f (Node f attr lexeme)) (f (Node f attr lexeme))
    | WhileStmt (f (Node f attr lexeme)) (f (Node f attr lexeme))
    | DoWhileStmt (f (Node f attr lexeme)) (f (Node f attr lexeme))
    | Case (f (Node f attr lexeme)) (f (Node f attr lexeme))
    | Default (f (Node f attr lexeme))
    | Label lexeme (f (Node f attr lexeme))
    -- Variable declarations
    | VLA (f (Node f attr lexeme)) lexeme (f (Node f attr lexeme))
    | VarDecl (f (Node f attr lexeme)) (f (Node f attr lexeme))
    | Declarator (f (Node f attr lexeme)) (Maybe (f (Node f attr lexeme)))
    | DeclSpecVar lexeme
    | DeclSpecArray (f (Node f attr lexeme)) (Maybe (f (Node f attr lexeme)))
    -- Expressions
    | InitialiserList [f (Node f attr lexeme)]
    | UnaryExpr UnaryOp (f (Node f attr lexeme))
    | BinaryExpr (f (Node f attr lexeme)) BinaryOp (f (Node f attr lexeme))
    | TernaryExpr (f (Node f attr lexeme)) (f (Node f attr lexeme)) (f (Node f attr lexeme))
    | AssignExpr (f (Node f attr lexeme)) AssignOp (f (Node f attr lexeme))
    | ParenExpr (f (Node f attr lexeme))
    | CastExpr (f (Node f attr lexeme)) (f (Node f attr lexeme))
    | CompoundExpr (f (Node f attr lexeme)) (f (Node f attr lexeme))
    | SizeofExpr (f (Node f attr lexeme))
    | SizeofType (f (Node f attr lexeme))
    | LiteralExpr LiteralType lexeme
    | VarExpr lexeme
    | MemberAccess (f (Node f attr lexeme)) lexeme
    | PointerAccess (f (Node f attr lexeme)) lexeme
    | ArrayAccess (f (Node f attr lexeme)) (f (Node f attr lexeme))
    | FunctionCall (f (Node f attr lexeme)) [f (Node f attr lexeme)]
    | CommentExpr (f (Node f attr lexeme)) (f (Node f attr lexeme))
    -- Type definitions
    | EnumClass lexeme [f (Node f attr lexeme)]
    | EnumConsts (Maybe lexeme) [f (Node f attr lexeme)]
    | EnumDecl lexeme [f (Node f attr lexeme)] lexeme
    | Enumerator lexeme (Maybe (f (Node f attr lexeme)))
    | ClassForward lexeme [f (Node f attr lexeme)]
    | Typedef (f (Node f attr lexeme)) lexeme
    | TypedefFunction (f (Node f attr lexeme))
    | Struct lexeme [f (Node f attr lexeme)]
    | Union lexeme [f (Node f attr lexeme)]
    | MemberDecl (f (Node f attr lexeme)) (f (Node f attr lexeme)) (Maybe lexeme)
    | TyConst (f (Node f attr lexeme))
    | TyPointer (f (Node f attr lexeme))
    | TyStruct lexeme
    | TyFunc lexeme
    | TyStd lexeme
    | TyVar lexeme
    | TyUserDefined lexeme
    -- Functions
    | FunctionDecl Scope (f (Node f attr lexeme)) (Maybe (f (Node f attr lexeme)))
    | FunctionDefn Scope (f (Node f attr lexeme)) (f (Node f attr lexeme))
    | FunctionPrototype (f (Node f attr lexeme)) lexeme [f (Node f attr lexeme)]
    | FunctionParam (f (Node f attr lexeme)) (f (Node f attr lexeme))
    | Event lexeme (f (Node f attr lexeme))
    | EventParams [f (Node f attr lexeme)]
    | Property (f (Node f attr lexeme)) (f (Node f attr lexeme)) [f (Node f attr lexeme)]
    | Accessor lexeme [f (Node f attr lexeme)] (Maybe (f (Node f attr lexeme)))
    | ErrorDecl lexeme [f (Node f attr lexeme)]
    | ErrorList [f (Node f attr lexeme)]
    | ErrorFor lexeme
    | Ellipsis
    -- Constants
    | ConstDecl (f (Node f attr lexeme)) lexeme
    | ConstDefn Scope (f (Node f attr lexeme)) lexeme (f (Node f attr lexeme))
    deriving (Generic, Functor, Foldable, Traversable)

deriving instance (Show attr, Show lexeme) => Show (Node Identity attr lexeme)
deriving instance (Eq attr, Eq lexeme) => Eq (Node Identity attr lexeme)

instance (FromJSON attr, FromJSON lexeme) => FromJSON (Node Identity attr lexeme)
instance (ToJSON attr, ToJSON lexeme) => ToJSON (Node Identity attr lexeme)

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
    deriving (Show, Eq, Generic)

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
    deriving (Show, Eq, Generic)

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
    deriving (Show, Eq, Generic)

instance FromJSON UnaryOp
instance ToJSON UnaryOp

data LiteralType
    = Char
    | Int
    | Bool
    | String
    | ConstId
    deriving (Show, Eq, Generic)

instance FromJSON LiteralType
instance ToJSON LiteralType

data Scope
    = Global
    | Static
    deriving (Show, Eq, Generic)

instance FromJSON Scope
instance ToJSON Scope

data CommentStyle
    = Regular
    | Doxygen
    | Block
    deriving (Show, Eq, Generic)

instance FromJSON CommentStyle
instance ToJSON CommentStyle
