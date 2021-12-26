{
module Language.Cimple.Parser
    ( parseTranslationUnit
    ) where

import           Data.Functor.Identity  (Identity (..))
import           Language.Cimple.AST    (AssignOp (..), BinaryOp (..),
                                         CommentStyle (..), LiteralType (..),
                                         Node (..), Scope (..), UnaryOp (..))
import           Language.Cimple.Lexer  (Alex, AlexPosn (..), Lexeme (..),
                                         alexError, alexMonadScan)
import           Language.Cimple.Tokens (LexemeClass (..))
}

-- Conflict between (static) FunctionDecl and (static) ConstDecl.
%expect 2

%name parseTranslationUnit TranslationUnit

%error {parseError}
%errorhandlertype explist
%lexer {lexwrap} {L _ Eof _}
%monad {Alex}
%tokentype {Lexeme String}
%token
    ID_CONST			{ L _ IdConst			_ }
    ID_FUNC_TYPE		{ L _ IdFuncType		_ }
    ID_STD_TYPE			{ L _ IdStdType			_ }
    ID_SUE_TYPE			{ L _ IdSueType			_ }
    ID_TYVAR			{ L _ IdTyVar			_ }
    ID_VAR			{ L _ IdVar			_ }
    bitmask			{ L _ KwBitmask			_ }
    break			{ L _ KwBreak			_ }
    case			{ L _ KwCase			_ }
    class			{ L _ KwClass			_ }
    const			{ L _ KwConst			_ }
    continue			{ L _ KwContinue		_ }
    default			{ L _ KwDefault			_ }
    do				{ L _ KwDo			_ }
    else			{ L _ KwElse			_ }
    enum			{ L _ KwEnum			_ }
    'error'			{ L _ KwError			_ }
    event			{ L _ KwEvent			_ }
    extern			{ L _ KwExtern			_ }
    for				{ L _ KwFor			_ }
    goto			{ L _ KwGoto			_ }
    if				{ L _ KwIf			_ }
    namespace			{ L _ KwNamespace		_ }
    return			{ L _ KwReturn			_ }
    sizeof			{ L _ KwSizeof			_ }
    static			{ L _ KwStatic			_ }
    static_assert		{ L _ KwStaticAssert		_ }
    struct			{ L _ KwStruct			_ }
    switch			{ L _ KwSwitch			_ }
    this			{ L _ KwThis			_ }
    typedef			{ L _ KwTypedef			_ }
    union			{ L _ KwUnion			_ }
    VLA				{ L _ KwVla			_ }
    void			{ L _ KwVoid			_ }
    while			{ L _ KwWhile			_ }
    with			{ L _ KwWith			_ }
    LIT_CHAR			{ L _ LitChar			_ }
    LIT_FALSE			{ L _ LitFalse			_ }
    LIT_TRUE			{ L _ LitTrue			_ }
    LIT_INTEGER			{ L _ LitInteger		_ }
    LIT_STRING			{ L _ LitString			_ }
    LIT_SYS_INCLUDE		{ L _ LitSysInclude		_ }
    '&'				{ L _ PctAmpersand		_ }
    '&&'			{ L _ PctAmpersandAmpersand	_ }
    '&='			{ L _ PctAmpersandEq		_ }
    '->'			{ L _ PctArrow			_ }
    '*'				{ L _ PctAsterisk		_ }
    '*='			{ L _ PctAsteriskEq		_ }
    '^'				{ L _ PctCaret			_ }
    '^='			{ L _ PctCaretEq		_ }
    ':'				{ L _ PctColon			_ }
    ','				{ L _ PctComma			_ }
    '!'				{ L _ PctEMark			_ }
    '!='			{ L _ PctEMarkEq		_ }
    '='				{ L _ PctEq			_ }
    '=='			{ L _ PctEqEq			_ }
    '>'				{ L _ PctGreater		_ }
    '>='			{ L _ PctGreaterEq		_ }
    '>>'			{ L _ PctGreaterGreater		_ }
    '>>='			{ L _ PctGreaterGreaterEq	_ }
    '{'				{ L _ PctLBrace			_ }
    '['				{ L _ PctLBrack			_ }
    '<'				{ L _ PctLess			_ }
    '<='			{ L _ PctLessEq			_ }
    '<<'			{ L _ PctLessLess		_ }
    '<<='			{ L _ PctLessLessEq		_ }
    '('				{ L _ PctLParen			_ }
    '-'				{ L _ PctMinus			_ }
    '-='			{ L _ PctMinusEq		_ }
    '--'			{ L _ PctMinusMinus		_ }
    '%'				{ L _ PctPercent		_ }
    '%='			{ L _ PctPercentEq		_ }
    '.'				{ L _ PctPeriod			_ }
    '...'			{ L _ PctEllipsis		_ }
    '|'				{ L _ PctPipe			_ }
    '|='			{ L _ PctPipeEq			_ }
    '||'			{ L _ PctPipePipe		_ }
    '+'				{ L _ PctPlus			_ }
    '+='			{ L _ PctPlusEq			_ }
    '++'			{ L _ PctPlusPlus		_ }
    '?'				{ L _ PctQMark			_ }
    '}'				{ L _ PctRBrace			_ }
    ']'				{ L _ PctRBrack			_ }
    ')'				{ L _ PctRParen			_ }
    ';'				{ L _ PctSemicolon		_ }
    '/'				{ L _ PctSlash			_ }
    '/='			{ L _ PctSlashEq		_ }
    '~'				{ L _ PctTilde			_ }
    'defined'			{ L _ PpDefined			_ }
    '#define'			{ L _ PpDefine			_ }
    '#elif'			{ L _ PpElif			_ }
    '#else'			{ L _ PpElse			_ }
    '#endif'			{ L _ PpEndif			_ }
    '#if'			{ L _ PpIf			_ }
    '#ifdef'			{ L _ PpIfdef			_ }
    '#ifndef'			{ L _ PpIfndef			_ }
    '#include'			{ L _ PpInclude			_ }
    '#undef'			{ L _ PpUndef			_ }
    '\n'			{ L _ PpNewline			_ }
    '/**/'			{ L _ CmtBlock			_ }
    '/*'			{ L _ CmtStart			_ }
    '/**'			{ L _ CmtStartDoc		_ }
    '/***'			{ L _ CmtStartBlock		_ }
    ' * '			{ L _ CmtIndent			_ }
    '*/'			{ L _ CmtEnd			_ }
    'Copyright'			{ L _ CmtSpdxCopyright		_ }
    'License'			{ L _ CmtSpdxLicense		_ }
    CMT_CODE			{ L _ CmtCode			_ }
    CMT_WORD			{ L _ CmtWord			_ }
    CMT_REF			{ L _ CmtRef			_ }

%left ','
%right '=' '+=' '-=' '*=' '/=' '%=' '<<=' '>>=' '&=' '^=' '|='
%right '?' ':'
%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%left '!=' '=='
%left '<' '<=' '>' '>='
%left '<<' '>>'
%left '+' '-'
%left '*' '/' '%'
%right CAST ADDRESS NEG DEREF sizeof '!' '~' '++' '--'
%left '->' '.' '(' '['

%%

TranslationUnit :: { [StringNode] }
TranslationUnit
:	ToplevelDecls						{ reverse $1 }
|	LicenseDecl ToplevelDecls				{ $1 : reverse $2 }

LicenseDecl :: { StringNode }
LicenseDecl
:	'/*' 'License' CMT_WORD '\n' CopyrightDecls '*/'	{ mk $ LicenseDecl $3 $5 }

CopyrightDecls :: { [StringNode] }
CopyrightDecls
:	CopyrightDecl						{ [$1] }
|	CopyrightDecls CopyrightDecl				{ $2 : $1 }

CopyrightDecl :: { StringNode }
CopyrightDecl
:	' * ' 'Copyright' CopyrightDates CopyrightOwner '\n'	{ mk $ CopyrightDecl (fst $3) (snd $3) $4 }

CopyrightDates :: { (StringLexeme, Maybe StringLexeme) }
CopyrightDates
:	LIT_INTEGER						{ ($1, Nothing) }
|	LIT_INTEGER '-' LIT_INTEGER				{ ($1, Just $3) }

CopyrightOwner :: { [StringLexeme] }
CopyrightOwner
:	CMT_WORD CommentWords					{ $1 : reverse $2 }

ToplevelDecls :: { [StringNode] }
ToplevelDecls
:	ToplevelDecl						{ [$1] }
|	ToplevelDecls ToplevelDecl				{ $2 : $1 }

ToplevelDecl :: { StringNode }
ToplevelDecl
:	AggregateDecl						{ $1 }
|	Comment							{ $1 }
|	ConstDecl						{ $1 }
|	EnumDecl						{ $1 }
|	ErrorDecl						{ $1 }
|	Event							{ $1 }
|	ExternC							{ $1 }
|	FunctionDecl						{ $1 }
|	Namespace						{ $1 }
|	PreprocDefine						{ $1 }
|	PreprocIfdef(ToplevelDecls)				{ $1 }
|	PreprocIf(ToplevelDecls)				{ $1 }
|	PreprocInclude						{ $1 }
|	PreprocUndef						{ $1 }
|	StaticAssert						{ $1 }
|	TypedefDecl						{ $1 }

StaticAssert :: { StringNode }
StaticAssert
:	static_assert '(' ConstExpr ',' LIT_STRING ')' ';'	{ mk $ StaticAssert $3 $5 }

Namespace :: { StringNode }
Namespace
:	NamespaceDeclarator					{ $1 Global }
|	static NamespaceDeclarator				{ $2 Static }

NamespaceDeclarator :: { Scope -> StringNode }
NamespaceDeclarator
:	class ID_SUE_TYPE TypeParams '{' ToplevelDecls '}'	{ \s -> mk $ Class s $2 $3 (reverse $5) }
|	namespace IdVar '{' ToplevelDecls '}'			{ \s -> mk $ Namespace s $2 (reverse $4) }

TypeParams :: { [StringNode] }
TypeParams
:								{ [] }
|	'<' ID_TYVAR '>'					{ [mk $ TyVar $2] }

Event :: { StringNode }
Event
:	event IdVar       '{' Comment EventType '}'		{ mk $ Event $2 (mk $ Commented $4 $5) }
|	event IdVar const '{' Comment EventType '}'		{ mk $ Event $2 (mk $ Commented $5 $6) }

EventType :: { StringNode }
EventType
:	typedef void EventParams ';'				{ $3 }

EventParams :: { StringNode }
EventParams
:	FunctionParamList					{ mk $ EventParams $1 }

ErrorDecl :: { StringNode }
ErrorDecl
:	'error' for IdVar EnumeratorList			{ mk $ ErrorDecl $3 $4 }

Comment :: { StringNode }
Comment
:	'/*' CommentTokens '*/'					{ mk $ Comment Regular $1 (reverse $2) $3 }
|	'/**' CommentTokens '*/'				{ mk $ Comment Doxygen $1 (reverse $2) $3 }
|	'/***' CommentTokens '*/'				{ mk $ Comment Block $1 (reverse $2) $3 }
|	'/**/'							{ mk $ CommentBlock $1 }

CommentTokens :: { [StringLexeme] }
CommentTokens
:	CommentToken						{ [$1] }
|	CommentTokens CommentToken				{ $2 : $1 }

CommentToken :: { StringLexeme }
CommentToken
:	CommentWord						{ $1 }
|	'\n'							{ $1 }
|	' * '							{ $1 }

CommentWords :: { [StringLexeme] }
CommentWords
:								{ [] }
|	CommentWords CommentWord				{ $2 : $1 }

CommentWord :: { StringLexeme }
CommentWord
:	CMT_WORD						{ $1 }
|	CMT_REF							{ $1 }
|	CMT_CODE						{ $1 }
|	LIT_INTEGER						{ $1 }
|	LIT_STRING						{ $1 }
|	'.'							{ $1 }
|	'?'							{ $1 }
|	'!'							{ $1 }
|	','							{ $1 }
|	';'							{ $1 }
|	':'							{ $1 }
|	'('							{ $1 }
|	')'							{ $1 }
|	'<'							{ $1 }
|	'>'							{ $1 }
|	'/'							{ $1 }
|	'+'							{ $1 }
|	'-'							{ $1 }
|	'='							{ $1 }

PreprocIfdef(decls)
:	'#ifdef' ID_CONST decls PreprocElse(decls) '#endif'	{ mk $ PreprocIfdef $2 (reverse $3) $4 }
|	'#ifndef' ID_CONST decls PreprocElse(decls) '#endif'	{ mk $ PreprocIfndef $2 (reverse $3) $4 }

PreprocIf(decls)
:	'#if' PreprocConstExpr '\n' decls PreprocElse(decls) '#endif'	{ mk $ PreprocIf $2 (reverse $4) $5 }

PreprocElse(decls)
:								{ mk $ PreprocElse [] }
|	'#else' decls						{ mk $ PreprocElse $2 }
|	'#elif' PreprocConstExpr '\n' decls PreprocElse(decls)	{ mk $ PreprocElif $2 (reverse $4) $5 }

PreprocInclude :: { StringNode }
PreprocInclude
:	'#include' LIT_STRING					{ mk $ PreprocInclude $2 }
|	'#include' LIT_SYS_INCLUDE				{ mk $ PreprocInclude $2 }

PreprocDefine :: { StringNode }
PreprocDefine
:	'#define' ID_CONST '\n'					{ mk $ PreprocDefine $2 }
|	'#define' ID_CONST PreprocSafeExpr(ConstExpr) '\n'	{ mk $ PreprocDefineConst $2 $3 }
|	'#define' ID_CONST MacroParamList MacroBody '\n'	{ mk $ PreprocDefineMacro $2 $3 $4 }

PreprocUndef :: { StringNode }
PreprocUndef
:	'#undef' ID_CONST					{ mk $ PreprocUndef $2 }

PreprocConstExpr :: { StringNode }
PreprocConstExpr
:	PureExpr(PreprocConstExpr)				{ $1 }
|	'defined' '(' ID_CONST ')'				{ mk $ PreprocDefined $3 }

MacroParamList :: { [StringNode] }
MacroParamList
:	'(' ')'							{ [] }
|	'(' MacroParams ')'					{ reverse $2 }
|	'(' MacroParams ',' '...' ')'				{ reverse $ mk Ellipsis : $2 }

MacroParams :: { [StringNode] }
MacroParams
:	MacroParam						{ [$1] }
|	MacroParams ',' MacroParam				{ $3 : $1 }

MacroParam :: { StringNode }
MacroParam
:	IdVar							{ mk $ MacroParam $1 }

MacroBody :: { StringNode }
MacroBody
:	do CompoundStmt while '(' LIT_INTEGER ')'		{% macroBodyStmt $2 $5 }
|	FunctionCall						{ mk $ MacroBodyFunCall $1 }

ExternC :: { StringNode }
ExternC
:	'#ifdef' ID_CONST
	extern LIT_STRING '{'
	'#endif'
	ToplevelDecls
	'#ifdef' ID_CONST
	'}'
	'#endif'						{% externC $2 $4 (reverse $7) $9 }

Stmts :: { [StringNode] }
Stmts
:	Stmt							{ [$1] }
|	Stmts Stmt						{ $2 : $1 }

Stmt :: { StringNode }
Stmt
:	PreprocIfdef(Stmts)					{ $1 }
|	PreprocIf(Stmts)					{ $1 }
|	PreprocDefine Stmts PreprocUndef			{ mk $ PreprocScopedDefine $1 $2 $3 }
|	DeclStmt						{ $1 }
|	CompoundStmt						{ $1 }
|	IfStmt							{ $1 }
|	ForStmt							{ $1 }
|	WhileStmt						{ $1 }
|	DoWhileStmt						{ $1 }
|	AssignExpr ';'						{ $1 }
|	ExprStmt ';'						{ $1 }
|	FunctionCall ';'					{ $1 }
|	break ';'						{ mk $ Break }
|	goto ID_CONST ';'					{ mk $ Goto $2 }
|	ID_CONST ':' Stmt					{ mk $ Label $1 $3 }
|	continue ';'						{ mk $ Continue }
|	return ';'						{ mk $ Return Nothing }
|	return Expr ';'						{ mk $ Return (Just $2) }
|	switch '(' Expr ')' '{' SwitchCases '}'			{ mk $ SwitchStmt $3 $6 }
|	Comment							{ $1 }

IfStmt :: { StringNode }
IfStmt
:	if '(' Expr ')' CompoundStmt				{ mk $ IfStmt $3 $5 Nothing }
|	if '(' Expr ')' CompoundStmt else IfStmt		{ mk $ IfStmt $3 $5 (Just $7) }
|	if '(' Expr ')' CompoundStmt else CompoundStmt		{ mk $ IfStmt $3 $5 (Just $7) }

ForStmt :: { StringNode }
ForStmt
:	for '(' ForInit Expr ';' ForNext ')' CompoundStmt	{ mk $ ForStmt $3 $4 $6 $8 }

ForInit :: { StringNode }
ForInit
:	AssignExpr ';'						{ $1 }
|	VarDecl							{ $1 }

ForNext :: { StringNode }
ForNext
:	ExprStmt						{ $1 }
|	AssignExpr						{ $1 }

WhileStmt :: { StringNode }
WhileStmt
:	while '(' Expr ')' CompoundStmt				{ mk $ WhileStmt $3 $5 }

DoWhileStmt :: { StringNode }
DoWhileStmt
:	do CompoundStmt while '(' Expr ')' ';'			{ mk $ DoWhileStmt $2 $5 }

SwitchCases :: { [StringNode] }
SwitchCases
:	SwitchCase						{ [$1] }
|	SwitchCases SwitchCase					{ $2 : $1 }

SwitchCase :: { StringNode }
SwitchCase
:	case Expr ':' SwitchCaseBody				{ mk $ Case $2 $4 }
|	default ':' SwitchCaseBody				{ mk $ Default $3 }

SwitchCaseBody :: { StringNode }
SwitchCaseBody
:	CompoundStmt						{ $1 }
|	SwitchCase						{ $1 }
|	return Expr ';'						{ mk $ Return (Just $2) }

DeclStmt :: { StringNode }
DeclStmt
:	VarDecl							{ $1 }
|	VLA '(' QualType ',' IdVar ',' Expr ')' ';'		{ mk $ VLA $3 $5 $7 }

VarDecl :: { StringNode }
VarDecl
:	QualType Declarator ';'					{ mk $ VarDecl $1 $2 }

Declarator :: { StringNode }
Declarator
:	DeclSpec '=' InitialiserExpr				{ mk $ Declarator $1 (Just $3) }
|	DeclSpec						{ mk $ Declarator $1 Nothing }

InitialiserExpr :: { StringNode }
InitialiserExpr
:	InitialiserList						{ mk $ InitialiserList $1 }
|	Expr							{ $1 }

DeclSpec :: { StringNode }
DeclSpec
:	IdVar							{ mk $ DeclSpecVar $1 }
|	DeclSpec '[' ']'					{ mk $ DeclSpecArray $1 Nothing }
|	DeclSpec '[' Expr ']'					{ mk $ DeclSpecArray $1 (Just $3) }

IdVar :: { Lexeme String }
IdVar
:	ID_VAR							{ $1 }
|	default							{ $1 }
|	'error'							{ $1 }

InitialiserList :: { [StringNode] }
InitialiserList
:	'{' Initialisers '}'					{ reverse $2 }
|	'{' Initialisers ',' '}'				{ reverse $2 }

Initialisers :: { [StringNode] }
Initialisers
:	Initialiser						{ [$1] }
|	Initialisers ',' Initialiser				{ $3 : $1 }

Initialiser :: { StringNode }
Initialiser
:	Expr							{ $1 }
|	InitialiserList						{ mk $ InitialiserList $1 }

CompoundStmt :: { StringNode }
CompoundStmt
:	'{' Stmts '}'						{ mk $ CompoundStmt (reverse $2) }

-- Expressions that are safe for use as macro body without () around it..
PreprocSafeExpr(x)
:	LiteralExpr						{ $1 }
|	'(' x ')'						{ mk $ ParenExpr $2 }
|	'(' QualType ')' x %prec CAST				{ mk $ CastExpr $2 $4 }
|	sizeof '(' x ')'					{ mk $ SizeofExpr $3 }
|	sizeof '(' QualType ')'					{ mk $ SizeofType $3 }

ConstExpr :: { StringNode }
ConstExpr
:	PureExpr(ConstExpr)					{ $1 }

PureExpr(x)
:	PreprocSafeExpr(x)					{ $1 }
|	x '!=' x						{ mk $ BinaryExpr $1 BopNe $3 }
|	x '==' x						{ mk $ BinaryExpr $1 BopEq $3 }
|	x '||' x						{ mk $ BinaryExpr $1 BopOr $3 }
|	x '^' x							{ mk $ BinaryExpr $1 BopBitXor $3 }
|	x '|' x							{ mk $ BinaryExpr $1 BopBitOr $3 }
|	x '&&' x						{ mk $ BinaryExpr $1 BopAnd $3 }
|	x '&' x							{ mk $ BinaryExpr $1 BopBitAnd $3 }
|	x '/' x							{ mk $ BinaryExpr $1 BopDiv $3 }
|	x '*' x							{ mk $ BinaryExpr $1 BopMul $3 }
|	x '%' x							{ mk $ BinaryExpr $1 BopMod $3 }
|	x '+' x							{ mk $ BinaryExpr $1 BopPlus $3 }
|	x '-' x							{ mk $ BinaryExpr $1 BopMinus $3 }
|	x '<' x							{ mk $ BinaryExpr $1 BopLt $3 }
|	x '<=' x						{ mk $ BinaryExpr $1 BopLe $3 }
|	x '<<' x						{ mk $ BinaryExpr $1 BopLsh $3 }
|	x '>' x							{ mk $ BinaryExpr $1 BopGt $3 }
|	x '>=' x						{ mk $ BinaryExpr $1 BopGe $3 }
|	x '>>' x						{ mk $ BinaryExpr $1 BopRsh $3 }
|	x '?' x ':' x						{ mk $ TernaryExpr $1 $3 $5 }
|	'!' x							{ mk $ UnaryExpr UopNot $2 }
|	'~' x							{ mk $ UnaryExpr UopNeg $2 }
|	'-' x %prec NEG						{ mk $ UnaryExpr UopMinus $2 }
|	'&' x %prec ADDRESS					{ mk $ UnaryExpr UopAddress $2 }

LiteralExpr :: { StringNode }
LiteralExpr
:	StringLiteralExpr					{ $1 }
|	LIT_CHAR						{ mk $ LiteralExpr Char $1 }
|	LIT_INTEGER						{ mk $ LiteralExpr Int $1 }
|	LIT_FALSE						{ mk $ LiteralExpr Bool $1 }
|	LIT_TRUE						{ mk $ LiteralExpr Bool $1 }
|	ID_CONST						{ mk $ LiteralExpr ConstId $1 }

StringLiteralExpr :: { StringNode }
StringLiteralExpr
:	LIT_STRING						{ mk $ LiteralExpr String $1 }
|	StringLiteralExpr LIT_STRING				{ $1 }

LhsExpr :: { StringNode }
LhsExpr
:	IdVar							{ mk $ VarExpr $1 }
|	'*' LhsExpr %prec DEREF					{ mk $ UnaryExpr UopDeref $2 }
|	LhsExpr '.' IdVar					{ mk $ MemberAccess $1 $3 }
|	LhsExpr '->' IdVar					{ mk $ PointerAccess $1 $3 }
|	LhsExpr '[' Expr ']'					{ mk $ ArrayAccess $1 $3 }

Expr :: { StringNode }
Expr
:	LhsExpr							{ $1 }
|	ExprStmt						{ $1 }
|	FunctionCall						{ $1 }
|	CompoundExpr						{ $1 }
|	PureExpr(Expr)						{ $1 }

-- Allow `(Type){0}` to set struct values to all-zero.
CompoundExpr :: { StringNode }
CompoundExpr
:	'(' QualType ')' '{' Expr '}'				{ mk $ CompoundExpr $2 $5 }

AssignExpr :: { StringNode }
AssignExpr
:	LhsExpr AssignOperator Expr				{ mk $ AssignExpr $1 $2 $3 }

AssignOperator :: { AssignOp }
AssignOperator
:	'='							{ AopEq      }
|	'*='							{ AopMul     }
|	'/='							{ AopDiv     }
|	'+='							{ AopPlus    }
|	'-='							{ AopMinus   }
|	'&='							{ AopBitAnd  }
|	'|='							{ AopBitOr   }
|	'^='							{ AopBitXor  }
|	'%='							{ AopMod     }
|	'<<='							{ AopLsh     }
|	'>>='							{ AopRsh     }

ExprStmt :: { StringNode }
ExprStmt
:	'++' Expr						{ mk $ UnaryExpr UopIncr $2 }
|	'--' Expr						{ mk $ UnaryExpr UopDecr $2 }

FunctionCall :: { StringNode }
FunctionCall
:	Expr ArgList						{ mk $ FunctionCall $1 $2 }

ArgList :: { [StringNode] }
ArgList
:	'(' ')'							{ [] }
|	'(' Args ')'						{ reverse $2 }

Args :: { [StringNode] }
Args
:	Arg							{ [$1] }
|	Args ',' Arg						{ $3 : $1 }

Arg :: { StringNode }
Arg
:	Expr							{ $1 }
|	Comment Expr						{ mk $ CommentExpr $1 $2 }

EnumDecl :: { StringNode }
EnumDecl
:	enum class ID_SUE_TYPE EnumeratorList			{ mk $ EnumClass $3 $4 }
|	enum       ID_SUE_TYPE EnumeratorList ';'		{ mk $ EnumConsts (Just $2) $3 }
|	enum                   EnumeratorList ';'		{ mk $ EnumConsts Nothing $2 }
|	typedef enum ID_SUE_TYPE EnumeratorList ID_SUE_TYPE ';'	{ mk $ EnumDecl $3 $4 $5 }
|	bitmask ID_SUE_TYPE EnumeratorList			{ mk $ EnumDecl $2 $3 $2 }

EnumeratorList :: { [StringNode] }
EnumeratorList
:	'{' Enumerators '}'					{ reverse $2 }

Enumerators :: { [StringNode] }
Enumerators
:	Enumerator						{ [$1] }
|	Enumerators Enumerator					{ $2 : $1 }

Enumerator :: { StringNode }
Enumerator
:	EnumeratorName ','					{ mk $ Enumerator $1 Nothing }
|	EnumeratorName '=' ConstExpr ','			{ mk $ Enumerator $1 (Just $3) }
|	namespace ID_CONST '{' Enumerators '}'			{ mk $ Namespace Global $2 $4 }
|	Comment							{ $1 }

EnumeratorName :: { Lexeme String }
EnumeratorName
:	ID_CONST						{ $1 }
|	ID_SUE_TYPE						{ $1 }

AggregateDecl :: { StringNode }
AggregateDecl
:	AggregateType ';'					{ $1 }
|	class ID_SUE_TYPE TypeParams ';'			{ mk $ ClassForward $2 $3 }
|	typedef AggregateType ID_SUE_TYPE ';'			{ mk $ Typedef $2 $3 }

AggregateType :: { StringNode }
AggregateType
:	struct ID_SUE_TYPE '{' MemberDeclList '}'		{ mk $ Struct $2 $4 }
|	struct this '{' MemberDeclList '}'			{ mk $ Struct $2 $4 }
|	union ID_SUE_TYPE '{' MemberDeclList '}'		{ mk $ Union $2 $4 }

MemberDeclList :: { [StringNode] }
MemberDeclList
:	MemberDecls						{ reverse $1 }

MemberDecls :: { [StringNode] }
MemberDecls
:	MemberDecl						{ [$1] }
|	MemberDecls MemberDecl					{ $2 : $1 }

MemberDecl :: { StringNode }
MemberDecl
:	QualType DeclSpec ';'					{ mk $ MemberDecl $1 $2 Nothing }
|	QualType DeclSpec ':' LIT_INTEGER ';'			{ mk $ MemberDecl $1 $2 (Just $4) }
|	namespace IdVar '{' MemberDeclList '}'			{ mk $ Namespace Global $2 $4 }
|	PreprocIfdef(MemberDeclList)				{ $1 }
|	Comment							{ $1 }

TypedefDecl :: { StringNode }
TypedefDecl
:	typedef QualType ID_SUE_TYPE ';'			{ mk $ Typedef $2 $3 }
|	typedef FunctionPrototype(ID_FUNC_TYPE) ';'		{ mk $ TypedefFunction $2 }

QualType :: { StringNode }
QualType
:	LeafType						{                               $1 }
|	LeafType '*'						{                     tyPointer $1 }
|	LeafType '*' '*'					{ tyPointer          (tyPointer $1) }
|	LeafType '*' const					{            tyConst (tyPointer $1) }
|	LeafType '*' const '*'					{ tyPointer (tyConst (tyPointer $1)) }
|	LeafType const						{                                tyConst $1 }
|	LeafType const '*'					{                     tyPointer (tyConst $1) }
|	LeafType const '*' const				{            tyConst (tyPointer (tyConst $1)) }
|	LeafType const '*' const '*'				{ tyPointer (tyConst (tyPointer (tyConst $1))) }
|	const LeafType						{                                tyConst $2 }
|	const LeafType '*'					{                     tyPointer (tyConst $2) }
|	const LeafType '*' const				{            tyConst (tyPointer (tyConst $2)) }
|	const LeafType '*' const '*'				{ tyPointer (tyConst (tyPointer (tyConst $2))) }

LeafType :: { StringNode }
LeafType
:	struct ID_SUE_TYPE					{ mk $ TyStruct $2 }
|	void							{ mk $ TyStd $1 }
|	this							{ mk $ TyStd $1 }
|	ID_FUNC_TYPE						{ mk $ TyFunc $1 }
|	ID_STD_TYPE						{ mk $ TyStd $1 }
|	ID_SUE_TYPE						{ mk $ TyUserDefined $1 }
|	ID_TYVAR						{ mk $ TyVar $1 }

FunctionDecl :: { StringNode }
FunctionDecl
:	FunctionDeclarator					{ $1 Global }
|	static FunctionDeclarator				{ $2 Static }

FunctionDeclarator :: { Scope -> StringNode }
FunctionDeclarator
:	FunctionPrototype(IdVar) WithError			{ \s -> mk $ FunctionDecl s $1 $2 }
|	FunctionPrototype(IdVar) CompoundStmt			{ \s -> mk $ FunctionDefn s $1 $2 }
|	QualType DeclSpec '{' Accessors '}'			{ \s -> mk $ Property $1 $2 (reverse $4) }

Accessors :: { [StringNode] }
Accessors
:	Accessor						{ [$1] }
|	Accessors Accessor					{ $2 : $1 }

Accessor :: { StringNode }
Accessor
:	IdVar FunctionParamList WithError			{ mk $ Accessor $1 $2 $3 }
|	Comment							{ $1 }

WithError :: { Maybe StringNode }
WithError
:	';'							{ Nothing }
|	with 'error' EnumeratorList				{ Just (mk $ ErrorList $3) }
|	with 'error' for IdVar ';'				{ Just (mk $ ErrorFor $4) }

FunctionPrototype(id)
:	QualType id FunctionParamList				{ mk $ FunctionPrototype $1 $2 $3 }
|	QualType id FunctionParamList const			{ mk $ FunctionPrototype $1 $2 $3 }

FunctionParamList :: { [StringNode] }
FunctionParamList
:	'(' ')'							{ [] }
|	'(' void ')'						{ [mk $ TyStd $2] }
|	'(' FunctionParams ')'					{ reverse $2 }
|	'(' FunctionParams ',' '...' ')'			{ reverse $ mk Ellipsis : $2 }

FunctionParams :: { [StringNode] }
FunctionParams
:	FunctionParam						{ [$1] }
|	FunctionParams ',' FunctionParam			{ $3 : $1 }

FunctionParam :: { StringNode }
FunctionParam
:	QualType DeclSpec					{ mk $ FunctionParam $1 $2 }

ConstDecl :: { StringNode }
ConstDecl
:	extern const LeafType ID_VAR ';'			{ mk $ ConstDecl $3 $4 }
|	const LeafType ID_VAR '=' InitialiserExpr ';'		{ mk $ ConstDefn Global $2 $3 $5 }
|	static const LeafType ID_VAR '=' InitialiserExpr ';'	{ mk $ ConstDefn Static $3 $4 $6 }

{
type StringLexeme = Lexeme String
type StringNode = Identity (Node Identity () StringLexeme)

mk :: a -> Identity a
mk = Identity

tyPointer = mk . TyPointer
tyConst = mk . TyConst

parseError :: Show text => (Lexeme text, [String]) -> Alex a
parseError (L (AlexPn _ line col) c t, options) =
    alexError $ show line <> ":" <> show col <> ": Parse error near " <> show c <> ": "
        <> show t <> "; expected one of " <> show options

lexwrap :: (Lexeme String -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

externC
    :: Lexeme String
    -> Lexeme String
    -> [StringNode]
    -> Lexeme String
    -> Alex StringNode
externC (L _ _ "__cplusplus") (L _ _ "\"C\"") decls (L _ _ "__cplusplus") =
    return $ mk $ ExternC decls
externC _ lang _ _ =
    alexError $ show lang
        <> ": extern \"C\" declaration invalid (did you spell __cplusplus right?)"

macroBodyStmt
    :: StringNode
    -> Lexeme String
    -> Alex StringNode
macroBodyStmt decls (L _ _ "0") =
    return $ mk $ MacroBodyStmt decls
macroBodyStmt _ cond =
    alexError $ show cond
        <> ": macro do-while body must end in 'while (0)'"
}
