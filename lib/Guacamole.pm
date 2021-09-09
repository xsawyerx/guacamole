package Guacamole;
# ABSTRACT: A parser toolkit for Standard Perl

use strict;
use warnings;
use Marpa::R2;
use constant {
    'DEBUG' => 0,
};

my $grammar_source = q{
lexeme default = latm => 1
:default ::= action => [ name, start, length, values ]

Program ::= StatementSeq

StatementSeq ::= Statement
               | Statement Semicolon
               | Statement Semicolon StatementSeq
               | BlockStatement
               | BlockStatement StatementSeq

# Statements that end with a block and do not need a semicolon terminator.
BlockStatement ::= LoopStatement
                 | PackageStatement
                 | SubStatement
                 | Condition
                 | BlockNonEmpty

Statement ::= BlockLevelExpression StatementModifier
            | BlockLevelExpression
            | EllipsisStatement
            | UseStatement
            | NoStatement
            | RequireStatement
            | PackageDeclaration
            | SubDeclaration

LoopStatement ::= ForStatement
                | WhileStatement
                | UntilStatement

ForStatement ::= ForStatementOp LParen Statement Semicolon Statement Semicolon Statement RParen Block ContinueExpr
               | ForStatementOp LParen Statement Semicolon Statement Semicolon Statement RParen Block
               | ForStatementOp LParen Semicolon Statement Semicolon Statement RParen Block ContinueExpr
               | ForStatementOp LParen Semicolon Statement Semicolon Statement RParen Block
               | ForStatementOp LParen Statement Semicolon Semicolon Statement RParen Block ContinueExpr
               | ForStatementOp LParen Statement Semicolon Semicolon Statement RParen Block
               | ForStatementOp LParen Statement Semicolon Statement Semicolon RParen Block ContinueExpr
               | ForStatementOp LParen Statement Semicolon Statement Semicolon RParen Block
               | ForStatementOp LParen Semicolon Semicolon Statement RParen Block ContinueExpr
               | ForStatementOp LParen Semicolon Semicolon Statement RParen Block
               | ForStatementOp LParen Statement Semicolon Semicolon RParen Block ContinueExpr
               | ForStatementOp LParen Statement Semicolon Semicolon RParen Block
               | ForStatementOp OpKeywordMy VarScalar LParen Expression RParen Block ContinueExpr
               | ForStatementOp OpKeywordMy VarScalar LParen Expression RParen Block
               | ForStatementOp VarScalar LParen Expression RParen Block ContinueExpr
               | ForStatementOp VarScalar LParen Expression RParen Block
               | ForStatementOp LParen Semicolon Semicolon RParen Block ContinueExpr
               | ForStatementOp LParen Semicolon Semicolon RParen Block
               | ForStatementOp LParen Expression RParen Block ContinueExpr
               | ForStatementOp LParen Expression RParen Block

ContinueExpr ::= OpKeywordContinue Block

ForStatementOp ::= OpKeywordFor
                 | OpKeywordForeach

WhileStatement ::= ConditionWhile LParen Expression RParen Block OpKeywordContinue Block
                 | ConditionWhile LParen Expression RParen Block
                 | ConditionWhile LParen RParen Block OpKeywordContinue Block
                 | ConditionWhile LParen RParen Block

UntilStatement ::= ConditionUntil LParen Expression RParen Block OpKeywordContinue Block
                 | ConditionUntil LParen Expression RParen Block

StatementModifier ::= ConditionIfPostfixExpr
                    | ConditionUnlessPostfixExpr
                    | ConditionWhilePostfixExpr
                    | ConditionUntilPostfixExpr
                    | ConditionForPostfixExpr
                    | ConditionForeachPostfixExpr

EllipsisStatement ::= Ellipsis

UseStatement ::= OpKeywordUse ClassIdent VersionExpr Expression
               | OpKeywordUse ClassIdent VersionExpr
               | OpKeywordUse ClassIdent Expression
               | OpKeywordUse VersionExpr
               | OpKeywordUse ClassIdent

NoStatement ::= OpKeywordNo ClassIdent VersionExpr Expression
              | OpKeywordNo ClassIdent VersionExpr
              | OpKeywordNo ClassIdent Expression
              | OpKeywordNo VersionExpr
              | OpKeywordNo ClassIdent

RequireStatement ::= OpKeywordRequire VersionExpr
                   | OpKeywordRequire ClassIdent
                   | OpKeywordRequire Expression

PackageStatement ::= OpKeywordPackage ClassIdent VersionExpr Block
                   | OpKeywordPackage ClassIdent Block
PackageDeclaration ::= OpKeywordPackage ClassIdent VersionExpr
                     | OpKeywordPackage ClassIdent

SubStatement ::= PhaseStatement Block
               | OpKeywordSub PhaseStatement Block
               | OpKeywordSub SubNameExpr SubDefinition

SubDeclaration ::= OpKeywordSub SubNameExpr

SubDefinition ::= SubAttrsDefinitionSeq SubSigsDefinition Block
                | SubAttrsDefinitionSeq Block
                | SubSigsDefinition Block
                | Block

SubAttrsDefinitionSeq ::= SubAttrsDefinition SubAttrsDefinitionSeq
                        | SubAttrsDefinition

SubAttrsDefinition ::= Colon IdentComp SubAttrArgs
                     | Colon IdentComp

SubSigsDefinition ::= ParenExpr

PhaseStatement ::= PhaseName

Condition ::= ConditionIfExpr ConditionElsifExpr ConditionElseExpr
            | ConditionIfExpr ConditionElseExpr
            | ConditionIfExpr ConditionElsifExpr
            | ConditionIfExpr
            | ConditionUnlessExpr

ConditionUnlessExpr         ::= ConditionUnless  LParen Expression RParen Block
ConditionIfExpr             ::= ConditionIf      LParen Expression RParen Block
ConditionElsifExpr          ::= ConditionElsif   LParen Expression RParen Block ConditionElsifExpr
                              | ConditionElsif   LParen Expression RParen Block
ConditionElseExpr           ::= ConditionElse    Block
ConditionIfPostfixExpr      ::= ConditionIf      Expression
ConditionUnlessPostfixExpr  ::= ConditionUnless  Expression
ConditionWhilePostfixExpr   ::= ConditionWhile   Expression
ConditionUntilPostfixExpr   ::= ConditionUntil   Expression
ConditionForPostfixExpr     ::= ConditionFor     Expression
ConditionForeachPostfixExpr ::= ConditionForeach Expression

Label ::= IdentComp Colon

# this is based on the order of ops in `perldoc perlop`
# U can be LHS of shift and up
# 0 can be LHS of assignment and up
# L can be LHS of comma and up
# R can be LHS of anything
ExprValueU    ::= Value
ExprValue0    ::= Value | OpUnaryKeywordExpr
ExprValueL    ::= Value | OpAssignKeywordExpr | OpUnaryKeywordExpr
ExprValueR    ::= Value | OpListKeywordExpr | OpAssignKeywordExpr | OpUnaryKeywordExpr
ExprArrowU    ::= ExprArrowU  OpArrow   ArrowRHS        | ExprValueU   action => ::first
ExprArrow0    ::= ExprArrowU  OpArrow   ArrowRHS        | ExprValue0   action => ::first
ExprArrowL    ::= ExprArrowU  OpArrow   ArrowRHS        | ExprValueL   action => ::first
ExprArrowR    ::= ExprArrowU  OpArrow   ArrowRHS        | ExprValueR   action => ::first
ExprIncU      ::= OpInc ExprArrowU | ExprArrowR OpInc   | ExprArrowU   action => ::first
ExprInc0      ::= OpInc ExprArrow0 | ExprArrowR OpInc   | ExprArrow0   action => ::first
ExprIncL      ::= OpInc ExprArrowL | ExprArrowR OpInc   | ExprArrowL   action => ::first
ExprIncR      ::= OpInc ExprArrowR | ExprArrowL OpInc   | ExprArrowR   action => ::first
ExprPowerU    ::= ExprIncU    OpPower   ExprUnaryU      | ExprIncU     action => ::first
ExprPower0    ::= ExprIncU    OpPower   ExprUnary0      | ExprInc0     action => ::first
ExprPowerL    ::= ExprIncU    OpPower   ExprUnaryL      | ExprIncL     action => ::first
ExprPowerR    ::= ExprIncU    OpPower   ExprUnaryR      | ExprIncR     action => ::first
ExprUnaryU    ::= OpUnary     ExprUnaryU                | ExprPowerU   action => ::first
ExprUnary0    ::= OpUnary     ExprUnary0                | ExprPower0   action => ::first
ExprUnaryL    ::= OpUnary     ExprUnaryL                | ExprPowerL   action => ::first
ExprUnaryR    ::= OpUnary     ExprUnaryR                | ExprPowerR   action => ::first
ExprRegexU    ::= ExprRegexU  OpRegex   ExprUnaryU      | ExprUnaryU   action => ::first
ExprRegex0    ::= ExprRegexU  OpRegex   ExprUnary0      | ExprUnary0   action => ::first
ExprRegexL    ::= ExprRegexU  OpRegex   ExprUnaryL      | ExprUnaryL   action => ::first
ExprRegexR    ::= ExprRegexU  OpRegex   ExprUnaryR      | ExprUnaryR   action => ::first
ExprMulU      ::= ExprMulU    OpMulti   ExprRegexU      | ExprRegexU   action => ::first
ExprMul0      ::= ExprMulU    OpMulti   ExprRegex0      | ExprRegex0   action => ::first
ExprMulL      ::= ExprMulU    OpMulti   ExprRegexL      | ExprRegexL   action => ::first
ExprMulR      ::= ExprMulU    OpMulti   ExprRegexR      | ExprRegexR   action => ::first
ExprAddU      ::= ExprAddU    OpAdd     ExprMulU        | ExprMulU     action => ::first
ExprAdd0      ::= ExprAddU    OpAdd     ExprMul0        | ExprMul0     action => ::first
ExprAddL      ::= ExprAddU    OpAdd     ExprMulL        | ExprMulL     action => ::first
ExprAddR      ::= ExprAddU    OpAdd     ExprMulR        | ExprMulR     action => ::first
ExprShiftU    ::= ExprShiftU  OpShift   ExprAddU        | ExprAddU     action => ::first
ExprShift0    ::= ExprShiftU  OpShift   ExprAdd0        | ExprAdd0     action => ::first
ExprShiftL    ::= ExprShiftU  OpShift   ExprAddL        | ExprAddL     action => ::first
ExprShiftR    ::= ExprShiftU  OpShift   ExprAddR        | ExprAddR     action => ::first
ExprNeq0      ::= ExprShift0  OpInequal ExprShift0      | ExprShift0   action => ::first
ExprNeqL      ::= ExprShift0  OpInequal ExprShiftL      | ExprShiftL   action => ::first
ExprNeqR      ::= ExprShift0  OpInequal ExprShiftR      | ExprShiftR   action => ::first
ExprEq0       ::= ExprNeq0    OpEqual   ExprNeq0        | ExprNeq0     action => ::first
ExprEqL       ::= ExprNeq0    OpEqual   ExprNeqL        | ExprNeqL     action => ::first
ExprEqR       ::= ExprNeq0    OpEqual   ExprNeqR        | ExprNeqR     action => ::first
ExprBinAnd0   ::= ExprBinAnd0 OpBinAnd  ExprEq0         | ExprEq0      action => ::first
ExprBinAndL   ::= ExprBinAnd0 OpBinAnd  ExprEqL         | ExprEqL      action => ::first
ExprBinAndR   ::= ExprBinAnd0 OpBinAnd  ExprEqR         | ExprEqR      action => ::first
ExprBinOr0    ::= ExprBinOr0  OpBinOr   ExprBinAnd0     | ExprBinAnd0  action => ::first
ExprBinOrL    ::= ExprBinOr0  OpBinOr   ExprBinAndL     | ExprBinAndL  action => ::first
ExprBinOrR    ::= ExprBinOr0  OpBinOr   ExprBinAndR     | ExprBinAndR  action => ::first
ExprLogAnd0   ::= ExprLogAnd0 OpLogAnd  ExprBinOr0      | ExprBinOr0   action => ::first
ExprLogAndL   ::= ExprLogAnd0 OpLogAnd  ExprBinOrL      | ExprBinOrL   action => ::first
ExprLogAndR   ::= ExprLogAnd0 OpLogAnd  ExprBinOrR      | ExprBinOrR   action => ::first
ExprLogOr0    ::= ExprLogOr0  OpLogOr   ExprLogAnd0     | ExprLogAnd0  action => ::first
ExprLogOrL    ::= ExprLogOr0  OpLogOr   ExprLogAndL     | ExprLogAndL  action => ::first
ExprLogOrR    ::= ExprLogOr0  OpLogOr   ExprLogAndR     | ExprLogAndR  action => ::first
ExprRange0    ::= ExprLogOr0  OpRange   ExprLogOr0      | ExprLogOr0   action => ::first
ExprRangeL    ::= ExprLogOr0  OpRange   ExprLogOrL      | ExprLogOrL   action => ::first
ExprRangeR    ::= ExprLogOr0  OpRange   ExprLogOrR      | ExprLogOrR   action => ::first
ExprCond0     ::= ExprRange0  OpTriThen ExprRange0 OpTriElse ExprCond0 | ExprRange0 action => ::first
ExprCondL     ::= ExprRange0  OpTriThen ExprRangeL OpTriElse ExprCondL | ExprRangeL action => ::first
ExprCondR     ::= ExprRange0  OpTriThen ExprRangeR OpTriElse ExprCondR | ExprRangeR action => ::first
ExprAssignL   ::= ExprCond0   OpAssign  ExprAssignL     | OpAssignKeywordExpr
                                                        | ExprCondL     action => ::first
ExprAssignR   ::= ExprCond0   OpAssign  ExprAssignR     | ExprCondR     action => ::first
ExprComma     ::= ExprAssignL OpComma ExprComma | ExprAssignL OpComma | ExprAssignR action => ::first
ExprNameNot   ::= OpNameNot   ExprNameNot               | ExprComma     action => ::first
ExprNameAnd   ::= ExprNameAnd OpNameAnd ExprNameNot     | ExprNameNot   action => ::first
ExprNameOr    ::= ExprNameOr  OpNameOr  ExprNameAnd     | ExprNameAnd   action => ::first
Expression    ::=                                         ExprNameOr    action => ::first

# These will never be evaluated as a hashref (LiteralHash)
# because hashrefs are not allowed to be top-level
# Being combined combined with '+' or 'return' means they aren't top-level,
# but follow top-level tokens ('+' or 'return')
NonBraceExprValueU    ::= NonBraceValue
NonBraceExprValue0    ::= NonBraceValue | OpUnaryKeywordExpr
NonBraceExprValueL    ::= NonBraceValue | OpAssignKeywordExpr | OpUnaryKeywordExpr
NonBraceExprValueR    ::= NonBraceValue | OpListKeywordExpr | OpAssignKeywordExpr | OpUnaryKeywordExpr
NonBraceExprArrowU    ::= NonBraceExprArrowU  OpArrow   ArrowRHS        | NonBraceExprValueU   action => ::first
NonBraceExprArrow0    ::= NonBraceExprArrowU  OpArrow   ArrowRHS        | NonBraceExprValue0   action => ::first
NonBraceExprArrowL    ::= NonBraceExprArrowU  OpArrow   ArrowRHS        | NonBraceExprValueL   action => ::first
NonBraceExprArrowR    ::= NonBraceExprArrowU  OpArrow   ArrowRHS        | NonBraceExprValueR   action => ::first
NonBraceExprIncU      ::= OpInc ExprArrowU | NonBraceExprArrowR OpInc   | NonBraceExprArrowU   action => ::first
NonBraceExprInc0      ::= OpInc ExprArrow0 | NonBraceExprArrowR OpInc   | NonBraceExprArrow0   action => ::first
NonBraceExprIncL      ::= OpInc ExprArrowL | NonBraceExprArrowR OpInc   | NonBraceExprArrowL   action => ::first
NonBraceExprIncR      ::= OpInc ExprArrowR | NonBraceExprArrowL OpInc   | NonBraceExprArrowR   action => ::first
NonBraceExprPowerU    ::= NonBraceExprIncU    OpPower   ExprUnaryU      | NonBraceExprIncU     action => ::first
NonBraceExprPower0    ::= NonBraceExprIncU    OpPower   ExprUnary0      | NonBraceExprInc0     action => ::first
NonBraceExprPowerL    ::= NonBraceExprIncU    OpPower   ExprUnaryL      | NonBraceExprIncL     action => ::first
NonBraceExprPowerR    ::= NonBraceExprIncU    OpPower   ExprUnaryR      | NonBraceExprIncR     action => ::first
NonBraceExprUnaryU    ::= OpUnary     ExprUnaryU                | NonBraceExprPowerU   action => ::first
NonBraceExprUnary0    ::= OpUnary     ExprUnary0                | NonBraceExprPower0   action => ::first
NonBraceExprUnaryL    ::= OpUnary     ExprUnaryL                | NonBraceExprPowerL   action => ::first
NonBraceExprUnaryR    ::= OpUnary     ExprUnaryR                | NonBraceExprPowerR   action => ::first
NonBraceExprRegexU    ::= NonBraceExprRegexU  OpRegex   ExprUnaryU      | NonBraceExprUnaryU   action => ::first
NonBraceExprRegex0    ::= NonBraceExprRegexU  OpRegex   ExprUnary0      | NonBraceExprUnary0   action => ::first
NonBraceExprRegexL    ::= NonBraceExprRegexU  OpRegex   ExprUnaryL      | NonBraceExprUnaryL   action => ::first
NonBraceExprRegexR    ::= NonBraceExprRegexU  OpRegex   ExprUnaryR      | NonBraceExprUnaryR   action => ::first
NonBraceExprMulU      ::= NonBraceExprMulU    OpMulti   ExprRegexU      | NonBraceExprRegexU   action => ::first
NonBraceExprMul0      ::= NonBraceExprMulU    OpMulti   ExprRegex0      | NonBraceExprRegex0   action => ::first
NonBraceExprMulL      ::= NonBraceExprMulU    OpMulti   ExprRegexL      | NonBraceExprRegexL   action => ::first
NonBraceExprMulR      ::= NonBraceExprMulU    OpMulti   ExprRegexR      | NonBraceExprRegexR   action => ::first
NonBraceExprAddU      ::= NonBraceExprAddU    OpAdd     ExprMulU        | NonBraceExprMulU     action => ::first
NonBraceExprAdd0      ::= NonBraceExprAddU    OpAdd     ExprMul0        | NonBraceExprMul0     action => ::first
NonBraceExprAddL      ::= NonBraceExprAddU    OpAdd     ExprMulL        | NonBraceExprMulL     action => ::first
NonBraceExprAddR      ::= NonBraceExprAddU    OpAdd     ExprMulR        | NonBraceExprMulR     action => ::first
NonBraceExprShiftU    ::= NonBraceExprShiftU  OpShift   ExprAddU        | NonBraceExprAddU     action => ::first
NonBraceExprShift0    ::= NonBraceExprShiftU  OpShift   ExprAdd0        | NonBraceExprAdd0     action => ::first
NonBraceExprShiftL    ::= NonBraceExprShiftU  OpShift   ExprAddL        | NonBraceExprAddL     action => ::first
NonBraceExprShiftR    ::= NonBraceExprShiftU  OpShift   ExprAddR        | NonBraceExprAddR     action => ::first
NonBraceExprNeq0      ::= NonBraceExprShift0  OpInequal ExprShift0      | NonBraceExprShift0   action => ::first
NonBraceExprNeqL      ::= NonBraceExprShift0  OpInequal ExprShiftL      | NonBraceExprShiftL   action => ::first
NonBraceExprNeqR      ::= NonBraceExprShift0  OpInequal ExprShiftR      | NonBraceExprShiftR   action => ::first
NonBraceExprEq0       ::= NonBraceExprNeq0    OpEqual   ExprNeq0        | NonBraceExprNeq0     action => ::first
NonBraceExprEqL       ::= NonBraceExprNeq0    OpEqual   ExprNeqL        | NonBraceExprNeqL     action => ::first
NonBraceExprEqR       ::= NonBraceExprNeq0    OpEqual   ExprNeqR        | NonBraceExprNeqR     action => ::first
NonBraceExprBinAnd0   ::= NonBraceExprBinAnd0 OpBinAnd  ExprEq0         | NonBraceExprEq0      action => ::first
NonBraceExprBinAndL   ::= NonBraceExprBinAnd0 OpBinAnd  ExprEqL         | NonBraceExprEqL      action => ::first
NonBraceExprBinAndR   ::= NonBraceExprBinAnd0 OpBinAnd  ExprEqR         | NonBraceExprEqR      action => ::first
NonBraceExprBinOr0    ::= NonBraceExprBinOr0  OpBinOr   ExprBinAnd0     | NonBraceExprBinAnd0  action => ::first
NonBraceExprBinOrL    ::= NonBraceExprBinOr0  OpBinOr   ExprBinAndL     | NonBraceExprBinAndL  action => ::first
NonBraceExprBinOrR    ::= NonBraceExprBinOr0  OpBinOr   ExprBinAndR     | NonBraceExprBinAndR  action => ::first
NonBraceExprLogAnd0   ::= NonBraceExprLogAnd0 OpLogAnd  ExprBinOr0      | NonBraceExprBinOr0   action => ::first
NonBraceExprLogAndL   ::= NonBraceExprLogAnd0 OpLogAnd  ExprBinOrL      | NonBraceExprBinOrL   action => ::first
NonBraceExprLogAndR   ::= NonBraceExprLogAnd0 OpLogAnd  ExprBinOrR      | NonBraceExprBinOrR   action => ::first
NonBraceExprLogOr0    ::= NonBraceExprLogOr0  OpLogOr   ExprLogAnd0     | NonBraceExprLogAnd0  action => ::first
NonBraceExprLogOrL    ::= NonBraceExprLogOr0  OpLogOr   ExprLogAndL     | NonBraceExprLogAndL  action => ::first
NonBraceExprLogOrR    ::= NonBraceExprLogOr0  OpLogOr   ExprLogAndR     | NonBraceExprLogAndR  action => ::first
NonBraceExprRange0    ::= NonBraceExprLogOr0  OpRange   ExprLogOr0      | NonBraceExprLogOr0   action => ::first
NonBraceExprRangeL    ::= NonBraceExprLogOr0  OpRange   ExprLogOrL      | NonBraceExprLogOrL   action => ::first
NonBraceExprRangeR    ::= NonBraceExprLogOr0  OpRange   ExprLogOrR      | NonBraceExprLogOrR   action => ::first
NonBraceExprCond0     ::= NonBraceExprRange0  OpTriThen ExprRange0 OpTriElse ExprCond0 | NonBraceExprRange0 action => ::first
NonBraceExprCondL     ::= NonBraceExprRange0  OpTriThen ExprRangeL OpTriElse ExprCondL | NonBraceExprRangeL action => ::first
NonBraceExprCondR     ::= NonBraceExprRange0  OpTriThen ExprRangeR OpTriElse ExprCondR | NonBraceExprRangeR action => ::first
NonBraceExprAssignL   ::= NonBraceExprCond0   OpAssign  ExprAssignL     | OpAssignKeywordExpr
                                                        | NonBraceExprCondL     action => ::first
NonBraceExprAssignR   ::= NonBraceExprCond0   OpAssign  ExprAssignR     | NonBraceExprCondR     action => ::first


NonBraceExprComma     ::= NonBraceExprAssignL OpComma ExprComma    | NonBraceExprAssignR action => ::first

# Comma is only allowed if it follows a keyword operator, to avoid block/hash disambiguation in perl.
BlockLevelExprNameNot ::= OpNameNot ExprNameNot | NonBraceExprAssignR action => ::first
BlockLevelExprNameAnd ::= BlockLevelExprNameAnd OpNameAnd ExprNameNot | BlockLevelExprNameNot action => ::first
BlockLevelExprNameOr  ::= BlockLevelExprNameOr OpNameOr ExprNameAnd | BlockLevelExprNameAnd action => ::first
BlockLevelExpression  ::= BlockLevelExprNameOr action => ::first

Value         ::= Literal | NonLiteral | QLikeValue

# Arguments of operators according to the operator precedence
OpUnaryKeywordArg         ::= ExprShiftR
OpUnaryKeywordArgNonBrace ::= NonBraceExprShiftR
OpAssignKeywordArg        ::= ExprAssignR
OpListKeywordArg          ::= ExprComma
OpListKeywordArgNonBrace  ::= NonBraceExprComma

# Same as Value above, but with a NonBraceLiteral
NonBraceValue ::= NonBraceLiteral | NonLiteral | QLikeValue

NonLiteral ::= Variable
             | DerefVariable
             | Modifier Variable
             | Modifier ParenExpr
             | UnderscoreValues
             | SubCall
             | PackageArrow
             | ParenExpr ElemSeq0
             | OpNullaryKeywordExpr
             | DiamondExpr

DiamondExpr ::= Diamond
              | DoubleDiamond

# This is written this way because of whitespace rules
Diamond ::= '<' VarScalar '>'
          | '<' BuiltinFilehandle '>'
          | '<>'

# This is written this way because of whitespace rules
DoubleDiamond ::= '<<' VarScalar '>>'
                | '<<' BuiltinFilehandle '>>'
                | '<<>>'

ParenExpr ::= LParen Expression RParen
            | LParen RParen # support ()

Modifier  ::= OpKeywordMy | OpKeywordOur | OpKeywordLocal | OpKeywordState

ElemSeq0 ::= Element*
ElemSeq1 ::= Element+
Element  ::= ArrayElem | HashElem

# UnderscoreData and UnderscoreEnd are not values
UnderscoreValues ::= UnderscorePackage
                   | UnderscoreFile
                   | UnderscoreLine
                   | UnderscoreSub

# Silence these until they are supported
#UnderscoreTokens ::= UnderscoreValues
#                   | UnderscoreData
#                   | UnderscoreEnd

Variable ::= GlobalVarExpr
           | VarScalar
           | VarArray
           | VarHash
           | VarCode
           | VarGlob
           | VarArrayTop

GlobalVarExpr ::= '$#'
                | SigilScalar GlobalVariables ElemSeq0
                | SigilArray  GlobalVariables ElemSeq0
                | SigilHash   GlobalVariables ElemSeq0
                | SigilGlob   GlobalVariables ElemSeq0

VarScalar   ::= SigilScalar VarIdentExpr ElemSeq0
VarArray    ::= SigilArray VarIdentExpr ElemSeq0
VarHash     ::= SigilHash VarIdentExpr ElemSeq0
VarCode     ::= SigilCode VarIdentExpr
VarGlob     ::= SigilGlob VarIdentExpr
VarArrayTop ::= SigilArrayTop VarIdentExpr

SubCall ::= SubNameCallExpr CallArgs
          | VarCode CallArgs

PackageArrow  ::= SubNameExpr OpArrow PackageArrowRHS

PackageArrowRHS ::= ArrowMethodCall
                  | ArrowIndirectCall

# Used for function calls (Non-QLikeValue string)
SubNameCallExpr ::= SubNameNonQLike
                  | SubNameCallExpr PackageSep SubNameNonQLike

# Used for defining subs (no limits)
SubNameExpr ::= SubName
              | SubNameExpr PackageSep SubName

# SubName is used for methods and subroutine definitions
# They are not limited in any regard (other than no-digits in first char)
SubName          ~ LeadingSubLetter CoreSubLetters
LeadingSubLetter ~ [a-zA-Z_]
CoreSubLetters   ~ [a-zA-Z0-9_]*

# SubNameNonQLike is for function calls
# They are not allowed to be:
# q / qq / qw / qr / qx
# s / m / y / tr
SubNameNonQLike ~
                  NonQLikeLetters                  # [non-qlike]
                | NonQLikeLetters AllSubLetters    # [non-qlike][*]
                | 'q' NonQRWXLetters               # q[non-qrwx]
                | 'q' NonQRWXLetters AllSubLetters # q[non-qrwx][*]
                | 'qq' AllSubLetters               # qq[*]
                | 'qr' AllSubLetters               # qr[*]
                | 'qw' AllSubLetters               # qw[*]
                | 'qx' AllSubLetters               # qx[*]
                | 't'                              # t
                | 't' NonRLetter                   # t[non-r]
                | 't' NonRLetter AllSubLetters     # t[non-r][*]
                | 'tr' AllSubLetters               # tr[*]
                | 's' AllSubLetters                # s[*]
                | 'm' AllSubLetters                # m[*]
                | 'y' AllSubLetters                # y[*]

# Variables are defined using a different ident
# Namespaced variables ($x::y) might have a different ident
VarIdentExpr ::= VarIdent
               | VarIdentExpr PackageSep VarIdent

VarIdent ~ NonGlobalVarLetters
         | NonGlobalVarLetters AllVarLetters
         | '_' AllVarLetters

NonGlobalVarLetters ~ [a-zA-Z]+
AllVarLetters       ~ [a-zA-Z0-9_]+

GlobalVariables ~ '!'
                | '"'
                | '%'
                | '&'
                | [']
                | '('
                | ')'
                | '*'
                | '+'
                | ','
                | '-'
                | '.'
                | '/'
                | ':'
                | ';'
                | '<'
                | '='
                | '>'
                | '?'
                | '@'
                | '['
                | '\\'
                | ']'
                | '^'
                | '_'
                | '`'
                | '|'
                | '~'
                | '$'
                | '^A'
                | '^C'
                | '{^CHILD_ERROR_NATIVE}'
                | '^D'
                | '^E'
                | '{^ENCODING}'
                | '^F'
                | '{^GLOBAL_PHASE}'
                | '^H'
                | '^I'
                | '^L'
                | '{^LAST_FH}'
                | '^M'
                | '{^MATCH}'
                | '^N'
                | '^O'
                | '{^OPEN}'
                | '^P'
                | '{^POSTMATCH}'
                | '{^PREMATCH}'
                | '^R'
                | '{^RE_COMPILE_RECURSION_LIMIT}'
                | '{^RE_DEBUG_FLAGS}'
                | '{^RE_TRIE_MAXBUF}'
                | '^S'
                | '{^SAFE_LOCALES}'
                | '^T'
                | '{^TAINT}'
                | '{^UNICODE}'
                | '{^UTF8CACHE}'
                | '{^UTF8LOCALE}'
                | '^V'
                | '^W'
                | '{^WARNING_BITS}'
                | '{^WIN32_SLOPPY_STAT}'
                | '^X'
                | '{^CAPTURE}'
                | '{^CAPTURE_ALL}'
                | Digits

# This uses the same definition as subroutine names
# In the future, we might want to split those
# but they are basically the same
ClassIdent ::= SubNameExpr

CallArgs ::= ParenExpr

# Depending on context, "{}" may be interpreted as an empty block or a hash literal.
Block         ::= BlockEmpty                  action => ::first
                | BlockNonEmpty               action => ::first
BlockEmpty    ::= LBrace RBrace               name => Block
BlockNonEmpty ::= LBrace StatementSeq RBrace  name => Block

ArrayElem ::= LBracket Expression RBracket

HashElem ::= LBrace Expression RBrace

NonBraceLiteral ::= LitNumber
                  | LitArray
                  | LitString
                  | InterpolString
                  | LitHashEmpty

Literal         ::= NonBraceLiteral
                  | LitHashNonEmpty

LitArray       ::= LBracket Expression RBracket
                 | LBracket RBracket

# Depending on context, "{}" may be interpreted as an empty block or a hash literal.
LitHashEmpty    ::= LBrace RBrace             name => LitHash
LitHashNonEmpty ::= LBrace Expression RBrace  name => LitHash

LitString      ::= SingleQuote NonSingleOrEscapedQuote_Many SingleQuote
                 | SingleQuote SingleQuote

InterpolString ::= DoubleQuote NonDoubleOrEscapedQuote_Many DoubleQuote
                 | DoubleQuote DoubleQuote

ArrowRHS ::= ArrowDerefCall
           | ArrowDerefVariable
           | ArrowMethodCall
           | ArrowIndirectCall
           | ElemSeq1

ArrowDerefCall     ::= CallArgs
ArrowDerefVariable ::= DerefVariableArgsAll
                     | DerefVariableSlice
ArrowMethodCall    ::= SubNameExpr CallArgs
                     | SubNameExpr
ArrowIndirectCall  ::= SigilScalar VarIdentExpr CallArgs

DerefVariableArgsAll ::= '$*' | '@*' | '%*' | '&*' | '**' | '$#*'

DerefVariableSlice ::= '@[' Expression ']'
                     | '@{' Expression '}'
                     | '%[' Expression ']'
                     | '%{' Expression '}'

DerefVariable ::= SigilScalar   BlockNonEmpty
                | SigilArray    BlockNonEmpty ElemSeq0
                | SigilHash     BlockNonEmpty ElemSeq0
                | SigilGlob     BlockNonEmpty
                | SigilArrayTop BlockNonEmpty

OpNullaryKeywordExpr ::=
      OpKeywordBreakExpr
    | OpKeywordForkExpr
    | OpKeywordGetloginExpr
    | OpKeywordGetppidExpr
    | OpKeywordGetpwentExpr
    | OpKeywordGetgrentExpr
    | OpKeywordGethostentExpr
    | OpKeywordGetnetentExpr
    | OpKeywordGetprotoentExpr
    | OpKeywordGetserventExpr
    | OpKeywordSetpwentExpr
    | OpKeywordSetgrentExpr
    | OpKeywordEndpwentExpr
    | OpKeywordEndgrentExpr
    | OpKeywordEndhostentExpr
    | OpKeywordEndnetentExpr
    | OpKeywordEndprotoentExpr
    | OpKeywordEndserventExpr
    | OpKeywordEvalExpr
    | OpKeywordSubExpr
    | OpKeywordTimeExpr
    | OpKeywordTimesExpr
    | OpKeywordWaitExpr
    | OpKeywordWantarrayExpr

# Unary keyword operators:
#       abs $a, $b => ((abs $a), $b)
#       abs $a = $b => ((abs $a) = $b)
# List keyword operators:
#       push $a, $b => (push ($a, $b))
#       push $a = $b => (push ($a = $b))
# Assign keyword operators:
#       goto $a, $b => ((goto $a), $b)
#       goto $a = $b => (goto ($a = $b))

OpUnaryKeywordExpr ::=
      OpKeywordAbsExpr
    | OpKeywordAlarmExpr
    | OpKeywordCallerExpr
    | OpKeywordChdirExpr
    | OpKeywordChompExpr
    | OpKeywordChopExpr
    | OpKeywordChrExpr
    | OpKeywordChrootExpr
    | OpKeywordCloseExpr
    | OpKeywordClosedirExpr
    | OpKeywordCosExpr
    | OpKeywordDbmcloseExpr
    | OpKeywordDefinedExpr
    | OpKeywordDeleteExpr
    | OpKeywordDoExpr
    | OpKeywordEachExpr
    | OpKeywordEofExpr
    | OpKeywordEvalbytesExpr
    | OpKeywordExistsExpr
    | OpKeywordExitExpr
    | OpKeywordExpExpr
    | OpKeywordFcExpr
    | OpKeywordFilenoExpr
    | OpKeywordGetcExpr
    | OpKeywordGetpeernameExpr
    | OpKeywordGetpgrpExpr
    | OpKeywordGetpwnamExpr
    | OpKeywordGetgrnamExpr
    | OpKeywordGethostbynameExpr
    | OpKeywordGetnetbynameExpr
    | OpKeywordGetprotobynameExpr
    | OpKeywordGetpwuidExpr
    | OpKeywordGetgrgidExpr
    | OpKeywordGetprotobynumberExpr
    | OpKeywordSethostentExpr
    | OpKeywordSetnetentExpr
    | OpKeywordSetprotoentExpr
    | OpKeywordSetserventExpr
    | OpKeywordGetsocknameExpr
    | OpKeywordGmtimeExpr
    | OpKeywordHexExpr
    | OpKeywordIntExpr
    | OpKeywordKeysExpr
    | OpKeywordLcExpr
    | OpKeywordLcfirstExpr
    | OpKeywordLengthExpr
    | OpKeywordLocaltimeExpr
    | OpKeywordLockExpr
    | OpKeywordLogExpr
    | OpKeywordLstatExpr
    | OpKeywordOctExpr
    | OpKeywordOrdExpr
    | OpKeywordPopExpr
    | OpKeywordPosExpr
    | OpKeywordPrototypeExpr
    | OpKeywordQuotemetaExpr
    | OpKeywordRandExpr
    | OpKeywordReaddirExpr
    | OpKeywordReadlineExpr
    | OpKeywordReadlinkExpr
    | OpKeywordReadpipeExpr
    | OpKeywordRefExpr
    | OpKeywordResetExpr
    | OpKeywordRewinddirExpr
    | OpKeywordRmdirExpr
    | OpKeywordScalarExpr
    | OpKeywordShiftExpr
    | OpKeywordSinExpr
    | OpKeywordSleepExpr
    | OpKeywordSqrtExpr
    | OpKeywordSrandExpr
    | OpKeywordStatExpr
    | OpKeywordStudyExpr
    | OpKeywordTellExpr
    | OpKeywordTelldirExpr
    | OpKeywordTiedExpr
    | OpKeywordUcExpr
    | OpKeywordUcfirstExpr
    | OpKeywordUmaskExpr
    | OpKeywordUndefExpr
    | OpKeywordUnlinkExpr
    | OpKeywordUntieExpr
    | OpKeywordUtimeExpr
    | OpKeywordValuesExpr
    | OpFileExpr

OpListKeywordExpr ::=
      OpKeywordAcceptExpr
    | OpKeywordAtan2Expr
    | OpKeywordBindExpr
    | OpKeywordBinmodeExpr
    | OpKeywordBlessExpr
    | OpKeywordChmodExpr
    | OpKeywordChownExpr
    | OpKeywordConnectExpr
    | OpKeywordCryptExpr
    | OpKeywordDbmopenExpr
    | OpKeywordDieExpr
    | OpKeywordFcntlExpr
    | OpKeywordFlockExpr
    | OpKeywordGetpriorityExpr
    | OpKeywordGetservbynameExpr
    | OpKeywordGethostbyaddrExpr
    | OpKeywordGetnetbyaddrExpr
    | OpKeywordGetservbyportExpr
    | OpKeywordExecExpr
    | OpKeywordGetsockoptExpr
    | OpKeywordGlobExpr
    | OpKeywordGrepExpr
    | OpKeywordIndexExpr
    | OpKeywordIoctlExpr
    | OpKeywordJoinExpr
    | OpKeywordKillExpr
    | OpKeywordLinkExpr
    | OpKeywordListenExpr
    | OpKeywordMapExpr
    | OpKeywordMkdirExpr
    | OpKeywordMsgctlExpr
    | OpKeywordMsggetExpr
    | OpKeywordMsgrcvExpr
    | OpKeywordMsgsndExpr
    | OpKeywordOpenExpr
    | OpKeywordOpendirExpr
    | OpKeywordPackExpr
    | OpKeywordPipeExpr
    | OpKeywordPrintExpr
    | OpKeywordPrintfExpr
    | OpKeywordPushExpr
    | OpKeywordReadExpr
    | OpKeywordRecvExpr
    | OpKeywordRenameExpr
    | OpKeywordReturnExpr
    | OpKeywordReverseExpr
    | OpKeywordRindexExpr
    | OpKeywordSayExpr
    | OpKeywordSeekExpr
    | OpKeywordSeekdirExpr
    | OpKeywordSelectExpr
    | OpKeywordSemctlExpr
    | OpKeywordSemgetExpr
    | OpKeywordSemopExpr
    | OpKeywordSendExpr
    | OpKeywordSetpgrpExpr
    | OpKeywordSetpriorityExpr
    | OpKeywordSetsockoptExpr
    | OpKeywordShmctlExpr
    | OpKeywordShmgetExpr
    | OpKeywordShmreadExpr
    | OpKeywordShmwriteExpr
    | OpKeywordShutdownExpr
    | OpKeywordSocketExpr
    | OpKeywordSocketpairExpr
    | OpKeywordSortExpr
    | OpKeywordSpliceExpr
    | OpKeywordSplitExpr
    | OpKeywordSprintfExpr
    | OpKeywordSubstrExpr
    | OpKeywordSymlinkExpr
    | OpKeywordSyscallExpr
    | OpKeywordSysopenExpr
    | OpKeywordSysreadExpr
    | OpKeywordSysseekExpr
    | OpKeywordSyswriteExpr
    | OpKeywordSystemExpr
    | OpKeywordTieExpr
    | OpKeywordTruncateExpr
    | OpKeywordUnpackExpr
    | OpKeywordUnshiftExpr
    | OpKeywordVecExpr
    | OpKeywordWaitpidExpr
    | OpKeywordWarnExpr
    | OpKeywordWriteExpr

OpAssignKeywordExpr ::=
      OpKeywordDumpExpr
    | OpKeywordGotoExpr
    | OpKeywordLastExpr
    | OpKeywordNextExpr
    | OpKeywordRedoExpr

OpKeywordAbsExpr              ::= OpKeywordAbs OpUnaryKeywordArg
                                | OpKeywordAbs

OpKeywordAcceptExpr           ::= OpKeywordAccept OpListKeywordArg

OpKeywordAlarmExpr            ::= OpKeywordAlarm OpUnaryKeywordArg
                                | OpKeywordAlarm

OpKeywordAtan2Expr            ::= OpKeywordAtan2 OpListKeywordArg

OpKeywordBindExpr             ::= OpKeywordBind OpListKeywordArg

OpKeywordBinmodeExpr          ::= OpKeywordBinmode OpListKeywordArg
                                | OpKeywordBinmode LParen BuiltinFilehandle RParen
                                | OpKeywordBinmode BuiltinFilehandle
                                | OpKeywordBinmode LParen BuiltinFilehandle OpComma ExprAssignR RParen
                                | OpKeywordBinmode BuiltinFilehandle OpComma ExprAssignR

OpKeywordBlessExpr            ::= OpKeywordBless OpListKeywordArg

OpKeywordBreakExpr            ::= OpKeywordBreak Label
                                | OpKeywordBreak

OpKeywordCallerExpr           ::= OpKeywordCaller OpUnaryKeywordArg
                                | OpKeywordCaller

OpKeywordChdirExpr            ::= OpKeywordChdir OpUnaryKeywordArg
                                | OpKeywordChdir

OpKeywordChmodExpr            ::= OpKeywordChmod OpListKeywordArg

OpKeywordChompExpr            ::= OpKeywordChomp OpUnaryKeywordArg
                                | OpKeywordChomp

OpKeywordChopExpr             ::= OpKeywordChop OpUnaryKeywordArg
                                | OpKeywordChop

OpKeywordChownExpr            ::= OpKeywordChown OpListKeywordArg

OpKeywordChrExpr              ::= OpKeywordChr OpUnaryKeywordArg
                                | OpKeywordChr

OpKeywordChrootExpr           ::= OpKeywordChroot OpUnaryKeywordArg
                                | OpKeywordChroot

OpKeywordCloseExpr            ::= OpKeywordClose OpUnaryKeywordArg
                                | OpKeywordClose

OpKeywordClosedirExpr         ::= OpKeywordClosedir OpUnaryKeywordArg

OpKeywordConnectExpr          ::= OpKeywordConnect OpListKeywordArg

OpKeywordCosExpr              ::= OpKeywordCos OpUnaryKeywordArg

OpKeywordCryptExpr            ::= OpKeywordCrypt OpListKeywordArg

OpKeywordDbmcloseExpr         ::= OpKeywordDbmclose OpUnaryKeywordArg

OpKeywordDbmopenExpr          ::= OpKeywordDbmopen OpListKeywordArg

OpKeywordDefinedExpr          ::= OpKeywordDefined OpUnaryKeywordArg
                                | OpKeywordDefined

OpKeywordDeleteExpr           ::= OpKeywordDelete OpUnaryKeywordArg

OpKeywordDieExpr              ::= OpKeywordDie OpListKeywordArg

OpKeywordDoExpr               ::= OpKeywordDo BlockNonEmpty
                                | OpKeywordDo OpUnaryKeywordArgNonBrace

OpKeywordDumpExpr             ::= OpKeywordDump OpAssignKeywordArg
                                | OpKeywordDump Label
                                | OpKeywordDump

OpKeywordEachExpr             ::= OpKeywordEach OpUnaryKeywordArg

OpKeywordEofExpr              ::= OpKeywordEof OpUnaryKeywordArg
                                | OpKeywordEof

OpKeywordEvalExpr             ::= OpKeywordEval BlockNonEmpty

OpKeywordEvalbytesExpr        ::= OpKeywordEvalbytes OpUnaryKeywordArg
                                | OpKeywordEvalbytes

OpKeywordExistsExpr           ::= OpKeywordExists OpUnaryKeywordArg

OpKeywordExitExpr             ::= OpKeywordExit OpUnaryKeywordArg
                                | OpKeywordExit

OpKeywordExpExpr              ::= OpKeywordExp OpUnaryKeywordArg
                                | OpKeywordExp

OpKeywordFcExpr               ::= OpKeywordFc OpUnaryKeywordArg
                                | OpKeywordFc

OpKeywordFcntlExpr            ::= OpKeywordFcntl OpListKeywordArg

OpKeywordFilenoExpr           ::= OpKeywordFileno OpUnaryKeywordArg

OpKeywordFlockExpr            ::= OpKeywordFlock OpListKeywordArg

OpKeywordForkExpr             ::= OpKeywordFork LParen RParen
                                | OpKeywordFork

OpKeywordGetcExpr             ::= OpKeywordGetc OpUnaryKeywordArg
                                | OpKeywordGetc

OpKeywordGetloginExpr         ::= OpKeywordGetlogin LParen RParen
                                | OpKeywordGetlogin

OpKeywordGetpeernameExpr      ::= OpKeywordGetpeername OpUnaryKeywordArg

OpKeywordGetpgrpExpr          ::= OpKeywordGetpgrp OpUnaryKeywordArg

OpKeywordGetppidExpr          ::= OpKeywordGetppid LParen RParen
                                | OpKeywordGetppid

OpKeywordGetpriorityExpr      ::= OpKeywordGetpriority OpListKeywordArg

OpKeywordGetpwnamExpr         ::= OpKeywordGetpwnam OpUnaryKeywordArg

OpKeywordGetgrnamExpr         ::= OpKeywordGetgrnam OpUnaryKeywordArg

OpKeywordGethostbynameExpr    ::= OpKeywordGethostbyname OpUnaryKeywordArg

OpKeywordGetnetbynameExpr     ::= OpKeywordGetnetbyname OpUnaryKeywordArg

OpKeywordGetprotobynameExpr   ::= OpKeywordGetprotobyname OpUnaryKeywordArg

OpKeywordGetpwuidExpr         ::= OpKeywordGetpwuid OpUnaryKeywordArg

OpKeywordGetgrgidExpr         ::= OpKeywordGetgrgid OpUnaryKeywordArg

OpKeywordGetservbynameExpr    ::= OpKeywordGetservbyname OpListKeywordArg

OpKeywordGethostbyaddrExpr    ::= OpKeywordGethostbyaddr OpListKeywordArg

OpKeywordGetnetbyaddrExpr     ::= OpKeywordGetnetbyaddr OpListKeywordArg

OpKeywordGetprotobynumberExpr ::= OpKeywordGetprotobynumber OpUnaryKeywordArg

OpKeywordGetservbyportExpr    ::= OpKeywordGetservbyport OpListKeywordArg

OpKeywordGetpwentExpr         ::= OpKeywordGetpwent LParen RParen
                                | OpKeywordGetpwent

OpKeywordGetgrentExpr         ::= OpKeywordGetgrent LParen RParen
                                | OpKeywordGetgrent

OpKeywordGethostentExpr       ::= OpKeywordGethostent LParen RParen
                                | OpKeywordGethostent

OpKeywordGetnetentExpr        ::= OpKeywordGetnetent LParen RParen
                                | OpKeywordGetnetent

OpKeywordGetprotoentExpr      ::= OpKeywordGetprotoent LParen RParen
                                | OpKeywordGetprotoent

OpKeywordGetserventExpr       ::= OpKeywordGetservent LParen RParen
                                | OpKeywordGetservent

OpKeywordSetpwentExpr         ::= OpKeywordSetpwent LParen RParen
                                | OpKeywordSetpwent

OpKeywordSetgrentExpr         ::= OpKeywordSetgrent LParen RParen
                                | OpKeywordSetgrent

OpKeywordSethostentExpr       ::= OpKeywordSethostent OpUnaryKeywordArg

OpKeywordSetnetentExpr        ::= OpKeywordSetnetent OpUnaryKeywordArg

OpKeywordSetprotoentExpr      ::= OpKeywordSetprotoent OpUnaryKeywordArg

OpKeywordSetserventExpr       ::= OpKeywordSetservent OpUnaryKeywordArg

OpKeywordEndpwentExpr         ::= OpKeywordEndpwent LParen RParen
                                | OpKeywordEndpwent

OpKeywordEndgrentExpr         ::= OpKeywordEndgrent LParen RParen
                                | OpKeywordEndgrent

OpKeywordEndhostentExpr       ::= OpKeywordEndhostent LParen RParen
                                | OpKeywordEndhostent

OpKeywordEndnetentExpr        ::= OpKeywordEndnetent LParen RParen
                                | OpKeywordEndnetent

OpKeywordEndprotoentExpr      ::= OpKeywordEndprotoent LParen RParen
                                | OpKeywordEndprotoent

OpKeywordEndserventExpr       ::= OpKeywordEndservent LParen RParen
                                | OpKeywordEndservent

OpKeywordExecExpr             ::= OpKeywordExec BlockNonEmpty OpListKeywordArg
                                | OpKeywordExec OpListKeywordArgNonBrace

OpKeywordGetsocknameExpr      ::= OpKeywordGetsockname OpUnaryKeywordArg

OpKeywordGetsockoptExpr       ::= OpKeywordGetsockopt OpListKeywordArg

OpKeywordGlobExpr             ::= OpKeywordGlob OpListKeywordArg
                                | OpKeywordGlob

OpKeywordGmtimeExpr           ::= OpKeywordGmtime OpUnaryKeywordArg
                                | OpKeywordGmtime

# &NAME is an expression too
OpKeywordGotoExpr             ::= OpKeywordGoto OpAssignKeywordArg
                                | OpKeywordGoto Label

OpKeywordGrepExpr             ::= OpKeywordGrep BlockNonEmpty OpListKeywordArg
                                | OpKeywordGrep OpListKeywordArgNonBrace

OpKeywordHexExpr              ::= OpKeywordHex OpUnaryKeywordArg
                                | OpKeywordHex

OpKeywordIndexExpr            ::= OpKeywordIndex OpListKeywordArg

OpKeywordIntExpr              ::= OpKeywordInt OpUnaryKeywordArg
                                | OpKeywordInt

OpKeywordIoctlExpr            ::= OpKeywordIoctl OpListKeywordArg

OpKeywordJoinExpr             ::= OpKeywordJoin OpListKeywordArg

OpKeywordKeysExpr             ::= OpKeywordKeys OpUnaryKeywordArg

OpKeywordKillExpr             ::= OpKeywordKill OpListKeywordArg
                                | OpKeywordKill Expression

OpKeywordLastExpr             ::= OpKeywordLast OpAssignKeywordArg
                                | OpKeywordLast Label
                                | OpKeywordLast

OpKeywordLcExpr               ::= OpKeywordLc OpUnaryKeywordArg
                                | OpKeywordLc

OpKeywordLcfirstExpr          ::= OpKeywordLcfirst OpUnaryKeywordArg
                                | OpKeywordLcfirst

OpKeywordLengthExpr           ::= OpKeywordLength OpUnaryKeywordArg
                                | OpKeywordLength

OpKeywordLinkExpr             ::= OpKeywordLink OpListKeywordArg

OpKeywordListenExpr           ::= OpKeywordListen OpListKeywordArg

OpKeywordLocaltimeExpr        ::= OpKeywordLocaltime OpUnaryKeywordArg
                                | OpKeywordLocaltime

OpKeywordLockExpr             ::= OpKeywordLock OpUnaryKeywordArg

OpKeywordLogExpr              ::= OpKeywordLog OpUnaryKeywordArg
                                | OpKeywordLog

OpKeywordLstatExpr            ::= OpKeywordLstat OpUnaryKeywordArg
                                | OpKeywordLstat

OpKeywordMapExpr              ::= OpKeywordMap BlockNonEmpty OpListKeywordArg
                                | OpKeywordMap OpListKeywordArgNonBrace

OpKeywordMkdirExpr            ::= OpKeywordMkdir OpListKeywordArg
                                | OpKeywordMkdir

OpKeywordMsgctlExpr           ::= OpKeywordMsgctl OpListKeywordArg

OpKeywordMsggetExpr           ::= OpKeywordMsgget OpListKeywordArg

OpKeywordMsgrcvExpr           ::= OpKeywordMsgrcv OpListKeywordArg

OpKeywordMsgsndExpr           ::= OpKeywordMsgsnd OpListKeywordArg

OpKeywordNextExpr             ::= OpKeywordNext OpAssignKeywordArg
                                | OpKeywordNext Label
                                | OpKeywordNext

OpKeywordOctExpr              ::= OpKeywordOct OpUnaryKeywordArg
                                | OpKeywordOct

OpKeywordOpenExpr             ::= OpKeywordOpen OpListKeywordArg

OpKeywordOpendirExpr          ::= OpKeywordOpendir OpListKeywordArg

OpKeywordOrdExpr              ::= OpKeywordOrd OpUnaryKeywordArg
                                | OpKeywordOrd

OpKeywordPackExpr             ::= OpKeywordPack OpListKeywordArg

OpKeywordPipeExpr             ::= OpKeywordPipe OpListKeywordArg

OpKeywordPopExpr              ::= OpKeywordPop OpUnaryKeywordArg
                                | OpKeywordPop

OpKeywordPosExpr              ::= OpKeywordPos OpUnaryKeywordArg
                                | OpKeywordPos

OpKeywordPrintExpr            ::= OpKeywordPrint BlockNonEmpty OpListKeywordArg
                                | OpKeywordPrint BuiltinFilehandle OpListKeywordArgNonBrace
                                | OpKeywordPrint BuiltinFilehandle
                                | OpKeywordPrint OpListKeywordArgNonBrace
                                | OpKeywordPrint BlockNonEmpty
                                | OpKeywordPrint

OpKeywordPrintfExpr           ::= OpKeywordPrintf BlockNonEmpty OpListKeywordArg
                                | OpKeywordPrintf BuiltinFilehandle OpListKeywordArgNonBrace
                                | OpKeywordPrintf BuiltinFilehandle
                                | OpKeywordPrintf OpListKeywordArgNonBrace
                                | OpKeywordPrintf BlockNonEmpty

OpKeywordPrototypeExpr        ::= OpKeywordPrototype OpUnaryKeywordArg
                                | OpKeywordPrototype

OpKeywordPushExpr             ::= OpKeywordPush OpListKeywordArg

OpKeywordQuotemetaExpr        ::= OpKeywordQuotemeta OpUnaryKeywordArg
                                | OpKeywordQuotemeta

OpKeywordRandExpr             ::= OpKeywordRand OpUnaryKeywordArg
                                | OpKeywordRand

OpKeywordReadExpr             ::= OpKeywordRead OpListKeywordArg

OpKeywordReaddirExpr          ::= OpKeywordReaddir OpUnaryKeywordArg

OpKeywordReadlineExpr         ::= OpKeywordReadline OpUnaryKeywordArg
                                | OpKeywordReadline

OpKeywordReadlinkExpr         ::= OpKeywordReadlink OpUnaryKeywordArg
                                | OpKeywordReadlink

OpKeywordReadpipeExpr         ::= OpKeywordReadpipe OpUnaryKeywordArg
                                | OpKeywordReadpipe

OpKeywordRecvExpr             ::= OpKeywordRecv OpListKeywordArg

OpKeywordRedoExpr             ::= OpKeywordRedo OpAssignKeywordArg
                                | OpKeywordRedo Label
                                | OpKeywordRedo

OpKeywordRefExpr              ::= OpKeywordRef OpUnaryKeywordArg
                                | OpKeywordRef

OpKeywordRenameExpr           ::= OpKeywordRename OpListKeywordArg

OpKeywordResetExpr            ::= OpKeywordReset OpUnaryKeywordArg
                                | OpKeywordReset

OpKeywordReturnExpr           ::= OpKeywordReturn OpListKeywordArg
                                | OpKeywordReturn

OpKeywordReverseExpr          ::= OpKeywordReverse OpListKeywordArg

OpKeywordRewinddirExpr        ::= OpKeywordRewinddir OpUnaryKeywordArg
                                | OpKeywordRewinddir

OpKeywordRindexExpr           ::= OpKeywordRindex OpListKeywordArg
                                | OpKeywordRindex

OpKeywordRmdirExpr            ::= OpKeywordRmdir OpUnaryKeywordArg
                                | OpKeywordRmdir

OpKeywordSayExpr              ::= OpKeywordSay BlockNonEmpty OpListKeywordArg
                                | OpKeywordSay BuiltinFilehandle OpListKeywordArgNonBrace
                                | OpKeywordSay BuiltinFilehandle
                                | OpKeywordSay OpListKeywordArgNonBrace
                                | OpKeywordSay BlockNonEmpty
                                | OpKeywordSay

OpKeywordScalarExpr           ::= OpKeywordScalar OpUnaryKeywordArg

OpKeywordSeekExpr             ::= OpKeywordSeek OpListKeywordArg

OpKeywordSeekdirExpr          ::= OpKeywordSeekdir OpListKeywordArg

OpKeywordSelectExpr           ::= OpKeywordSelect OpListKeywordArg

OpKeywordSemctlExpr           ::= OpKeywordSemctl OpListKeywordArg

OpKeywordSemgetExpr           ::= OpKeywordSemget OpListKeywordArg

OpKeywordSemopExpr            ::= OpKeywordSemop OpListKeywordArg

OpKeywordSendExpr             ::= OpKeywordSend OpListKeywordArg

OpKeywordSetpgrpExpr          ::= OpKeywordSetpgrp OpListKeywordArg

OpKeywordSetpriorityExpr      ::= OpKeywordSetpriority OpListKeywordArg

OpKeywordSetsockoptExpr       ::= OpKeywordSetsockopt OpListKeywordArg

OpKeywordShiftExpr            ::= OpKeywordShift OpUnaryKeywordArg
                                | OpKeywordShift

OpKeywordShmctlExpr           ::= OpKeywordShmctl OpListKeywordArg

OpKeywordShmgetExpr           ::= OpKeywordShmget OpListKeywordArg

OpKeywordShmreadExpr          ::= OpKeywordShmread OpListKeywordArg

OpKeywordShmwriteExpr         ::= OpKeywordShmwrite OpListKeywordArg

OpKeywordShutdownExpr         ::= OpKeywordShutdown OpListKeywordArg

OpKeywordSinExpr              ::= OpKeywordSin OpUnaryKeywordArg
                                | OpKeywordSin

OpKeywordSleepExpr            ::= OpKeywordSleep OpUnaryKeywordArg
                                | OpKeywordSleep

OpKeywordSocketExpr           ::= OpKeywordSocket OpListKeywordArg

OpKeywordSocketpairExpr       ::= OpKeywordSocketpair OpListKeywordArg

OpKeywordSortExpr             ::= OpKeywordSort BlockNonEmpty OpListKeywordArg
                                | OpKeywordSort VarScalar OpListKeywordArg
                                | OpKeywordSort OpListKeywordArgNonBrace

OpKeywordSpliceExpr           ::= OpKeywordSplice OpListKeywordArg

OpKeywordSplitExpr            ::= OpKeywordSplit OpListKeywordArg

OpKeywordSprintfExpr          ::= OpKeywordSprintf OpListKeywordArg

OpKeywordSqrtExpr             ::= OpKeywordSqrt OpUnaryKeywordArg
                                | OpKeywordSqrt

OpKeywordSrandExpr            ::= OpKeywordSrand OpUnaryKeywordArg
                                | OpKeywordSrand

OpKeywordStatExpr             ::= OpKeywordStat OpUnaryKeywordArg
                                | OpKeywordStat

OpKeywordStudyExpr            ::= OpKeywordStudy OpUnaryKeywordArg
                                | OpKeywordStudy

OpKeywordSubExpr              ::= OpKeywordSub SubDefinition

OpKeywordSubstrExpr           ::= OpKeywordSubstr OpListKeywordArg

OpKeywordSymlinkExpr          ::= OpKeywordSymlink OpListKeywordArg

OpKeywordSyscallExpr          ::= OpKeywordSyscall OpListKeywordArg

OpKeywordSysopenExpr          ::= OpKeywordSysopen OpListKeywordArg

OpKeywordSysreadExpr          ::= OpKeywordSysread OpListKeywordArg

OpKeywordSysseekExpr          ::= OpKeywordSysseek OpListKeywordArg

OpKeywordSyswriteExpr         ::= OpKeywordSyswrite OpListKeywordArg

OpKeywordSystemExpr           ::= OpKeywordSystem BlockNonEmpty OpListKeywordArg
                                | OpKeywordSystem OpListKeywordArgNonBrace

OpKeywordTellExpr             ::= OpKeywordTell OpUnaryKeywordArg
                                | OpKeywordTell

OpKeywordTelldirExpr          ::= OpKeywordTelldir OpUnaryKeywordArg

OpKeywordTieExpr              ::= OpKeywordTie OpListKeywordArg

OpKeywordTiedExpr             ::= OpKeywordTied OpUnaryKeywordArg

OpKeywordTimeExpr             ::= OpKeywordTime LParen RParen
                                | OpKeywordTime

OpKeywordTimesExpr            ::= OpKeywordTimes LParen RParen
                                | OpKeywordTimes

OpKeywordTruncateExpr         ::= OpKeywordTruncate OpListKeywordArg

OpKeywordUcExpr               ::= OpKeywordUc OpUnaryKeywordArg
                                | OpKeywordUc

OpKeywordUcfirstExpr          ::= OpKeywordUcfirst OpUnaryKeywordArg
                                | OpKeywordUcfirst

OpKeywordUmaskExpr            ::= OpKeywordUmask OpUnaryKeywordArg
                                | OpKeywordUmask

OpKeywordUndefExpr            ::= OpKeywordUndef OpUnaryKeywordArg
                                | OpKeywordUndef

OpKeywordUnlinkExpr           ::= OpKeywordUnlink OpUnaryKeywordArg
                                | OpKeywordUnlink

OpKeywordUnpackExpr           ::= OpKeywordUnpack OpListKeywordArg

OpKeywordUnshiftExpr          ::= OpKeywordUnshift OpListKeywordArg

OpKeywordUntieExpr            ::= OpKeywordUntie OpUnaryKeywordArg

OpKeywordUtimeExpr            ::= OpKeywordUtime OpUnaryKeywordArg

OpKeywordValuesExpr           ::= OpKeywordValues OpUnaryKeywordArg

OpKeywordVecExpr              ::= OpKeywordVec OpListKeywordArg

OpKeywordWaitExpr             ::= OpKeywordWait LParen RParen
                                | OpKeywordWait

OpKeywordWaitpidExpr          ::= OpKeywordWaitpid OpListKeywordArg

OpKeywordWantarrayExpr        ::= OpKeywordWantarray LParen RParen
                                | OpKeywordWantarray

OpKeywordWarnExpr             ::= OpKeywordWarn OpListKeywordArg
                                | OpKeywordWarn

OpKeywordWriteExpr            ::= OpKeywordWrite OpListKeywordArg
                                | OpKeywordWrite

OpFile ::=
      OpFileReadableEffective
    | OpFileWritableEffective
    | OpFileRExecutableEffective
    | OpFileOwnedEffective
    | OpFileReadableReal
    | OpFileWritableReal
    | OpFileRExecutableReal
    | OpFileOwnedReal
    | OpFileExists
    | OpFileEmpty
    | OpFileNonEmpty
    | OpFilePlain
    | OpFileDirectory
    | OpFileSymbolic
    | OpFileNamedPipe
    | OpFileSocket
    | OpFileBlock
    | OpFileCharacter
    | OpFileOpenedTty
    | OpFileSetuid
    | OpFileSetgid
    | OpFileSticky
    | OpFileAsciiUtf8
    | OpFileBinary
    | OpFileStartTime
    | OpFileAccessTime
    | OpFileChangeTime

OpFileExpr ::= OpFile OpFileArg
OpFileArg  ::= OpUnaryKeywordArg
             | BuiltinFilehandle

QLikeValue ::= QLikeValueExpr | QLikeValueExprWithMods

QLikeValueExpr
    ~ QLikeFunction '(' NonRParenOrEscapedParens_Any               ')'
    | QLikeFunction '{' NonRBraceOrEscapedBraces_Any               '}'
    | QLikeFunction '<' NonRAngleOrEscapedAngles_Any               '>'
    | QLikeFunction '[' NonRBracketOrEscapedBrackets_Any           ']'
    | QLikeFunction '/' NonForwardSlashOrEscapedForwardSlashes_Any '/'
    | QLikeFunction '!' NonExclamPointOrEscapedExclamPoints_Any    '!'
    | QLikeFunction '|' NonPipeOrEscapedPipes_Any                  '|'

QLikeFunction ~ OpKeywordQ
              | OpKeywordQq
              | OpKeywordQx
              | OpKeywordQw

# Here we begin with "qr//" and "m//" which can have parameters
# Then we continue with "s///", "tr///", and "y///" which have two args, not one
# "//" follow at the end
QLikeValueExprWithMods
    ~ QLikeFunctionWithMods '(' NonRParenOrEscapedParens_Any               ')' RegexModifiers
    | QLikeFunctionWithMods '{' NonRBraceOrEscapedBraces_Any               '}' RegexModifiers
    | QLikeFunctionWithMods '<' NonRAngleOrEscapedAngles_Any               '>' RegexModifiers
    | QLikeFunctionWithMods '[' NonRBracketOrEscapedBrackets_Any           ']' RegexModifiers
    | QLikeFunctionWithMods '/' NonForwardSlashOrEscapedForwardSlashes_Any '/' RegexModifiers
    | QLikeFunctionWithMods '!' NonExclamPointOrEscapedExclamPoints_Any    '!' RegexModifiers
    | QLikeFunctionWithMods '|' NonPipeOrEscapedPipes_Any                  '|' RegexModifiers
    | QLikeSubstWithMods    '(' NonRParenOrEscapedParens_Any               ')(' NonRParenOrEscapedParens_Any               ')' RegexModifiers
    | QLikeSubstWithMods    '{' NonRBraceOrEscapedBraces_Any               '}{' NonRBraceOrEscapedBraces_Any               '}' RegexModifiers
    | QLikeSubstWithMods    '<' NonRAngleOrEscapedAngles_Any               '><' NonRAngleOrEscapedAngles_Any               '>' RegexModifiers
    | QLikeSubstWithMods    '[' NonRBracketOrEscapedBrackets_Any           '][' NonRBracketOrEscapedBrackets_Any           ']' RegexModifiers
    | QLikeSubstWithMods    '/' NonForwardSlashOrEscapedForwardSlashes_Any '/'  NonForwardSlashOrEscapedForwardSlashes_Any '/' RegexModifiers
    | QLikeSubstWithMods    '!' NonExclamPointOrEscapedExclamPoints_Any    '!'  NonExclamPointOrEscapedExclamPoints_Any    '!' RegexModifiers
    | QLikeSubstWithMods    '|' NonPipeOrEscapedPipes_Any                  '|'  NonPipeOrEscapedPipes_Any                  '|' RegexModifiers
    | '/' NonForwardSlashOrEscapedForwardSlashes_Any '/' RegexModifiers
    | '`' NonBacktickOrEscapedBackticks_Any '`'

QLikeFunctionWithMods ~ OpKeywordQr
                      | OpKeywordM

QLikeSubstWithMods ~ OpKeywordS
                   | OpKeywordTr
                   | OpKeywordY

RegexModifiers ~ [a-z]*

###

# Everything except: # q* / s / m / y / t*
#                         |       |   | |         |
# a b c d e f g h i j k l m n o p q r s t u v w x y z
# a -                   l   n - p   r     u -   x   z
# (Cannot begin with digit, so digits are out)
NonQLikeLetters ~ [a-ln-pru-xzA-Z_]+

# Everything except: q / w / r / x (qq, qw, qr, qx)
#                                 | |         | |
# a b c d e f g h i j k l m n o p q r s t u v w x y z
# a -                           p     s -   v     y-z
# (digits also allowed at this point)
NonQRWXLetters ~ [a-ps-vy-zA-Z0-9_]+

# Everything except: r (tr)
#                                   |
# a b c d e f g h i j k l m n o p q r s t u v w x y z
# a -                             q   s -           z
# (digits also allowed at this point)
NonRLetter ~ [a-qs-zA-Z0-9_]

# Everything else allowed (including digits)
AllSubLetters ~ [a-zA-Z0-9_]+

IdentComp  ~ [a-zA-Z_0-9]+
PackageSep ~ '::'

VersionExpr           ::= VersionNumber
VersionNumber         ~ VersionNumberSegments
                      | 'v' VersionNumberSegments

VersionNumberSegments ~ VersionNumberSegment '.' VersionNumberSegment '.' VersionNumberSegment
                      | VersionNumberSegment '.' VersionNumberSegment
                      | VersionNumberSegment

VersionNumberSegment ~ [0-9] [0-9] [0-9]
                     | [0-9] [0-9]
                     | [0-9]

LitNumber ::= LitNumberDec
            | LitNumberOct
            | LitNumberHex
            | LitNumberBin

LitNumberDec ::= NumberDec
LitNumberOct ::= NumberOct
LitNumberHex ::= NumberHex
LitNumberBin ::= NumberBin

NumberDec ~ NumberDecInt
          | NumberDecInt ExpDec
          | NumberDecInt '.' DigitsDec
          | NumberDecInt '.' DigitsDec ExpDec
          | '.' DigitDec DigitsDec
          | '.' DigitDec DigitsDec ExpDec

NumberDecInt ~ [1-9] DigitsDec
             | '0'

NumberOct ~ '0' Underbars DigitOct DigitsOct
          | '0' Underbars DigitOct DigitsOct ExpHex
          | '0' DigitsOct '.' DigitOct DigitsOct ExpHex

NumberHex ~ '0' [xX] Underbars DigitHex DigitsHex
          | '0' [xX] Underbars DigitHex DigitsHex ExpHex
          | '0' [xX] DigitsHex '.' DigitHex DigitsHex ExpHex

NumberBin ~ '0' [bB] Underbars DigitBin DigitsBin
          | '0' [bB] Underbars DigitBin DigitsBin ExpHex
          | '0' [bB] DigitsBin '.' DigitBin DigitsBin ExpHex

ExpDec ~ [eE] [+-] ExpDecExp
       | [eE] [_] [+-] ExpDecExp
       | [eE] ExpDecExp

ExpDecExp ~ Underbars DigitDec DigitsDec
Underbars ~ [_]*

ExpHex ~ [pP] [+-] DigitDec DigitsDec
       | [pP] DigitDec DigitsDec

DigitDec    ~ [0-9]
DigitsDec   ~ [0-9_]*

DigitOct    ~ [0-7]
DigitsOct   ~ [0-7_]*
DigitHex    ~ [0-9a-fA-F]
DigitsHex   ~ [0-9a-fA-F_]*

DigitBin    ~ [01]
DigitsBin   ~ [01_]*

Digits      ~ [0-9]+
SingleQuote ~ [']
DoubleQuote ~ ["]

NonDoubleOrEscapedQuote_Many ~ NonDoubleOrEscapedQuote+
NonDoubleOrEscapedQuote      ~ EscapedDoubleQuote | NonDoubleQuote
EscapedDoubleQuote           ~ Escape ["]
NonDoubleQuote               ~ [^"]

NonSingleOrEscapedQuote_Many ~ NonSingleOrEscapedQuote+
NonSingleOrEscapedQuote      ~ EscapedSingleQuote | NonSingleQuote
EscapedSingleQuote           ~ Escape [']
NonSingleQuote               ~ [^']

Colon     ~ ':'
Semicolon ~ ';'
Escape    ~ '\'

SigilScalar   ~ '$'
SigilArray    ~ '@'
SigilHash     ~ '%'
SigilCode     ~ '&'
SigilGlob     ~ '*'
SigilArrayTop ~ '$#'

LParen   ~ '('
RParen   ~ ')'
LBracket ~ '['
RBracket ~ ']'
LBrace   ~ '{'
RBrace   ~ '}'

NonRParenOrEscapedParens_Any ~ NonRParenOrEscapedParens*
NonRParenOrEscapedParens     ~ EscapedParens | NonRParen
EscapedParens                ~ EscapedLParen | EscapedRParen
EscapedLParen                ~ Escape [(]
EscapedRParen                ~ Escape [)]
NonRParen                    ~ [^)]

NonRBracketOrEscapedBrackets_Any ~ NonRBracketOrEscapedBrackets*
NonRBracketOrEscapedBrackets     ~ EscapedBrackets | NonRBracket
EscapedBrackets                  ~ EscapedLBracket | EscapedRBracket
EscapedLBracket                  ~ Escape [\[]
EscapedRBracket                  ~ Escape [\]]
NonRBracket                      ~ [^\]]

NonRBraceOrEscapedBraces_Any ~ NonRBraceOrEscapedBraces*
NonRBraceOrEscapedBraces     ~ EscapedBraces | NonRBrace
EscapedBraces                ~ EscapedLBrace | EscapedRBrace
EscapedLBrace                ~ Escape [\{]
EscapedRBrace                ~ Escape [\}]
NonRBrace                    ~ [^\}]

NonRAngleOrEscapedAngles_Any ~ NonRAngleOrEscapedAngles*
NonRAngleOrEscapedAngles     ~ EscapedAngles | NonRAngle
EscapedAngles                ~ EscapedLAngle | EscapedRAngle
EscapedLAngle                ~ Escape [<]
EscapedRAngle                ~ Escape [>]
NonRAngle                    ~ [^>]

NonForwardSlashOrEscapedForwardSlashes_Any ~ NonForwardSlashOrEscapedForwardSlashes*
NonForwardSlashOrEscapedForwardSlashes     ~ EscapedForwardSlash | NonForwardSlash
EscapedForwardSlash                        ~ Escape [/]
NonForwardSlash                            ~ [^\/]

NonExclamPointOrEscapedExclamPoints_Any ~ NonExclamPointOrEscapedExclamPoints*
NonExclamPointOrEscapedExclamPoints     ~ EscapedExclamPoint | NonExclamPoint
EscapedExclamPoint                      ~ Escape [!]
NonExclamPoint                          ~ [^\!]

NonPipeOrEscapedPipes_Any ~ NonPipeOrEscapedPipes*
NonPipeOrEscapedPipes     ~ EscapedPipe | NonPipe
EscapedPipe               ~ Escape [\|]
NonPipe                   ~ [^\|]

NonBacktickOrEscapedBackticks_Any ~ NonBacktickOrEscapedBackticks*
NonBacktickOrEscapedBackticks     ~ EscapedBacktick | NonBacktick
EscapedBacktick                   ~ Escape [`]
NonBacktick                       ~ [^\`]

Ellipsis ~ '...'

UnderscorePackage ~ '__PACKAGE__'
UnderscoreFile    ~ '__FILE__'
UnderscoreLine    ~ '__LINE__'
UnderscoreSub     ~ '__SUB__'
#UnderscoreData    ~ '__DATA__'
#UnderscoreEnd     ~ '__END__'

PhaseName ~ 'BEGIN' | 'CHECK' | 'INIT' | 'UNITCHECK' | 'END'

SubAttrArgs ~ '(' NonRParenOrEscapedParens_Any ')'

OpArrow   ~ '->'
OpInc     ~ '++' | '--'
OpPower   ~ '**'
OpUnary   ~ '!' | '~' | '\' | '+' | '-'
OpRegex   ~ '=~' | '!~'
OpMulti   ~ '*' | '/' | '%' | 'x'
OpAdd     ~ '+' | '-' | '.'
OpShift   ~ '<<' | '>>'
OpInequal ~ '<' | '>' | '<=' | '>=' | 'lt' | 'gt' | 'le' | 'ge'
OpEqual   ~ '==' | '!=' | '<=>' | 'eq' | 'ne' | 'cmp'
OpBinAnd  ~ '&'
OpBinOr   ~ '|' | '^'
OpLogAnd  ~ '&&'
OpLogOr   ~ '||' | '//'
OpRange   ~ '..' | '...'
OpTriThen ~ '?'
OpTriElse ~ ':'
OpAssign  ~ '=' | '*=' | '/=' | '%=' | 'x=' | '+=' | '-=' | '.=' | '<<=' | '>>=' | '&=' | '|=' | '^=' | '&&=' | '||=' | '//='
OpComma   ~ ',' | '=>'
OpNameNot ~ 'not'
OpNameAnd ~ 'and'
OpNameOr  ~ 'or' | 'xor'

OpKeywordAbs              ~ 'abs'
OpKeywordAccept           ~ 'accept'
OpKeywordAlarm            ~ 'alarm'
OpKeywordAtan2            ~ 'atan2'
OpKeywordBind             ~ 'bind'
OpKeywordBinmode          ~ 'binmode'
OpKeywordBless            ~ 'bless'
OpKeywordBreak            ~ 'break'
OpKeywordCaller           ~ 'caller'
OpKeywordChdir            ~ 'chdir'
OpKeywordChmod            ~ 'chmod'
OpKeywordChomp            ~ 'chomp'
OpKeywordChop             ~ 'chop'
OpKeywordChown            ~ 'chown'
OpKeywordChr              ~ 'chr'
OpKeywordChroot           ~ 'chroot'
OpKeywordClose            ~ 'close'
OpKeywordClosedir         ~ 'closedir'
OpKeywordConnect          ~ 'connect'
OpKeywordContinue         ~ 'continue'
OpKeywordCos              ~ 'cos'
OpKeywordCrypt            ~ 'crypt'
OpKeywordDbmclose         ~ 'dbmclose'
OpKeywordDbmopen          ~ 'dbmopen'
OpKeywordDefined          ~ 'defined'
OpKeywordDelete           ~ 'delete'
OpKeywordDie              ~ 'die'
OpKeywordDo               ~ 'do'
OpKeywordDump             ~ 'dump'
OpKeywordEach             ~ 'each'
OpKeywordEof              ~ 'eof'
OpKeywordEval             ~ 'eval'
OpKeywordEvalbytes        ~ 'evalbytes'
OpKeywordExec             ~ 'exec'
OpKeywordExists           ~ 'exists'
OpKeywordExit             ~ 'exit'
OpKeywordExp              ~ 'exp'
OpKeywordFc               ~ 'fc'
OpKeywordFor              ~ 'for'
OpKeywordForeach          ~ 'foreach'
OpKeywordFcntl            ~ 'fcntl'
OpKeywordFileno           ~ 'fileno'
OpKeywordFlock            ~ 'flock'
OpKeywordFork             ~ 'fork'
OpKeywordGetc             ~ 'getc'
OpKeywordGetlogin         ~ 'getlogin'
OpKeywordGetpeername      ~ 'getpeername'
OpKeywordGetpgrp          ~ 'getpgrp'
OpKeywordGetppid          ~ 'getppid'
OpKeywordGetpriority      ~ 'getpriority'
OpKeywordGetpwnam         ~ 'getpwnam'
OpKeywordGetgrnam         ~ 'getgrnam'
OpKeywordGethostbyname    ~ 'gethostbyname'
OpKeywordGetnetbyname     ~ 'getnetbyname'
OpKeywordGetprotobyname   ~ 'getprotobyname'
OpKeywordGetpwuid         ~ 'getpwuid'
OpKeywordGetgrgid         ~ 'getgrgid'
OpKeywordGetservbyname    ~ 'getservbyname'
OpKeywordGethostbyaddr    ~ 'gethostbyaddr'
OpKeywordGetnetbyaddr     ~ 'getnetbyaddr'
OpKeywordGetprotobynumber ~ 'getprotobynumber'
OpKeywordGetservbyport    ~ 'getservbyport'
OpKeywordGetpwent         ~ 'getpwent'
OpKeywordGetgrent         ~ 'getgrent'
OpKeywordGethostent       ~ 'gethostent'
OpKeywordGetnetent        ~ 'getnetent'
OpKeywordGetprotoent      ~ 'getprotoent'
OpKeywordGetservent       ~ 'getservent'
OpKeywordSetpwent         ~ 'setpwent'
OpKeywordSetgrent         ~ 'setgrent'
OpKeywordSethostent       ~ 'sethostent'
OpKeywordSetnetent        ~ 'setnetent'
OpKeywordSetprotoent      ~ 'setprotoent'
OpKeywordSetservent       ~ 'setservent'
OpKeywordEndpwent         ~ 'endpwent'
OpKeywordEndgrent         ~ 'endgrent'
OpKeywordEndhostent       ~ 'endhostent'
OpKeywordEndnetent        ~ 'endnetent'
OpKeywordEndprotoent      ~ 'endprotoent'
OpKeywordEndservent       ~ 'endservent'
OpKeywordGetsockname      ~ 'getsockname'
OpKeywordGetsockopt       ~ 'getsockopt'
OpKeywordGlob             ~ 'glob'
OpKeywordGmtime           ~ 'gmtime'
OpKeywordGoto             ~ 'goto'
OpKeywordGrep             ~ 'grep'
OpKeywordHex              ~ 'hex'
OpKeywordIndex            ~ 'index'
OpKeywordInt              ~ 'int'
OpKeywordIoctl            ~ 'ioctl'
OpKeywordJoin             ~ 'join'
OpKeywordKeys             ~ 'keys'
OpKeywordKill             ~ 'kill'
OpKeywordLast             ~ 'last'
OpKeywordLc               ~ 'lc'
OpKeywordLcfirst          ~ 'lcfirst'
OpKeywordLength           ~ 'length'
OpKeywordLink             ~ 'link'
OpKeywordListen           ~ 'listen'
OpKeywordLocal            ~ 'local'
OpKeywordLocaltime        ~ 'localtime'
OpKeywordLock             ~ 'lock'
OpKeywordLog              ~ 'log'
OpKeywordLstat            ~ 'lstat'
OpKeywordM                ~ 'm'
OpKeywordMap              ~ 'map'
OpKeywordMkdir            ~ 'mkdir'
OpKeywordMsgctl           ~ 'msgctl'
OpKeywordMsgget           ~ 'msgget'
OpKeywordMsgrcv           ~ 'msgrcv'
OpKeywordMsgsnd           ~ 'msgsnd'
OpKeywordMy               ~ 'my'
OpKeywordNext             ~ 'next'
OpKeywordNo               ~ 'no'
OpKeywordOct              ~ 'oct'
OpKeywordOpen             ~ 'open'
OpKeywordOpendir          ~ 'opendir'
OpKeywordOrd              ~ 'ord'
OpKeywordOur              ~ 'our'
OpKeywordPack             ~ 'pack'
OpKeywordPackage          ~ 'package'
OpKeywordPipe             ~ 'pipe'
OpKeywordPop              ~ 'pop'
OpKeywordPos              ~ 'pos'
OpKeywordPrint            ~ 'print'
OpKeywordPrintf           ~ 'printf'
OpKeywordPrototype        ~ 'prototype'
OpKeywordPush             ~ 'push'
OpKeywordQ                ~ 'q'
OpKeywordQq               ~ 'qq'
OpKeywordQx               ~ 'qx'
OpKeywordQw               ~ 'qw'
OpKeywordQr               ~ 'qr'
OpKeywordQuotemeta        ~ 'quotemeta'
OpKeywordRand             ~ 'rand'
OpKeywordRead             ~ 'read'
OpKeywordReaddir          ~ 'readdir'
OpKeywordReadline         ~ 'readline'
OpKeywordReadlink         ~ 'readlink'
OpKeywordReadpipe         ~ 'readpipe'
OpKeywordRecv             ~ 'recv'
OpKeywordRedo             ~ 'redo'
OpKeywordRef              ~ 'ref'
OpKeywordRename           ~ 'rename'
OpKeywordRequire          ~ 'require'
OpKeywordReset            ~ 'reset'
OpKeywordReturn           ~ 'return'
OpKeywordReverse          ~ 'reverse'
OpKeywordRewinddir        ~ 'rewinddir'
OpKeywordRindex           ~ 'rindex'
OpKeywordRmdir            ~ 'rmdir'
OpKeywordS                ~ 's'
OpKeywordSay              ~ 'say'
OpKeywordScalar           ~ 'scalar'
OpKeywordSeek             ~ 'seek'
OpKeywordSeekdir          ~ 'seekdir'
OpKeywordSelect           ~ 'select'
OpKeywordSemctl           ~ 'semctl'
OpKeywordSemget           ~ 'semget'
OpKeywordSemop            ~ 'semop'
OpKeywordSend             ~ 'send'
OpKeywordSetpgrp          ~ 'setpgrp'
OpKeywordSetpriority      ~ 'setpriority'
OpKeywordSetsockopt       ~ 'setsockopt'
OpKeywordShift            ~ 'shift'
OpKeywordShmctl           ~ 'shmctl'
OpKeywordShmget           ~ 'shmget'
OpKeywordShmread          ~ 'shmread'
OpKeywordShmwrite         ~ 'shmwrite'
OpKeywordShutdown         ~ 'shutdown'
OpKeywordSin              ~ 'sin'
OpKeywordSleep            ~ 'sleep'
OpKeywordSocket           ~ 'socket'
OpKeywordSocketpair       ~ 'socketpair'
OpKeywordSort             ~ 'sort'
OpKeywordSplice           ~ 'splice'
OpKeywordSplit            ~ 'split'
OpKeywordSprintf          ~ 'sprintf'
OpKeywordSqrt             ~ 'sqrt'
OpKeywordSrand            ~ 'srand'
OpKeywordStat             ~ 'stat'
OpKeywordState            ~ 'state'
OpKeywordStudy            ~ 'study'
OpKeywordSub              ~ 'sub'
OpKeywordSubstr           ~ 'substr'
OpKeywordSymlink          ~ 'symlink'
OpKeywordSyscall          ~ 'syscall'
OpKeywordSysopen          ~ 'sysopen'
OpKeywordSysread          ~ 'sysread'
OpKeywordSysseek          ~ 'sysseek'
OpKeywordSystem           ~ 'system'
OpKeywordSyswrite         ~ 'syswrite'
OpKeywordTr               ~ 'tr'
OpKeywordTell             ~ 'tell'
OpKeywordTelldir          ~ 'telldir'
OpKeywordTie              ~ 'tie'
OpKeywordTied             ~ 'tied'
OpKeywordTime             ~ 'time'
OpKeywordTimes            ~ 'times'
OpKeywordTruncate         ~ 'truncate'
OpKeywordUc               ~ 'uc'
OpKeywordUcfirst          ~ 'ucfirst'
OpKeywordUmask            ~ 'umask'
OpKeywordUndef            ~ 'undef'
OpKeywordUnlink           ~ 'unlink'
OpKeywordUnpack           ~ 'unpack'
OpKeywordUnshift          ~ 'unshift'
OpKeywordUntie            ~ 'untie'
OpKeywordUse              ~ 'use'
OpKeywordUtime            ~ 'utime'
OpKeywordValues           ~ 'values'
OpKeywordVec              ~ 'vec'
OpKeywordWait             ~ 'wait'
OpKeywordWaitpid          ~ 'waitpid'
OpKeywordWantarray        ~ 'wantarray'
OpKeywordWarn             ~ 'warn'
OpKeywordWrite            ~ 'write'
OpKeywordY                ~ 'y'

OpFileReadableEffective     ~ '-r'
OpFileWritableEffective     ~ '-w'
OpFileRExecutableEffective  ~ '-x'
OpFileOwnedEffective        ~ '-o'
OpFileReadableReal          ~ '-R'
OpFileWritableReal          ~ '-W'
OpFileRExecutableReal       ~ '-X'
OpFileOwnedReal             ~ '-O'
OpFileExists                ~ '-e'
OpFileEmpty                 ~ '-z'
OpFileNonEmpty              ~ '-s'
OpFilePlain                 ~ '-f'
OpFileDirectory             ~ '-d'
OpFileSymbolic              ~ '-l'
OpFileNamedPipe             ~ '-p'
OpFileSocket                ~ '-S'
OpFileBlock                 ~ '-b'
OpFileCharacter             ~ '-c'
OpFileOpenedTty             ~ '-t'
OpFileSetuid                ~ '-u'
OpFileSetgid                ~ '-g'
OpFileSticky                ~ '-k'
OpFileAsciiUtf8             ~ '-T'
OpFileBinary                ~ '-B'
OpFileStartTime             ~ '-M'
OpFileAccessTime            ~ '-A'
OpFileChangeTime            ~ '-C'

ConditionIf      ~ 'if'
ConditionElsif   ~ 'elsif'
ConditionElse    ~ 'else'
ConditionUnless  ~ 'unless'
ConditionWhile   ~ 'while'
ConditionUntil   ~ 'until'
ConditionFor     ~ 'for'
ConditionForeach ~ 'foreach'

BuiltinFilehandle ~ 'STDIN' | 'STDOUT' | 'STDERR' | 'ARGV' | 'ARGVOUT' | 'DATA'

# Ignore whitespace
:discard ~ whitespace
whitespace ~ [\s]+

# Comments
:discard ~ <hash comment>
<hash comment>                    ~ <terminated hash comment> | <unterminated final hash comment>
<terminated hash comment>         ~ '#' <hash comment body> <vertical space char>
<unterminated final hash comment> ~ '#' <hash comment body>
<hash comment body>               ~ <hash comment char>*
<vertical space char>             ~ [\x{A}\x{B}\x{C}\x{D}\x{2028}\x{2029}]
<hash comment char>               ~ [^\x{A}\x{B}\x{C}\x{D}\x{2028}\x{2029}]

# Recognize phases before subroutines
:lexeme ~ PhaseName              priority => 1

# Recognize q* / s / m / y / tr before subroutines
:lexeme ~ QLikeValueExpr         priority => 1
:lexeme ~ QLikeValueExprWithMods priority => 1

:lexeme ~ VersionNumber          priority => 1

# Prioritize keywords over functions
:lexeme ~ OpKeywordAbs              priority => 1
:lexeme ~ OpKeywordAccept           priority => 1
:lexeme ~ OpKeywordAlarm            priority => 1
:lexeme ~ OpKeywordAtan2            priority => 1
:lexeme ~ OpKeywordBind             priority => 1
:lexeme ~ OpKeywordBinmode          priority => 1
:lexeme ~ OpKeywordBless            priority => 1
:lexeme ~ OpKeywordBreak            priority => 1
:lexeme ~ OpKeywordCaller           priority => 1
:lexeme ~ OpKeywordChdir            priority => 1
:lexeme ~ OpKeywordChmod            priority => 1
:lexeme ~ OpKeywordChomp            priority => 1
:lexeme ~ OpKeywordChop             priority => 1
:lexeme ~ OpKeywordChown            priority => 1
:lexeme ~ OpKeywordChr              priority => 1
:lexeme ~ OpKeywordChroot           priority => 1
:lexeme ~ OpKeywordClose            priority => 1
:lexeme ~ OpKeywordClosedir         priority => 1
:lexeme ~ OpKeywordConnect          priority => 1
:lexeme ~ OpKeywordContinue         priority => 1
:lexeme ~ OpKeywordCos              priority => 1
:lexeme ~ OpKeywordCrypt            priority => 1
:lexeme ~ OpKeywordDbmclose         priority => 1
:lexeme ~ OpKeywordDbmopen          priority => 1
:lexeme ~ OpKeywordDefined          priority => 1
:lexeme ~ OpKeywordDelete           priority => 1
:lexeme ~ OpKeywordDie              priority => 1
:lexeme ~ OpKeywordDo               priority => 1
:lexeme ~ OpKeywordDump             priority => 1
:lexeme ~ OpKeywordEach             priority => 1
:lexeme ~ OpKeywordEof              priority => 1
:lexeme ~ OpKeywordEval             priority => 1
:lexeme ~ OpKeywordEvalbytes        priority => 1
:lexeme ~ OpKeywordExec             priority => 1
:lexeme ~ OpKeywordExists           priority => 1
:lexeme ~ OpKeywordExit             priority => 1
:lexeme ~ OpKeywordExp              priority => 1
:lexeme ~ OpKeywordFc               priority => 1
:lexeme ~ OpKeywordFor              priority => 1
:lexeme ~ OpKeywordForeach          priority => 1
:lexeme ~ OpKeywordFcntl            priority => 1
:lexeme ~ OpKeywordFileno           priority => 1
:lexeme ~ OpKeywordFlock            priority => 1
:lexeme ~ OpKeywordFork             priority => 1
:lexeme ~ OpKeywordGetc             priority => 1
:lexeme ~ OpKeywordGetlogin         priority => 1
:lexeme ~ OpKeywordGetpeername      priority => 1
:lexeme ~ OpKeywordGetpgrp          priority => 1
:lexeme ~ OpKeywordGetppid          priority => 1
:lexeme ~ OpKeywordGetpriority      priority => 1
:lexeme ~ OpKeywordGetpwnam         priority => 1
:lexeme ~ OpKeywordGetgrnam         priority => 1
:lexeme ~ OpKeywordGethostbyname    priority => 1
:lexeme ~ OpKeywordGetnetbyname     priority => 1
:lexeme ~ OpKeywordGetprotobyname   priority => 1
:lexeme ~ OpKeywordGetpwuid         priority => 1
:lexeme ~ OpKeywordGetgrgid         priority => 1
:lexeme ~ OpKeywordGetservbyname    priority => 1
:lexeme ~ OpKeywordGethostbyaddr    priority => 1
:lexeme ~ OpKeywordGetnetbyaddr     priority => 1
:lexeme ~ OpKeywordGetprotobynumber priority => 1
:lexeme ~ OpKeywordGetservbyport    priority => 1
:lexeme ~ OpKeywordGetpwent         priority => 1
:lexeme ~ OpKeywordGetgrent         priority => 1
:lexeme ~ OpKeywordGethostent       priority => 1
:lexeme ~ OpKeywordGetnetent        priority => 1
:lexeme ~ OpKeywordGetprotoent      priority => 1
:lexeme ~ OpKeywordGetservent       priority => 1
:lexeme ~ OpKeywordSetpwent         priority => 1
:lexeme ~ OpKeywordSetgrent         priority => 1
:lexeme ~ OpKeywordSethostent       priority => 1
:lexeme ~ OpKeywordSetnetent        priority => 1
:lexeme ~ OpKeywordSetprotoent      priority => 1
:lexeme ~ OpKeywordSetservent       priority => 1
:lexeme ~ OpKeywordEndpwent         priority => 1
:lexeme ~ OpKeywordEndgrent         priority => 1
:lexeme ~ OpKeywordEndhostent       priority => 1
:lexeme ~ OpKeywordEndnetent        priority => 1
:lexeme ~ OpKeywordEndprotoent      priority => 1
:lexeme ~ OpKeywordEndservent       priority => 1
:lexeme ~ OpKeywordGetsockname      priority => 1
:lexeme ~ OpKeywordGetsockopt       priority => 1
:lexeme ~ OpKeywordGlob             priority => 1
:lexeme ~ OpKeywordGmtime           priority => 1
:lexeme ~ OpKeywordGoto             priority => 1
:lexeme ~ OpKeywordGrep             priority => 1
:lexeme ~ OpKeywordHex              priority => 1
:lexeme ~ OpKeywordIndex            priority => 1
:lexeme ~ OpKeywordInt              priority => 1
:lexeme ~ OpKeywordIoctl            priority => 1
:lexeme ~ OpKeywordJoin             priority => 1
:lexeme ~ OpKeywordKeys             priority => 1
:lexeme ~ OpKeywordKill             priority => 1
:lexeme ~ OpKeywordLast             priority => 1
:lexeme ~ OpKeywordLc               priority => 1
:lexeme ~ OpKeywordLcfirst          priority => 1
:lexeme ~ OpKeywordLength           priority => 1
:lexeme ~ OpKeywordLink             priority => 1
:lexeme ~ OpKeywordListen           priority => 1
:lexeme ~ OpKeywordLocal            priority => 1
:lexeme ~ OpKeywordLocaltime        priority => 1
:lexeme ~ OpKeywordLock             priority => 1
:lexeme ~ OpKeywordLog              priority => 1
:lexeme ~ OpKeywordLstat            priority => 1
:lexeme ~ OpKeywordMap              priority => 1
:lexeme ~ OpKeywordMkdir            priority => 1
:lexeme ~ OpKeywordMsgctl           priority => 1
:lexeme ~ OpKeywordMsgget           priority => 1
:lexeme ~ OpKeywordMsgrcv           priority => 1
:lexeme ~ OpKeywordMsgsnd           priority => 1
:lexeme ~ OpKeywordMy               priority => 1
:lexeme ~ OpKeywordNext             priority => 1
:lexeme ~ OpKeywordNo               priority => 1
:lexeme ~ OpKeywordOct              priority => 1
:lexeme ~ OpKeywordOpen             priority => 1
:lexeme ~ OpKeywordOpendir          priority => 1
:lexeme ~ OpKeywordOrd              priority => 1
:lexeme ~ OpKeywordOur              priority => 1
:lexeme ~ OpKeywordPack             priority => 1
:lexeme ~ OpKeywordPackage          priority => 1
:lexeme ~ OpKeywordPipe             priority => 1
:lexeme ~ OpKeywordPop              priority => 1
:lexeme ~ OpKeywordPos              priority => 1
:lexeme ~ OpKeywordPrint            priority => 1
:lexeme ~ OpKeywordPrintf           priority => 1
:lexeme ~ OpKeywordPrototype        priority => 1
:lexeme ~ OpKeywordPush             priority => 1
:lexeme ~ OpKeywordQuotemeta        priority => 1
:lexeme ~ OpKeywordRand             priority => 1
:lexeme ~ OpKeywordRead             priority => 1
:lexeme ~ OpKeywordReaddir          priority => 1
:lexeme ~ OpKeywordReadline         priority => 1
:lexeme ~ OpKeywordReadlink         priority => 1
:lexeme ~ OpKeywordReadpipe         priority => 1
:lexeme ~ OpKeywordRecv             priority => 1
:lexeme ~ OpKeywordRedo             priority => 1
:lexeme ~ OpKeywordRef              priority => 1
:lexeme ~ OpKeywordRename           priority => 1
:lexeme ~ OpKeywordRequire          priority => 1
:lexeme ~ OpKeywordReset            priority => 1
:lexeme ~ OpKeywordReturn           priority => 1
:lexeme ~ OpKeywordReverse          priority => 1
:lexeme ~ OpKeywordRewinddir        priority => 1
:lexeme ~ OpKeywordRindex           priority => 1
:lexeme ~ OpKeywordRmdir            priority => 1
:lexeme ~ OpKeywordSay              priority => 1
:lexeme ~ OpKeywordScalar           priority => 1
:lexeme ~ OpKeywordSeek             priority => 1
:lexeme ~ OpKeywordSeekdir          priority => 1
:lexeme ~ OpKeywordSelect           priority => 1
:lexeme ~ OpKeywordSemctl           priority => 1
:lexeme ~ OpKeywordSemget           priority => 1
:lexeme ~ OpKeywordSemop            priority => 1
:lexeme ~ OpKeywordSend             priority => 1
:lexeme ~ OpKeywordSetpgrp          priority => 1
:lexeme ~ OpKeywordSetpriority      priority => 1
:lexeme ~ OpKeywordSetsockopt       priority => 1
:lexeme ~ OpKeywordShift            priority => 1
:lexeme ~ OpKeywordShmctl           priority => 1
:lexeme ~ OpKeywordShmget           priority => 1
:lexeme ~ OpKeywordShmread          priority => 1
:lexeme ~ OpKeywordShmwrite         priority => 1
:lexeme ~ OpKeywordShutdown         priority => 1
:lexeme ~ OpKeywordSin              priority => 1
:lexeme ~ OpKeywordSleep            priority => 1
:lexeme ~ OpKeywordSocket           priority => 1
:lexeme ~ OpKeywordSocketpair       priority => 1
:lexeme ~ OpKeywordSort             priority => 1
:lexeme ~ OpKeywordSplice           priority => 1
:lexeme ~ OpKeywordSplit            priority => 1
:lexeme ~ OpKeywordSprintf          priority => 1
:lexeme ~ OpKeywordSqrt             priority => 1
:lexeme ~ OpKeywordSrand            priority => 1
:lexeme ~ OpKeywordStat             priority => 1
:lexeme ~ OpKeywordState            priority => 1
:lexeme ~ OpKeywordStudy            priority => 1
:lexeme ~ OpKeywordSub              priority => 1
:lexeme ~ OpKeywordSubstr           priority => 1
:lexeme ~ OpKeywordSymlink          priority => 1
:lexeme ~ OpKeywordSyscall          priority => 1
:lexeme ~ OpKeywordSysopen          priority => 1
:lexeme ~ OpKeywordSysread          priority => 1
:lexeme ~ OpKeywordSysseek          priority => 1
:lexeme ~ OpKeywordSystem           priority => 1
:lexeme ~ OpKeywordSyswrite         priority => 1
:lexeme ~ OpKeywordTell             priority => 1
:lexeme ~ OpKeywordTelldir          priority => 1
:lexeme ~ OpKeywordTie              priority => 1
:lexeme ~ OpKeywordTied             priority => 1
:lexeme ~ OpKeywordTime             priority => 1
:lexeme ~ OpKeywordTimes            priority => 1
:lexeme ~ OpKeywordTruncate         priority => 1
:lexeme ~ OpKeywordUc               priority => 1
:lexeme ~ OpKeywordUcfirst          priority => 1
:lexeme ~ OpKeywordUmask            priority => 1
:lexeme ~ OpKeywordUndef            priority => 1
:lexeme ~ OpKeywordUnlink           priority => 1
:lexeme ~ OpKeywordUnpack           priority => 1
:lexeme ~ OpKeywordUnshift          priority => 1
:lexeme ~ OpKeywordUntie            priority => 1
:lexeme ~ OpKeywordUse              priority => 1
:lexeme ~ OpKeywordUtime            priority => 1
:lexeme ~ OpKeywordValues           priority => 1
:lexeme ~ OpKeywordVec              priority => 1
:lexeme ~ OpKeywordWait             priority => 1
:lexeme ~ OpKeywordWaitpid          priority => 1
:lexeme ~ OpKeywordWantarray        priority => 1
:lexeme ~ OpKeywordWarn             priority => 1
:lexeme ~ OpKeywordWrite            priority => 1

# This is the only Op that conflicts
# OpInc conflicts with OpUnary
# So when it's both, OpInc wins
# (such as: sort $x + $y
:lexeme ~ OpAdd priority => 1

};

our $grammar = Marpa::R2::Scanless::G->new({ source => \$grammar_source });

sub build_struct {
    my ( $rec, $initial_valueref ) = @_;
    my @values = ($initial_valueref);

    while ( my $valueref = shift @values ) {
        if ( ! ref ${$valueref} ) {
            ${$valueref} = {
                'name'  => '',
                'type'  => 'lexeme',
                'value' => ${$valueref},
            };

            next;
        }

        my $name      = shift @{ ${$valueref} };
        my $start_pos = shift @{ ${$valueref} };
        my $length    = shift @{ ${$valueref} };
        my @children  = @{ ${$valueref} };

        my ( $line, $column ) = $rec->line_column($start_pos);
        ${$valueref} = {
            'name'      => $name,
            'type'      => 'rule',
            'start_pos' => $start_pos,
            'length'    => $length,
            'children'  => \@children,
            'line'      => $line,
            'column'    => $column,
        };

        push @values, map \$_, @{ ${$valueref}->{'children'} }
    }
}

sub parse {
    my ($class, $text) = @_;

    my %args = (
        'grammar' => $grammar,

        DEBUG()
        ? (
            'trace_terminals' => 1,
            'trace_values'    => 1,
          )
        : (),
    );

    my $rec = Marpa::R2::Scanless::R->new( \%args );

    my @values;
    eval {
        my $res = $rec->read( \$text );
        while ( my $value = $rec->value() ) {
            build_struct( $rec, $value );
            push @values, $$value;
        }
        1;
    }
    or do {
        my $err = $@;
        if (!@values) {
            for my $nterm (reverse qw/Program BlockStatement Statement NonBraceExprComma BlockLevelExpression Expression SubCall VarIdentExpr SubNameExpr/) {
                my ($start, $length) = $rec->last_completed($nterm);
                next unless defined $start;
                my $range = $rec->substring($start, $length);
                my $expect = $rec->terminals_expected();
                my $progress = $rec->show_progress();
                die "$err\nFailed to parse past: $range (char $start, length $length), expected " . ( join ',', @{$expect} );# . "\n$progress";
            }
            die "Failed to parse, dunno why.";
        }
    };

    return @values;
}

1;

__END__

=pod

=head1 SYNOPSIS

    use Guacamole;
    my ($ast) = Guacamole->parse($string);

=head1 DESCRIPITON

B<Guacamole> is a Perl parser toolkit.

It can:

=over 4

=item * Parse Standard Perl

This is explained in this document.

For B<Standard Perl>, see the next clause.

=item * Check a file is written in Standard Perl

This is done by L<standard>, which is where Standard Perl is described.

=item * Lint your code

See L<Guacamole::Linter>.

=item * Deparse your code

See L<Guacamole::Deparse>.

=item * Rewrite your code

There is a proof-of-concept for this and we hope to provide this as a framework.

=back

=head1 Standard Perl

Guacamole only works on Standard Perl. You can read about it here: L<standard>.

=head1 Parser

    my ($ast) = Guacamole->parse($string);

To parse a string, call L<Gaucamole>'s C<parse> method. (This might turn to an
object-oriented interface in the future.)

It returns a list of results. If it ever returns more than one, this is a bug that
means it couldn't ambiguously parse something. This will later be enforced in the
interface. The current interface is not official.

=head2 AST Nodes

Guacamole returns an AST with two types of nodes.

    my ($ast) = Guacamole->parse('$foo = 1');

The above will generate a larger AST than you imagine (which might be pruned
in the future). We'll focus on two types of nodes that will appear above.

=head3 Rules

Rules are the top level expressions. They include the definitions for rules.
They include information on location in the file, length, line, and column.

    $rule = {
        'children'  => [...],
        'column'    => 2,
        'length'    => 3,
        'line'      => 1,
        'name'      => 'VarIdentExpr',
        'start_pos' => 1,
        'type'      => 'rule',
    },

This rule is a C<VarIdentExpr> which is an expression for a variable identity.

In the code above, it refers to the C<foo> in C<$foo> - which is the identity
itself.

It has one child, described below under C<Lexemes>.

=head3 Lexemes

The child for the C<VarIdentExpr> rule should be the value of the identity.

    $lexeme = {
        'name'  => '',
        'type'  => 'lexeme',
        'value' => 'foo',
    };

The C<name> attribute for all lexemes is empty. This is to make it easy to
write code that checks for the value of a rule without having to check whether
it's a rule first.

=head1 THANKS

=over 4

=item * Damian Conway

For helping understand what is feasible, what isn't, and why, and for having
infinite patience in explaining these.

=item * Jeffrey Kegler

For L<Marpa> and helping understand how to use Marpa better.

=item * Gonzalo Diethelm

For continuous feedback and support.

=item * H. Merijn Brand (@Tux)

For providing the initial production-level test of Guacamole to
help shake many of the bugs in the BNF.

=back

=head1 SEE ALSO

=over 4

=item * L<standard>

=item * L<Gaucamole::Linter>

=item * L<Guacamole::Deparse>

=back

