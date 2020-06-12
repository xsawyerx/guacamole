package Guacamole;
use strict;
use warnings;
use Marpa::R2;
use constant {
    'DEBUG' => 0,
};

my $grammar_source = q{
lexeme default = latm => 1
:default ::= action => [ name, start, length, values ]

Program ::= (WS_Any) StatementSeq (WS_Any)

StatementSeq ::= Statement (WS_Any)
               | Statement (WS_Any) Semicolon
               | Statement (WS_Any) Semicolon (WS_Any) StatementSeq
               | BlockStatement (WS_Any)
               | BlockStatement (WS_Any) StatementSeq

# Statements that end with a block and do not need a semicolon terminator.
BlockStatement ::= LoopStatement
                 | PackageStatement
                 | SubStatement
                 | Condition
                 | Block

Statement ::= BlockLevelExpression (WS_Many) StatementModifier
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

ForStatement ::= ForStatementOp (WS_Any) LParen (WS_Any) Statement (WS_Any) Semicolon (WS_Any) Statement (WS_Any) Semicolon (WS_Any) Statement (WS_Any) RParen (WS_Any) Block (WS_Any) ContinueExpr
               | ForStatementOp (WS_Any) LParen (WS_Any) Statement (WS_Any) Semicolon (WS_Any) Statement (WS_Any) Semicolon (WS_Any) Statement (WS_Any) RParen (WS_Any) Block
               | ForStatementOp (WS_Many) OpKeywordMy (WS_Many) VarScalar (WS_Any) LParen (WS_Any) Expression (WS_Any) RParen (WS_Any) Block (WS_Any) ContinueExpr
               | ForStatementOp (WS_Many) OpKeywordMy (WS_Many) VarScalar (WS_Any) LParen (WS_Any) Expression (WS_Any) RParen (WS_Any) Block
               | ForStatementOp (WS_Many) VarScalar (WS_Any) LParen (WS_Any) Expression (WS_Any) RParen (WS_Any) Block (WS_Any) ContinueExpr
               | ForStatementOp (WS_Many) VarScalar (WS_Any) LParen (WS_Any) Expression (WS_Any) RParen (WS_Any) Block
               | ForStatementOp (WS_Many) LParen (WS_Any) Semicolon (WS_Any) Semicolon (WS_Any) RParen (WS_Any) Block (WS_Any) ContinueExpr
               | ForStatementOp (WS_Many) LParen (WS_Any) Semicolon (WS_Any) Semicolon (WS_Any) RParen (WS_Any) Block
               | ForStatementOp (WS_Many) LParen (WS_Any) Expression (WS_Any) RParen (WS_Any) Block (WS_Any) ContinueExpr
               | ForStatementOp (WS_Many) LParen (WS_Any) Expression (WS_Any) RParen (WS_Any) Block

ContinueExpr ::= OpKeywordContinue (WS_Many) Block

ForStatementOp ::= OpKeywordFor
                 | OpKeywordForeach

WhileStatement ::= ConditionWhile (WS_Any) LParen (WS_Any) Expression (WS_Any) RParen (WS_Any) Block (WS_Any) OpKeywordContinue (WS_Any) Block
                 | ConditionWhile (WS_Any) LParen (WS_Any) Expression (WS_Any) RParen (WS_Any) Block
                 | ConditionWhile (WS_Any) LParen (WS_Any) RParen (WS_Any) Block (WS_Any) OpKeywordContinue (WS_Any) Block
                 | ConditionWhile (WS_Any) LParen (WS_Any) RParen (WS_Any) Block

UntilStatement ::= ConditionUntil (WS_Any) LParen (WS_Any) Expression (WS_Any) RParen (WS_Any) Block (WS_Any) OpKeywordContinue (WS_Any) Block
                 | ConditionUntil (WS_Any) LParen (WS_Any) Expression (WS_Any) RParen (WS_Any) Block

StatementModifier ::= ConditionIfPostfixExpr
                    | ConditionUnlessPostfixExpr
                    | ConditionWhilePostfixExpr
                    | ConditionUntilPostfixExpr
                    | ConditionForPostfixExpr
                    | ConditionForeachPostfixExpr

EllipsisStatement ::= Ellipsis

UseStatement ::= OpKeywordUse (WS_Many) Ident (WS_Many) VersionExpr (WS_Many) Expression
               | OpKeywordUse (WS_Many) Ident (WS_Many) VersionExpr
               | OpKeywordUse (WS_Many) Ident (WS_Many) Expression
               | OpKeywordUse (WS_Many) VersionExpr
               | OpKeywordUse (WS_Many) Ident

NoStatement ::= OpKeywordNo (WS_Many) Ident (WS_Many) VersionExpr (WS_Many) Expression
              | OpKeywordNo (WS_Many) Ident (WS_Many) VersionExpr
              | OpKeywordNo (WS_Many) Ident (WS_Many) Expression
              | OpKeywordNo (WS_Many) VersionExpr
              | OpKeywordNo (WS_Many) Ident

RequireStatement ::= OpKeywordRequire (WS_Many) VersionExpr
                   | OpKeywordRequire (WS_Many) Ident
                   | OpKeywordRequire (WS_Many) Expression

PackageStatement ::= OpKeywordPackage (WS_Many) Ident (WS_Many) VersionExpr (WS_Any) Block
                   | OpKeywordPackage (WS_Many) Ident (WS_Any) Block

PackageDeclaration ::= OpKeywordPackage (WS_Many) Ident (WS_Many) VersionExpr
                     | OpKeywordPackage (WS_Many) Ident

SubStatement ::= PhaseStatement (WS_Any) Block
               | OpKeywordSub (WS_Many) PhaseStatement (WS_Any) Block
               | OpKeywordSub (WS_Many) SubNameExpr (WS_Any) SubDefinition

SubDeclaration ::= OpKeywordSub (WS_Many) SubNameExpr

SubDefinition ::= SubAttrsDefinitionSeq (WS_Many) SubSigsDefinition (WS_Any) Block
                | SubAttrsDefinitionSeq (WS_Any) Block
                | SubSigsDefinition (WS_Any) Block
                | Block

SubAttrsDefinitionSeq ::= SubAttrsDefinition (WS_Many) SubAttrsDefinitionSeq
                        | SubAttrsDefinition

SubAttrsDefinition ::= Colon (WS_Any) IdentComp (WS_Any) SubAttrArgs
                     | Colon (WS_Any) IdentComp

SubSigsDefinition ::= ParenExpr

PhaseStatement ::= PhaseName

Condition ::= ConditionIfExpr (WS_Any) ConditionElsifExpr (WS_Any) ConditionElseExpr
            | ConditionIfExpr (WS_Any) ConditionElseExpr
            | ConditionIfExpr (WS_Any) ConditionElsifExpr
            | ConditionIfExpr
            | ConditionUnlessExpr

ConditionUnlessExpr         ::= ConditionUnless  (WS_Any) LParen (WS_Any) Expression (WS_Any) RParen (WS_Any) Block
ConditionIfExpr             ::= ConditionIf      (WS_Any) LParen (WS_Any) Expression (WS_Any) RParen (WS_Any) Block
ConditionElsifExpr          ::= ConditionElsif   (WS_Any) LParen (WS_Any) Expression (WS_Any) RParen (WS_Any) Block (WS_Any) ConditionElsifExpr
                              | ConditionElsif   (WS_Any) LParen (WS_Any) Expression (WS_Any) RParen (WS_Any) Block
ConditionElseExpr           ::= ConditionElse    (WS_Any) Block
ConditionIfPostfixExpr      ::= ConditionIf      (WS_Any) Expression
ConditionUnlessPostfixExpr  ::= ConditionUnless  (WS_Any) Expression
ConditionWhilePostfixExpr   ::= ConditionWhile   (WS_Any) Expression
ConditionUntilPostfixExpr   ::= ConditionUntil   (WS_Any) Expression
ConditionForPostfixExpr     ::= ConditionFor     (WS_Any) Expression
ConditionForeachPostfixExpr ::= ConditionForeach (WS_Any) Expression

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
ExprIncU      ::= OpInc (WS_Any) ExprArrowU | ExprArrowR (WS_Any) OpInc   | ExprArrowU   action => ::first
ExprInc0      ::= OpInc (WS_Any) ExprArrow0 | ExprArrowR (WS_Any) OpInc   | ExprArrow0   action => ::first
ExprIncL      ::= OpInc (WS_Any) ExprArrowL | ExprArrowR (WS_Any) OpInc   | ExprArrowL   action => ::first
ExprIncR      ::= OpInc (WS_Any) ExprArrowR | ExprArrowL (WS_Any) OpInc   | ExprArrowR   action => ::first
ExprPowerU    ::= ExprIncU    (WS_Any) OpPower   (WS_Any) ExprUnaryU      | ExprIncU     action => ::first
ExprPower0    ::= ExprIncU    (WS_Any) OpPower   (WS_Any) ExprUnary0      | ExprInc0     action => ::first
ExprPowerL    ::= ExprIncU    (WS_Any) OpPower   (WS_Any) ExprUnaryL      | ExprIncL     action => ::first
ExprPowerR    ::= ExprIncU    (WS_Any) OpPower   (WS_Any) ExprUnaryR      | ExprIncR     action => ::first
ExprUnaryU    ::= OpUnary     (WS_Any) ExprUnaryU                | ExprPowerU   action => ::first
ExprUnary0    ::= OpUnary     (WS_Any) ExprUnary0                | ExprPower0   action => ::first
ExprUnaryL    ::= OpUnary     (WS_Any) ExprUnaryL                | ExprPowerL   action => ::first
ExprUnaryR    ::= OpUnary     (WS_Any) ExprUnaryR                | ExprPowerR   action => ::first
ExprRegexU    ::= ExprRegexU  (WS_Any) OpRegex   (WS_Any) ExprUnaryU      | ExprUnaryU   action => ::first
ExprRegex0    ::= ExprRegexU  (WS_Any) OpRegex   (WS_Any) ExprUnary0      | ExprUnary0   action => ::first
ExprRegexL    ::= ExprRegexU  (WS_Any) OpRegex   (WS_Any) ExprUnaryL      | ExprUnaryL   action => ::first
ExprRegexR    ::= ExprRegexU  (WS_Any) OpRegex   (WS_Any) ExprUnaryR      | ExprUnaryR   action => ::first
ExprMulU      ::= ExprMulU    (WS_Any) OpMulti   (WS_Any) ExprRegexU      | ExprRegexU   action => ::first
ExprMul0      ::= ExprMulU    (WS_Any) OpMulti   (WS_Any) ExprRegex0      | ExprRegex0   action => ::first
ExprMulL      ::= ExprMulU    (WS_Any) OpMulti   (WS_Any) ExprRegexL      | ExprRegexL   action => ::first
ExprMulR      ::= ExprMulU    (WS_Any) OpMulti   (WS_Any) ExprRegexR      | ExprRegexR   action => ::first
ExprAddU      ::= ExprAddU    (WS_Any) OpAdd     (WS_Any) ExprMulU        | ExprMulU     action => ::first
ExprAdd0      ::= ExprAddU    (WS_Any) OpAdd     (WS_Any) ExprMul0        | ExprMul0     action => ::first
ExprAddL      ::= ExprAddU    (WS_Any) OpAdd     (WS_Any) ExprMulL        | ExprMulL     action => ::first
ExprAddR      ::= ExprAddU    (WS_Any) OpAdd     (WS_Any) ExprMulR        | ExprMulR     action => ::first
ExprShiftU    ::= ExprShiftU  (WS_Any) OpShift   (WS_Any) ExprAddU        | ExprAddU     action => ::first
ExprShift0    ::= ExprShiftU  (WS_Any) OpShift   (WS_Any) ExprAdd0        | ExprAdd0     action => ::first
ExprShiftL    ::= ExprShiftU  (WS_Any) OpShift   (WS_Any) ExprAddL        | ExprAddL     action => ::first
ExprShiftR    ::= ExprShiftU  (WS_Any) OpShift   (WS_Any) ExprAddR        | ExprAddR     action => ::first
ExprFile0     ::= OpFile      (WS_Many) ExprFile0                 | ExprShift0 action => ::first
ExprFileL     ::= OpFile      (WS_Many) ExprFileL                 | ExprShiftL action => ::first
ExprFileR     ::= OpFile      (WS_Many) ExprFileR                 | ExprShiftR action => ::first
ExprNeq0      ::= ExprFile0   (WS_Any) OpInequal (WS_Any) ExprFile0       | ExprFile0    action => ::first
ExprNeqL      ::= ExprFile0   (WS_Any) OpInequal (WS_Any) ExprFileL       | ExprFileL    action => ::first
ExprNeqR      ::= ExprFile0   (WS_Any) OpInequal (WS_Any) ExprFileR       | ExprFileR    action => ::first
ExprEq0       ::= ExprNeq0    (WS_Any) OpEqual   (WS_Any) ExprNeq0        | ExprNeq0     action => ::first
ExprEqL       ::= ExprNeq0    (WS_Any) OpEqual   (WS_Any) ExprNeqL        | ExprNeqL     action => ::first
ExprEqR       ::= ExprNeq0    (WS_Any) OpEqual   (WS_Any) ExprNeqR        | ExprNeqR     action => ::first
ExprBinAnd0   ::= ExprBinAnd0 (WS_Any) OpBinAnd  (WS_Any) ExprEq0         | ExprEq0      action => ::first
ExprBinAndL   ::= ExprBinAnd0 (WS_Any) OpBinAnd  (WS_Any) ExprEqL         | ExprEqL      action => ::first
ExprBinAndR   ::= ExprBinAnd0 (WS_Any) OpBinAnd  (WS_Any) ExprEqR         | ExprEqR      action => ::first
ExprBinOr0    ::= ExprBinOr0  (WS_Any) OpBinOr   (WS_Any) ExprBinAnd0     | ExprBinAnd0  action => ::first
ExprBinOrL    ::= ExprBinOr0  (WS_Any) OpBinOr   (WS_Any) ExprBinAndL     | ExprBinAndL  action => ::first
ExprBinOrR    ::= ExprBinOr0  (WS_Any) OpBinOr   (WS_Any) ExprBinAndR     | ExprBinAndR  action => ::first
ExprLogAnd0   ::= ExprLogAnd0 (WS_Any) OpLogAnd  (WS_Any) ExprBinOr0      | ExprBinOr0   action => ::first
ExprLogAndL   ::= ExprLogAnd0 (WS_Any) OpLogAnd  (WS_Any) ExprBinOrL      | ExprBinOrL   action => ::first
ExprLogAndR   ::= ExprLogAnd0 (WS_Any) OpLogAnd  (WS_Any) ExprBinOrR      | ExprBinOrR   action => ::first
ExprLogOr0    ::= ExprLogOr0  (WS_Any) OpLogOr   (WS_Any) ExprLogAnd0     | ExprLogAnd0  action => ::first
ExprLogOrL    ::= ExprLogOr0  (WS_Any) OpLogOr   (WS_Any) ExprLogAndL     | ExprLogAndL  action => ::first
ExprLogOrR    ::= ExprLogOr0  (WS_Any) OpLogOr   (WS_Any) ExprLogAndR     | ExprLogAndR  action => ::first
ExprRange0    ::= ExprLogOr0  (WS_Any) OpRange   (WS_Any) ExprLogOr0      | ExprLogOr0   action => ::first
ExprRangeL    ::= ExprLogOr0  (WS_Any) OpRange   (WS_Any) ExprLogOrL      | ExprLogOrL   action => ::first
ExprRangeR    ::= ExprLogOr0  (WS_Any) OpRange   (WS_Any) ExprLogOrR      | ExprLogOrR   action => ::first
ExprCond0     ::= ExprRange0  (WS_Any) OpTriThen (WS_Any) ExprRange0 (WS_Any) OpTriElse (WS_Any) ExprCond0 | ExprRange0 action => ::first
ExprCondL     ::= ExprRange0  (WS_Any) OpTriThen (WS_Any) ExprRangeL (WS_Any) OpTriElse (WS_Any) ExprCondL | ExprRangeL action => ::first
ExprCondR     ::= ExprRange0  (WS_Any) OpTriThen (WS_Any) ExprRangeR (WS_Any) OpTriElse (WS_Any) ExprCondR | ExprRangeR action => ::first
ExprAssignL   ::= ExprCond0   (WS_Any) OpAssign  (WS_Any) ExprAssignL     | OpAssignKeywordExpr
                                                        | ExprCondL     action => ::first
ExprAssignR   ::= ExprCond0   (WS_Any) OpAssign  (WS_Any) ExprAssignR     | ExprCondR     action => ::first
ExprComma     ::= ExprAssignL (WS_Any) OpComma (WS_Any) ExprComma | ExprAssignL (WS_Any) OpComma | ExprAssignR action => ::first
ExprNameNot   ::= OpNameNot   (WS_Many) ExprNameNot               | ExprComma     action => ::first
ExprNameAnd   ::= ExprNameAnd (WS_Many) OpNameAnd (WS_Many) ExprNameNot     | ExprNameNot   action => ::first
ExprNameOr    ::= ExprNameOr  (WS_Many) OpNameOr  (WS_Many) ExprNameAnd     | ExprNameAnd   action => ::first
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
NonBraceExprIncU      ::= OpInc (WS_Any) ExprArrowU | NonBraceExprArrowR (WS_Any) OpInc   | NonBraceExprArrowU   action => ::first
NonBraceExprInc0      ::= OpInc (WS_Any) ExprArrow0 | NonBraceExprArrowR (WS_Any) OpInc   | NonBraceExprArrow0   action => ::first
NonBraceExprIncL      ::= OpInc (WS_Any) ExprArrowL | NonBraceExprArrowR (WS_Any) OpInc   | NonBraceExprArrowL   action => ::first
NonBraceExprIncR      ::= OpInc (WS_Any) ExprArrowR | NonBraceExprArrowL (WS_Any) OpInc   | NonBraceExprArrowR   action => ::first
NonBraceExprPowerU    ::= NonBraceExprIncU    (WS_Any) OpPower   (WS_Any) ExprUnaryU      | NonBraceExprIncU     action => ::first
NonBraceExprPower0    ::= NonBraceExprIncU    (WS_Any) OpPower   (WS_Any) ExprUnary0      | NonBraceExprInc0     action => ::first
NonBraceExprPowerL    ::= NonBraceExprIncU    (WS_Any) OpPower   (WS_Any) ExprUnaryL      | NonBraceExprIncL     action => ::first
NonBraceExprPowerR    ::= NonBraceExprIncU    (WS_Any) OpPower   (WS_Any) ExprUnaryR      | NonBraceExprIncR     action => ::first
NonBraceExprUnaryU    ::= OpUnary     (WS_Any) ExprUnaryU                | NonBraceExprPowerU   action => ::first
NonBraceExprUnary0    ::= OpUnary     (WS_Any) ExprUnary0                | NonBraceExprPower0   action => ::first
NonBraceExprUnaryL    ::= OpUnary     (WS_Any) ExprUnaryL                | NonBraceExprPowerL   action => ::first
NonBraceExprUnaryR    ::= OpUnary     (WS_Any) ExprUnaryR                | NonBraceExprPowerR   action => ::first
NonBraceExprRegexU    ::= NonBraceExprRegexU  (WS_Any) OpRegex   (WS_Any) ExprUnaryU      | NonBraceExprUnaryU   action => ::first
NonBraceExprRegex0    ::= NonBraceExprRegexU  (WS_Any) OpRegex   (WS_Any) ExprUnary0      | NonBraceExprUnary0   action => ::first
NonBraceExprRegexL    ::= NonBraceExprRegexU  (WS_Any) OpRegex   (WS_Any) ExprUnaryL      | NonBraceExprUnaryL   action => ::first
NonBraceExprRegexR    ::= NonBraceExprRegexU  (WS_Any) OpRegex   (WS_Any) ExprUnaryR      | NonBraceExprUnaryR   action => ::first
NonBraceExprMulU      ::= NonBraceExprMulU    (WS_Any) OpMulti   (WS_Any) ExprRegexU      | NonBraceExprRegexU   action => ::first
NonBraceExprMul0      ::= NonBraceExprMulU    (WS_Any) OpMulti   (WS_Any) ExprRegex0      | NonBraceExprRegex0   action => ::first
NonBraceExprMulL      ::= NonBraceExprMulU    (WS_Any) OpMulti   (WS_Any) ExprRegexL      | NonBraceExprRegexL   action => ::first
NonBraceExprMulR      ::= NonBraceExprMulU    (WS_Any) OpMulti   (WS_Any) ExprRegexR      | NonBraceExprRegexR   action => ::first
NonBraceExprAddU      ::= NonBraceExprAddU    (WS_Any) OpAdd     (WS_Any) ExprMulU        | NonBraceExprMulU     action => ::first
NonBraceExprAdd0      ::= NonBraceExprAddU    (WS_Any) OpAdd     (WS_Any) ExprMul0        | NonBraceExprMul0     action => ::first
NonBraceExprAddL      ::= NonBraceExprAddU    (WS_Any) OpAdd     (WS_Any) ExprMulL        | NonBraceExprMulL     action => ::first
NonBraceExprAddR      ::= NonBraceExprAddU    (WS_Any) OpAdd     (WS_Any) ExprMulR        | NonBraceExprMulR     action => ::first
NonBraceExprShiftU    ::= NonBraceExprShiftU  (WS_Any) OpShift   (WS_Any) ExprAddU        | NonBraceExprAddU     action => ::first
NonBraceExprShift0    ::= NonBraceExprShiftU  (WS_Any) OpShift   (WS_Any) ExprAdd0        | NonBraceExprAdd0     action => ::first
NonBraceExprShiftL    ::= NonBraceExprShiftU  (WS_Any) OpShift   (WS_Any) ExprAddL        | NonBraceExprAddL     action => ::first
NonBraceExprShiftR    ::= NonBraceExprShiftU  (WS_Any) OpShift   (WS_Any) ExprAddR        | NonBraceExprAddR     action => ::first
NonBraceExprFile0     ::= OpFile      (WS_Many) ExprFile0                 | NonBraceExprShift0 action => ::first
NonBraceExprFileL     ::= OpFile      (WS_Many) ExprFileL                 | NonBraceExprShiftL action => ::first
NonBraceExprFileR     ::= OpFile      (WS_Many) ExprFileR                 | NonBraceExprShiftR action => ::first
NonBraceExprNeq0      ::= NonBraceExprFile0   (WS_Any) OpInequal (WS_Any) ExprFile0       | NonBraceExprFile0    action => ::first
NonBraceExprNeqL      ::= NonBraceExprFile0   (WS_Any) OpInequal (WS_Any) ExprFileL       | NonBraceExprFileL    action => ::first
NonBraceExprNeqR      ::= NonBraceExprFile0   (WS_Any) OpInequal (WS_Any) ExprFileR       | NonBraceExprFileR    action => ::first
NonBraceExprEq0       ::= NonBraceExprNeq0    (WS_Any) OpEqual   (WS_Any) ExprNeq0        | NonBraceExprNeq0     action => ::first
NonBraceExprEqL       ::= NonBraceExprNeq0    (WS_Any) OpEqual   (WS_Any) ExprNeqL        | NonBraceExprNeqL     action => ::first
NonBraceExprEqR       ::= NonBraceExprNeq0    (WS_Any) OpEqual   (WS_Any) ExprNeqR        | NonBraceExprNeqR     action => ::first
NonBraceExprBinAnd0   ::= NonBraceExprBinAnd0 (WS_Any) OpBinAnd  (WS_Any) ExprEq0         | NonBraceExprEq0      action => ::first
NonBraceExprBinAndL   ::= NonBraceExprBinAnd0 (WS_Any) OpBinAnd  (WS_Any) ExprEqL         | NonBraceExprEqL      action => ::first
NonBraceExprBinAndR   ::= NonBraceExprBinAnd0 (WS_Any) OpBinAnd  (WS_Any) ExprEqR         | NonBraceExprEqR      action => ::first
NonBraceExprBinOr0    ::= NonBraceExprBinOr0  (WS_Any) OpBinOr   (WS_Any) ExprBinAnd0     | NonBraceExprBinAnd0  action => ::first
NonBraceExprBinOrL    ::= NonBraceExprBinOr0  (WS_Any) OpBinOr   (WS_Any) ExprBinAndL     | NonBraceExprBinAndL  action => ::first
NonBraceExprBinOrR    ::= NonBraceExprBinOr0  (WS_Any) OpBinOr   (WS_Any) ExprBinAndR     | NonBraceExprBinAndR  action => ::first
NonBraceExprLogAnd0   ::= NonBraceExprLogAnd0 (WS_Any) OpLogAnd  (WS_Any) ExprBinOr0      | NonBraceExprBinOr0   action => ::first
NonBraceExprLogAndL   ::= NonBraceExprLogAnd0 (WS_Any) OpLogAnd  (WS_Any) ExprBinOrL      | NonBraceExprBinOrL   action => ::first
NonBraceExprLogAndR   ::= NonBraceExprLogAnd0 (WS_Any) OpLogAnd  (WS_Any) ExprBinOrR      | NonBraceExprBinOrR   action => ::first
NonBraceExprLogOr0    ::= NonBraceExprLogOr0  (WS_Any) OpLogOr   (WS_Any) ExprLogAnd0     | NonBraceExprLogAnd0  action => ::first
NonBraceExprLogOrL    ::= NonBraceExprLogOr0  (WS_Any) OpLogOr   (WS_Any) ExprLogAndL     | NonBraceExprLogAndL  action => ::first
NonBraceExprLogOrR    ::= NonBraceExprLogOr0  (WS_Any) OpLogOr   (WS_Any) ExprLogAndR     | NonBraceExprLogAndR  action => ::first
NonBraceExprRange0    ::= NonBraceExprLogOr0  (WS_Any) OpRange   (WS_Any) ExprLogOr0      | NonBraceExprLogOr0   action => ::first
NonBraceExprRangeL    ::= NonBraceExprLogOr0  (WS_Any) OpRange   (WS_Any) ExprLogOrL      | NonBraceExprLogOrL   action => ::first
NonBraceExprRangeR    ::= NonBraceExprLogOr0  (WS_Any) OpRange   (WS_Any) ExprLogOrR      | NonBraceExprLogOrR   action => ::first
NonBraceExprCond0     ::= NonBraceExprRange0  (WS_Any) OpTriThen (WS_Any) ExprRange0 (WS_Any) OpTriElse (WS_Any) ExprCond0 | NonBraceExprRange0 action => ::first
NonBraceExprCondL     ::= NonBraceExprRange0  (WS_Any) OpTriThen (WS_Any) ExprRangeL (WS_Any) OpTriElse (WS_Any) ExprCondL | NonBraceExprRangeL action => ::first
NonBraceExprCondR     ::= NonBraceExprRange0  (WS_Any) OpTriThen (WS_Any) ExprRangeR (WS_Any) OpTriElse (WS_Any) ExprCondR | NonBraceExprRangeR action => ::first
NonBraceExprAssignL   ::= NonBraceExprCond0   (WS_Any) OpAssign  (WS_Any) ExprAssignL     | OpAssignKeywordExpr
                                                        | NonBraceExprCondL     action => ::first
NonBraceExprAssignR   ::= NonBraceExprCond0   (WS_Any) OpAssign  (WS_Any) ExprAssignR     | NonBraceExprCondR     action => ::first


NonBraceExprComma     ::= NonBraceExprAssignL (WS_Any) OpComma (WS_Any) ExprComma    | NonBraceExprAssignR action => ::first

# Comma is only allowed if it follows a keyword operator, to avoid block/hash disambiguation in perl.
BlockLevelExprNameNot ::= OpNameNot (WS_Many) ExprNameNot | NonBraceExprAssignR action => ::first
BlockLevelExprNameAnd ::= BlockLevelExprNameAnd (WS_Many) OpNameAnd (WS_Many) ExprNameNot | BlockLevelExprNameNot action => ::first
BlockLevelExprNameOr  ::= BlockLevelExprNameOr (WS_Many) OpNameOr (WS_Many) ExprNameAnd | BlockLevelExprNameAnd action => ::first
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

NonLiteral ::= GlobalVariable
             | Variable
             | DerefVariable
             | Modifier (WS_Many) Variable
             | Modifier (WS_Many) ParenExpr
             | UnderscoreValues
             | SubCall
             | PackageArrow
             | ParenExpr ElemSeq0
             | OpNullaryKeywordExpr
             | DiamondExpr

GlobalVariable ~ '$!'
               | '$"'
               | '$#'
               | '$%'
               | '$&'
               | '$' [']
               | '$('
               | '$)'
               | '$*'
               | '$+'
               | '$,'
               | '$-'
               | '$.'
               | '$/'
               | '$:'
               | '$;'
               | '$<'
               | '$='
               | '$>'
               | '$?'
               | '$@'
               | '$['
               | '$\\'
               | '$]'
               | '$^'
               | '$_'
               | '$`'
               | '$|'
               | '$~'
               | '$$'
               | '$^A'
               | '$^C'
               | '${^CHILD_ERROR_NATIVE}'
               | '$^D'
               | '$<*digits*>'
               | '$^E'
               | '${^ENCODING}'
               | '$^F'
               | '${^GLOBAL_PHASE}'
               | '$^H'
               | '$^I'
               | '$^L'
               | '${^LAST_FH}'
               | '$^M'
               | '${^MATCH}'
               | '$^N'
               | '$^O'
               | '${^OPEN}'
               | '$^P'
               | '${^POSTMATCH}'
               | '${^PREMATCH}'
               | '$^R'
               | '${^RE_COMPILE_RECURSION_LIMIT}'
               | '${^RE_DEBUG_FLAGS}'
               | '${^RE_TRIE_MAXBUF}'
               | '$^S'
               | '${^SAFE_LOCALES}'
               | '$^T'
               | '${^TAINT}'
               | '${^UNICODE}'
               | '${^UTF8CACHE}'
               | '${^UTF8LOCALE}'
               | '$^V'
               | '$^W'
               | '${^WARNING_BITS}'
               | '${^WIN32_SLOPPY_STAT}'
               | '$^X'
               | '@+'
               | '@-'
               | '@_'
               | '@{^CAPTURE}'
               | '%!'
               | '%+'
               | '%-'
               | '%{^CAPTURE}'
               | '%{^CAPTURE_ALL}'
               | '%^H'


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

ParenExpr ::= LParen (WS_Any) Expression (WS_Any) RParen
            | LParen (WS_Any) RParen # support ()

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

Variable ::= VarScalar
           | VarArray
           | VarHash
           | VarCode
           | VarGlob
           | VarArrayTop

VarScalar   ::= SigilScalar VarName ElemSeq0
VarArray    ::= SigilArray VarName ElemSeq0
VarHash     ::= SigilHash VarName ElemSeq0
VarCode     ::= SigilCode VarName
VarGlob     ::= SigilGlob VarName
VarArrayTop ::= SigilArrayTop VarName

# This is tricky because
# $x is ok, $3 is ok, $x::3 is ok, $3::x is not ok
# so we define it as Digits are okay
# otherwise, a proper ident, which eliminated $3::x but also $x::3
VarName ::= DigitsExpr
         || Ident

DigitsExpr ::= [0-9]+

SubCall ::= SubNameCallExpr (WS_Any) CallArgs
          | VarCode (WS_Any) CallArgs

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
CoreSubLetters   ~ [a-zA-z0-9_]*

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

# These are sort of the same but not
# Idents are defined differently for different purposes
# Subroutine names are defined using one ident
# Calling subroutines with arrow are defined using another ident
# Variables are defined using a different ident
# Namespaced variables ($x::y) are defined with another ident
Ident ::= SubNameExpr

CallArgs ::= ParenExpr

Block ::= LBrace (WS_Any) RBrace
        | LBrace (WS_Any) StatementSeq (WS_Any) RBrace

ArrayElem ::= LBracket (WS_Any) Expression (WS_Any) RBracket

HashElem ::= LBrace (WS_Any) Expression (WS_Any) RBrace

NonBraceLiteral ::= LitNumber
                  | LitArray
                  | LitString
                  | InterpolString

Literal         ::= NonBraceLiteral
                  | LitHash

LitArray       ::= LBracket (WS_Any) Expression (WS_Any) RBracket
                 | LBracket (WS_Any) RBracket

LitHash        ::= LBrace (WS_Any) Expression (WS_Any) RBrace
                 | LBrace (WS_Any) RBrace

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
ArrowMethodCall    ::= Ident CallArgs
ArrowIndirectCall  ::= SigilScalar Ident CallArgs

DerefVariableArgsAll ::= '$*' | '@*' | '%*' | '&*' | '**' | '$#*'

DerefVariableSlice ::= '@[' (WS_Any) Expression (WS_Any) ']'
                     | '@{' (WS_Any) Expression (WS_Any) '}'
                     | '%[' (WS_Any) Expression (WS_Any) ']'
                     | '%{' (WS_Any) Expression (WS_Any) '}'

DerefVariable ::= SigilScalar   Block
                | SigilArray    Block
                | SigilHash     Block
                | SigilGlob     Block
                | SigilArrayTop Block

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

OpKeywordAbsExpr              ::= OpKeywordAbs (WS_Any) OpUnaryKeywordArg
                                | OpKeywordAbs

OpKeywordAcceptExpr           ::= OpKeywordAccept (WS_Any) OpListKeywordArg

OpKeywordAlarmExpr            ::= OpKeywordAlarm (WS_Any) OpUnaryKeywordArg
                                | OpKeywordAlarm

OpKeywordAtan2Expr            ::= OpKeywordAtan2 (WS_Any) OpListKeywordArg

OpKeywordBindExpr             ::= OpKeywordBind (WS_Any) OpListKeywordArg

OpKeywordBinmodeExpr          ::= OpKeywordBinmode (WS_Any) OpListKeywordArg

OpKeywordBlessExpr            ::= OpKeywordBless (WS_Any) OpListKeywordArg

OpKeywordBreakExpr            ::= OpKeywordBreak (WS_Many) Label
                                | OpKeywordBreak

OpKeywordCallerExpr           ::= OpKeywordCaller (WS_Any) OpUnaryKeywordArg
                                | OpKeywordCaller

OpKeywordChdirExpr            ::= OpKeywordChdir (WS_Any) OpUnaryKeywordArg
                                | OpKeywordChdir

OpKeywordChmodExpr            ::= OpKeywordChmod (WS_Any) OpListKeywordArg

OpKeywordChompExpr            ::= OpKeywordChomp (WS_Any) OpUnaryKeywordArg
                                | OpKeywordChomp

OpKeywordChopExpr             ::= OpKeywordChop (WS_Any) OpUnaryKeywordArg
                                | OpKeywordChop

OpKeywordChownExpr            ::= OpKeywordChown (WS_Any) OpListKeywordArg

OpKeywordChrExpr              ::= OpKeywordChr (WS_Any) OpUnaryKeywordArg
                                | OpKeywordChr

OpKeywordChrootExpr           ::= OpKeywordChroot (WS_Any) OpUnaryKeywordArg
                                | OpKeywordChroot

OpKeywordCloseExpr            ::= OpKeywordClose (WS_Any) OpUnaryKeywordArg
                                | OpKeywordClose

OpKeywordClosedirExpr         ::= OpKeywordClosedir (WS_Any) OpUnaryKeywordArg

OpKeywordConnectExpr          ::= OpKeywordConnect (WS_Any) OpListKeywordArg

OpKeywordCosExpr              ::= OpKeywordCos (WS_Any) OpUnaryKeywordArg

OpKeywordCryptExpr            ::= OpKeywordCrypt (WS_Any) OpListKeywordArg

OpKeywordDbmcloseExpr         ::= OpKeywordDbmclose (WS_Any) OpUnaryKeywordArg

OpKeywordDbmopenExpr          ::= OpKeywordDbmopen (WS_Any) OpListKeywordArg

OpKeywordDefinedExpr          ::= OpKeywordDefined (WS_Any) OpUnaryKeywordArg
                                | OpKeywordDefined

OpKeywordDeleteExpr           ::= OpKeywordDelete (WS_Any) OpUnaryKeywordArg

OpKeywordDieExpr              ::= OpKeywordDie (WS_Any) OpListKeywordArg

OpKeywordDoExpr               ::= OpKeywordDo (WS_Any) Block
                                | OpKeywordDo (WS_Any) OpUnaryKeywordArgNonBrace

OpKeywordDumpExpr             ::= OpKeywordDump (WS_Any) OpAssignKeywordArg
                                | OpKeywordDump (WS_Many) Label
                                | OpKeywordDump

OpKeywordEachExpr             ::= OpKeywordEach (WS_Any) OpUnaryKeywordArg

OpKeywordEofExpr              ::= OpKeywordEof (WS_Any) OpUnaryKeywordArg
                                | OpKeywordEof

OpKeywordEvalExpr             ::= OpKeywordEval (WS_Any) Block

OpKeywordEvalbytesExpr        ::= OpKeywordEvalbytes (WS_Any) OpUnaryKeywordArg
                                | OpKeywordEvalbytes

OpKeywordExistsExpr           ::= OpKeywordExists (WS_Any) OpUnaryKeywordArg

OpKeywordExitExpr             ::= OpKeywordExit (WS_Any) OpUnaryKeywordArg
                                | OpKeywordExit

OpKeywordExpExpr              ::= OpKeywordExp (WS_Any) OpUnaryKeywordArg
                                | OpKeywordExp

OpKeywordFcExpr               ::= OpKeywordFc (WS_Any) OpUnaryKeywordArg
                                | OpKeywordFc

OpKeywordFcntlExpr            ::= OpKeywordFcntl (WS_Any) OpListKeywordArg

OpKeywordFilenoExpr           ::= OpKeywordFileno (WS_Any) OpUnaryKeywordArg

OpKeywordFlockExpr            ::= OpKeywordFlock (WS_Any) OpListKeywordArg

OpKeywordForkExpr             ::= OpKeywordFork

OpKeywordGetcExpr             ::= OpKeywordGetc (WS_Any) OpUnaryKeywordArg
                                | OpKeywordGetc

OpKeywordGetloginExpr         ::= OpKeywordGetlogin

OpKeywordGetpeernameExpr      ::= OpKeywordGetpeername (WS_Any) OpUnaryKeywordArg

OpKeywordGetpgrpExpr          ::= OpKeywordGetpgrp (WS_Any) OpUnaryKeywordArg

OpKeywordGetppidExpr          ::= OpKeywordGetppid

OpKeywordGetpriorityExpr      ::= OpKeywordGetpriority (WS_Any) OpListKeywordArg

OpKeywordGetpwnamExpr         ::= OpKeywordGetpwnam (WS_Any) OpUnaryKeywordArg

OpKeywordGetgrnamExpr         ::= OpKeywordGetgrnam (WS_Any) OpUnaryKeywordArg

OpKeywordGethostbynameExpr    ::= OpKeywordGethostbyname (WS_Any) OpUnaryKeywordArg

OpKeywordGetnetbynameExpr     ::= OpKeywordGetnetbyname (WS_Any) OpUnaryKeywordArg

OpKeywordGetprotobynameExpr   ::= OpKeywordGetprotobyname (WS_Any) OpUnaryKeywordArg

OpKeywordGetpwuidExpr         ::= OpKeywordGetpwuid (WS_Any) OpUnaryKeywordArg

OpKeywordGetgrgidExpr         ::= OpKeywordGetgrgid (WS_Any) OpUnaryKeywordArg

OpKeywordGetservbynameExpr    ::= OpKeywordGetservbyname (WS_Any) OpListKeywordArg

OpKeywordGethostbyaddrExpr    ::= OpKeywordGethostbyaddr (WS_Any) OpListKeywordArg

OpKeywordGetnetbyaddrExpr     ::= OpKeywordGetnetbyaddr (WS_Any) OpListKeywordArg

OpKeywordGetprotobynumberExpr ::= OpKeywordGetprotobynumber (WS_Any) OpUnaryKeywordArg

OpKeywordGetservbyportExpr    ::= OpKeywordGetservbyport (WS_Any) OpListKeywordArg

OpKeywordGetpwentExpr         ::= OpKeywordGetpwent

OpKeywordGetgrentExpr         ::= OpKeywordGetgrent

OpKeywordGethostentExpr       ::= OpKeywordGethostent

OpKeywordGetnetentExpr        ::= OpKeywordGetnetent

OpKeywordGetprotoentExpr      ::= OpKeywordGetprotoent

OpKeywordGetserventExpr       ::= OpKeywordGetservent

OpKeywordSetpwentExpr         ::= OpKeywordSetpwent

OpKeywordSetgrentExpr         ::= OpKeywordSetgrent

OpKeywordSethostentExpr       ::= OpKeywordSethostent (WS_Any) OpUnaryKeywordArg

OpKeywordSetnetentExpr        ::= OpKeywordSetnetent (WS_Any) OpUnaryKeywordArg

OpKeywordSetprotoentExpr      ::= OpKeywordSetprotoent (WS_Any) OpUnaryKeywordArg

OpKeywordSetserventExpr       ::= OpKeywordSetservent (WS_Any) OpUnaryKeywordArg

OpKeywordEndpwentExpr         ::= OpKeywordEndpwent

OpKeywordEndgrentExpr         ::= OpKeywordEndgrent

OpKeywordEndhostentExpr       ::= OpKeywordEndhostent

OpKeywordEndnetentExpr        ::= OpKeywordEndnetent

OpKeywordEndprotoentExpr      ::= OpKeywordEndprotoent

OpKeywordEndserventExpr       ::= OpKeywordEndservent

OpKeywordExecExpr             ::= OpKeywordExec (WS_Any) Block (WS_Any) OpListKeywordArg
                                | OpKeywordExec (WS_Any) OpListKeywordArgNonBrace

OpKeywordGetsocknameExpr      ::= OpKeywordGetsockname (WS_Any) OpUnaryKeywordArg

OpKeywordGetsockoptExpr       ::= OpKeywordGetsockopt (WS_Any) OpListKeywordArg

OpKeywordGlobExpr             ::= OpKeywordGlob (WS_Any) OpListKeywordArg
                                | OpKeywordGlob

OpKeywordGmtimeExpr           ::= OpKeywordGmtime (WS_Any) OpUnaryKeywordArg
                                | OpKeywordGmtime

# &NAME is an expression too
OpKeywordGotoExpr             ::= OpKeywordGoto (WS_Any) OpAssignKeywordArg
                                | OpKeywordGoto (WS_Many) Label

OpKeywordGrepExpr             ::= OpKeywordGrep (WS_Any) Block (WS_Any) OpListKeywordArg
                                | OpKeywordGrep (WS_Any) OpListKeywordArgNonBrace

OpKeywordHexExpr              ::= OpKeywordHex (WS_Any) OpUnaryKeywordArg
                                | OpKeywordHex

OpKeywordIndexExpr            ::= OpKeywordIndex (WS_Any) OpListKeywordArg

OpKeywordIntExpr              ::= OpKeywordInt (WS_Any) OpUnaryKeywordArg
                                | OpKeywordInt

OpKeywordIoctlExpr            ::= OpKeywordIoctl (WS_Any) OpListKeywordArg

OpKeywordJoinExpr             ::= OpKeywordJoin (WS_Any) OpListKeywordArg

OpKeywordKeysExpr             ::= OpKeywordKeys (WS_Any) OpUnaryKeywordArg

OpKeywordKillExpr             ::= OpKeywordKill (WS_Any) OpListKeywordArg
                                | OpKeywordKill (WS_Many) Expression

OpKeywordLastExpr             ::= OpKeywordLast (WS_Any) OpAssignKeywordArg
                                | OpKeywordLast (WS_Many) Label
                                | OpKeywordLast

OpKeywordLcExpr               ::= OpKeywordLc (WS_Any) OpUnaryKeywordArg
                                | OpKeywordLc

OpKeywordLcfirstExpr          ::= OpKeywordLcfirst (WS_Any) OpUnaryKeywordArg
                                | OpKeywordLcfirst

OpKeywordLengthExpr           ::= OpKeywordLength (WS_Any) OpUnaryKeywordArg
                                | OpKeywordLength

OpKeywordLinkExpr             ::= OpKeywordLink (WS_Any) OpListKeywordArg

OpKeywordListenExpr           ::= OpKeywordListen (WS_Any) OpListKeywordArg

OpKeywordLocaltimeExpr        ::= OpKeywordLocaltime (WS_Any) OpUnaryKeywordArg
                                | OpKeywordLocaltime

OpKeywordLockExpr             ::= OpKeywordLock (WS_Any) OpUnaryKeywordArg

OpKeywordLogExpr              ::= OpKeywordLog (WS_Any) OpUnaryKeywordArg
                                | OpKeywordLog

OpKeywordLstatExpr            ::= OpKeywordLstat (WS_Any) OpUnaryKeywordArg
                                | OpKeywordLstat

OpKeywordMapExpr              ::= OpKeywordMap (WS_Any) Block (WS_Any) OpListKeywordArg
                                | OpKeywordMap (WS_Any) OpListKeywordArgNonBrace

OpKeywordMkdirExpr            ::= OpKeywordMkdir (WS_Any) OpListKeywordArg
                                | OpKeywordMkdir

OpKeywordMsgctlExpr           ::= OpKeywordMsgctl (WS_Any) OpListKeywordArg

OpKeywordMsggetExpr           ::= OpKeywordMsgget (WS_Any) OpListKeywordArg

OpKeywordMsgrcvExpr           ::= OpKeywordMsgrcv (WS_Any) OpListKeywordArg

OpKeywordMsgsndExpr           ::= OpKeywordMsgsnd (WS_Any) OpListKeywordArg

OpKeywordNextExpr             ::= OpKeywordNext (WS_Any) OpAssignKeywordArg
                                | OpKeywordNext (WS_Many) Label
                                | OpKeywordNext

OpKeywordOctExpr              ::= OpKeywordOct (WS_Any) OpUnaryKeywordArg
                                | OpKeywordOct

OpKeywordOpenExpr             ::= OpKeywordOpen (WS_Any) OpListKeywordArg

OpKeywordOpendirExpr          ::= OpKeywordOpendir (WS_Any) OpListKeywordArg

OpKeywordOrdExpr              ::= OpKeywordOrd (WS_Any) OpUnaryKeywordArg
                                | OpKeywordOrd

OpKeywordPackExpr             ::= OpKeywordPack (WS_Any) OpListKeywordArg

OpKeywordPipeExpr             ::= OpKeywordPipe (WS_Any) OpListKeywordArg

OpKeywordPopExpr              ::= OpKeywordPop (WS_Any) OpUnaryKeywordArg
                                | OpKeywordPop

OpKeywordPosExpr              ::= OpKeywordPos (WS_Any) OpUnaryKeywordArg
                                | OpKeywordPos

OpKeywordPrintExpr            ::= OpKeywordPrint (WS_Any) Block (WS_Any) OpListKeywordArg
                                | OpKeywordPrint (WS_Many) BuiltinFilehandle (WS_Many) OpListKeywordArgNonBrace
                                | OpKeywordPrint (WS_Many) BuiltinFilehandle
                                | OpKeywordPrint (WS_Any) OpListKeywordArgNonBrace
                                | OpKeywordPrint (WS_Any) Block
                                | OpKeywordPrint

OpKeywordPrintfExpr           ::= OpKeywordPrintf (WS_Any) Block (WS_Any) OpListKeywordArg
                                | OpKeywordPrintf (WS_Many) BuiltinFilehandle (WS_Many) OpListKeywordArgNonBrace
                                | OpKeywordPrintf (WS_Many) BuiltinFilehandle
                                | OpKeywordPrintf (WS_Any) OpListKeywordArgNonBrace
                                | OpKeywordPrintf (WS_Any) Block

OpKeywordPrototypeExpr        ::= OpKeywordPrototype (WS_Any) OpUnaryKeywordArg
                                | OpKeywordPrototype

OpKeywordPushExpr             ::= OpKeywordPush (WS_Any) OpListKeywordArg

OpKeywordQuotemetaExpr        ::= OpKeywordQuotemeta (WS_Any) OpUnaryKeywordArg
                                | OpKeywordQuotemeta

OpKeywordRandExpr             ::= OpKeywordRand (WS_Any) OpUnaryKeywordArg
                                | OpKeywordRand

OpKeywordReadExpr             ::= OpKeywordRead (WS_Any) OpListKeywordArg

OpKeywordReaddirExpr          ::= OpKeywordReaddir (WS_Any) OpUnaryKeywordArg

OpKeywordReadlineExpr         ::= OpKeywordReadline (WS_Any) OpUnaryKeywordArg
                                | OpKeywordReadline

OpKeywordReadlinkExpr         ::= OpKeywordReadlink (WS_Any) OpUnaryKeywordArg
                                | OpKeywordReadlink

OpKeywordReadpipeExpr         ::= OpKeywordReadpipe (WS_Any) OpUnaryKeywordArg
                                | OpKeywordReadpipe

OpKeywordRecvExpr             ::= OpKeywordRecv (WS_Any) OpListKeywordArg

OpKeywordRedoExpr             ::= OpKeywordRedo (WS_Any) OpAssignKeywordArg
                                | OpKeywordRedo (WS_Many) Label
                                | OpKeywordRedo

OpKeywordRefExpr              ::= OpKeywordRef (WS_Any) OpUnaryKeywordArg
                                | OpKeywordRef

OpKeywordRenameExpr           ::= OpKeywordRename (WS_Many) OpListKeywordArg

OpKeywordResetExpr            ::= OpKeywordReset (WS_Any) OpUnaryKeywordArg
                                | OpKeywordReset

OpKeywordReturnExpr           ::= OpKeywordReturn (WS_Any) OpListKeywordArg
                                | OpKeywordReturn

OpKeywordReverseExpr          ::= OpKeywordReverse (WS_Any) OpListKeywordArg

OpKeywordRewinddirExpr        ::= OpKeywordRewinddir (WS_Any) OpUnaryKeywordArg
                                | OpKeywordRewinddir

OpKeywordRindexExpr           ::= OpKeywordRindex (WS_Any) OpListKeywordArg
                                | OpKeywordRindex

OpKeywordRmdirExpr            ::= OpKeywordRmdir (WS_Any) OpUnaryKeywordArg
                                | OpKeywordRmdir

OpKeywordSayExpr              ::= OpKeywordSay (WS_Any) Block (WS_Any) OpListKeywordArg
                                | OpKeywordSay (WS_Many) BuiltinFilehandle (WS_Many) OpListKeywordArgNonBrace
                                | OpKeywordSay (WS_Many) BuiltinFilehandle
                                | OpKeywordSay (WS_Any) OpListKeywordArgNonBrace
                                | OpKeywordSay Block
                                | OpKeywordSay

OpKeywordScalarExpr           ::= OpKeywordScalar (WS_Any) OpUnaryKeywordArg

OpKeywordSeekExpr             ::= OpKeywordSeek (WS_Any) OpListKeywordArg

OpKeywordSeekdirExpr          ::= OpKeywordSeekdir (WS_Any) OpListKeywordArg

OpKeywordSelectExpr           ::= OpKeywordSelect (WS_Any) OpListKeywordArg

OpKeywordSemctlExpr           ::= OpKeywordSemctl (WS_Any) OpListKeywordArg

OpKeywordSemgetExpr           ::= OpKeywordSemget (WS_Any) OpListKeywordArg

OpKeywordSemopExpr            ::= OpKeywordSemop (WS_Any) OpListKeywordArg

OpKeywordSendExpr             ::= OpKeywordSend (WS_Any) OpListKeywordArg

OpKeywordSetpgrpExpr          ::= OpKeywordSetpgrp (WS_Any) OpListKeywordArg

OpKeywordSetpriorityExpr      ::= OpKeywordSetpriority (WS_Any) OpListKeywordArg

OpKeywordSetsockoptExpr       ::= OpKeywordSetsockopt (WS_Any) OpListKeywordArg

OpKeywordShiftExpr            ::= OpKeywordShift (WS_Any) OpUnaryKeywordArg
                                | OpKeywordShift

OpKeywordShmctlExpr           ::= OpKeywordShmctl (WS_Any) OpListKeywordArg

OpKeywordShmgetExpr           ::= OpKeywordShmget (WS_Any) OpListKeywordArg

OpKeywordShmreadExpr          ::= OpKeywordShmread (WS_Any) OpListKeywordArg

OpKeywordShmwriteExpr         ::= OpKeywordShmwrite (WS_Any) OpListKeywordArg

OpKeywordShutdownExpr         ::= OpKeywordShutdown (WS_Any) OpListKeywordArg

OpKeywordSinExpr              ::= OpKeywordSin (WS_Any) OpUnaryKeywordArg
                                | OpKeywordSin

OpKeywordSleepExpr            ::= OpKeywordSleep (WS_Any) OpUnaryKeywordArg
                                | OpKeywordSleep

OpKeywordSocketExpr           ::= OpKeywordSocket (WS_Any) OpListKeywordArg

OpKeywordSocketpairExpr       ::= OpKeywordSocketpair (WS_Any) OpListKeywordArg

OpKeywordSortExpr             ::= OpKeywordSort (WS_Any) Block (WS_Any) OpListKeywordArg
                                | OpKeywordSort (WS_Many) VarScalar (WS_Many) OpListKeywordArg
                                | OpKeywordSort (WS_Any) OpListKeywordArgNonBrace

OpKeywordSpliceExpr           ::= OpKeywordSplice (WS_Any) OpListKeywordArg

OpKeywordSplitExpr            ::= OpKeywordSplit (WS_Any) OpListKeywordArg

OpKeywordSprintfExpr          ::= OpKeywordSprintf (WS_Any) OpListKeywordArg

OpKeywordSqrtExpr             ::= OpKeywordSqrt (WS_Any) OpUnaryKeywordArg
                                | OpKeywordSqrt

OpKeywordSrandExpr            ::= OpKeywordSrand (WS_Any) OpUnaryKeywordArg
                                | OpKeywordSrand

OpKeywordStatExpr             ::= OpKeywordStat (WS_Any) OpUnaryKeywordArg
                                | OpKeywordStat

OpKeywordStudyExpr            ::= OpKeywordStudy (WS_Any) OpUnaryKeywordArg
                                | OpKeywordStudy

OpKeywordSubExpr              ::= OpKeywordSub (WS_Any) SubDefinition

OpKeywordSubstrExpr           ::= OpKeywordSubstr (WS_Any) OpListKeywordArg

OpKeywordSymlinkExpr          ::= OpKeywordSymlink (WS_Any) OpListKeywordArg

OpKeywordSyscallExpr          ::= OpKeywordSyscall (WS_Any) OpListKeywordArg

OpKeywordSysopenExpr          ::= OpKeywordSysopen (WS_Any) OpListKeywordArg

OpKeywordSysreadExpr          ::= OpKeywordSysread (WS_Any) OpListKeywordArg

OpKeywordSysseekExpr          ::= OpKeywordSysseek (WS_Any) OpListKeywordArg

OpKeywordSyswriteExpr         ::= OpKeywordSyswrite (WS_Any) OpListKeywordArg

OpKeywordSystemExpr           ::= OpKeywordSystem (WS_Any) Block (WS_Any) OpListKeywordArg
                                | OpKeywordSystem (WS_Any) OpListKeywordArgNonBrace

OpKeywordTellExpr             ::= OpKeywordTell (WS_Any) OpUnaryKeywordArg
                                | OpKeywordTell

OpKeywordTelldirExpr          ::= OpKeywordTelldir (WS_Any) OpUnaryKeywordArg

OpKeywordTieExpr              ::= OpKeywordTie (WS_Any) OpListKeywordArg

OpKeywordTiedExpr             ::= OpKeywordTied (WS_Any) OpUnaryKeywordArg

OpKeywordTimeExpr             ::= OpKeywordTime

OpKeywordTimesExpr            ::= OpKeywordTimes

OpKeywordTruncateExpr         ::= OpKeywordTruncate (WS_Any) OpListKeywordArg

OpKeywordUcExpr               ::= OpKeywordUc (WS_Any) OpUnaryKeywordArg
                                | OpKeywordUc

OpKeywordUcfirstExpr          ::= OpKeywordUcfirst (WS_Any) OpUnaryKeywordArg
                                | OpKeywordUcfirst

OpKeywordUmaskExpr            ::= OpKeywordUmask (WS_Any) OpUnaryKeywordArg
                                | OpKeywordUmask

OpKeywordUndefExpr            ::= OpKeywordUndef (WS_Any) OpUnaryKeywordArg
                                | OpKeywordUndef

OpKeywordUnlinkExpr           ::= OpKeywordUnlink (WS_Any) OpUnaryKeywordArg
                                | OpKeywordUnlink

OpKeywordUnpackExpr           ::= OpKeywordUnpack (WS_Any) OpListKeywordArg

OpKeywordUnshiftExpr          ::= OpKeywordUnshift (WS_Any) OpListKeywordArg

OpKeywordUntieExpr            ::= OpKeywordUntie (WS_Any) OpUnaryKeywordArg

OpKeywordUtimeExpr            ::= OpKeywordUtime (WS_Any) OpUnaryKeywordArg

OpKeywordValuesExpr           ::= OpKeywordValues (WS_Any) OpUnaryKeywordArg

OpKeywordVecExpr              ::= OpKeywordVec (WS_Any) OpListKeywordArg

OpKeywordWaitExpr             ::= OpKeywordWait

OpKeywordWaitpidExpr          ::= OpKeywordWaitpid (WS_Any) OpListKeywordArg

OpKeywordWantarrayExpr        ::= OpKeywordWantarray

OpKeywordWarnExpr             ::= OpKeywordWarn (WS_Any) OpListKeywordArg
                                | OpKeywordWarn

OpKeywordWriteExpr            ::= OpKeywordWrite (WS_Any) OpListKeywordArg
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

QLikeValue ::= QLikeValueExpr | QLikeValueExprWithMods

QLikeValueExpr
    ~ QLikeFunction '(' NonRParenOrEscapedParens_Any               ')'
    | QLikeFunction '{' NonRBraceOrEscapedBraces_Any               '}'
    | QLikeFunction '<' NonRAngleOrEscapedAngles_Any               '>'
    | QLikeFunction '[' NonRBracketOrEscapedBrackets_Any           ']'
    | QLikeFunction '/' NonForwardSlashOrEscapedForwardSlashes_Any '/'
    | QLikeFunction '!' NonExclamPointOrEscapedExclamPoints_Any    '!'

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
    | QLikeSubstWithMods    '(' NonRParenOrEscapedParens_Any               ')(' NonRParenOrEscapedParens_Any               ')' RegexModifiers
    | QLikeSubstWithMods    '{' NonRBraceOrEscapedBraces_Any               '}{' NonRBraceOrEscapedBraces_Any               '}' RegexModifiers
    | QLikeSubstWithMods    '<' NonRAngleOrEscapedAngles_Any               '><' NonRAngleOrEscapedAngles_Any               '>' RegexModifiers
    | QLikeSubstWithMods    '[' NonRBracketOrEscapedBrackets_Any           '][' NonRBracketOrEscapedBrackets_Any             ']' RegexModifiers
    | QLikeSubstWithMods    '/' NonForwardSlashOrEscapedForwardSlashes_Any '/'  NonForwardSlashOrEscapedForwardSlashes_Any '/' RegexModifiers
    | QLikeSubstWithMods    '!' NonExclamPointOrEscapedExclamPoints_Any    '!'  NonExclamPointOrEscapedExclamPoints_Any    '!' RegexModifiers
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
LitNumberDec ~ Digits Period Digits
             | Digits Period
             | Period Digits
             | Digits

Period      ~ '.'
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

# Types of whitespaces
# WS_Any / WS_Many are used as rules within rules
WS_Any  ::= [\s]*
WS_Many ::= [\s]+

# Comments
:discard ~ <hash comment>
<hash comment>                    ~ <terminated hash comment> | <unterminated final hash comment>
<terminated hash comment>         ~ '#' <hash comment body> <vertical space char>
<unterminated final hash comment> ~ '#' <hash comment body>
<hash comment body>               ~ <hash comment char>*
<vertical space char>             ~ [\x{A}\x{B}\x{C}\x{D}\x{2028}\x{2029}]
<hash comment char>               ~ [^\x{A}\x{B}\x{C}\x{D}\x{2028}\x{2029}]

:lexeme ~ PhaseName              priority => 1
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

};

our $grammar = Marpa::R2::Scanless::G->new({ source => \$grammar_source });

sub build_struct {
    my ( $rec, $initial_valueref ) = @_;
    my @values = ($initial_valueref);

    while ( my $valueref = shift @values ) {
        if ( ! ref ${$valueref} ) {
            ${$valueref} = {
                'type'  => ':bare',
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
            'type'      => 'lexeme',
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
            for my $nterm (reverse qw/Program BlockStatement Statement NonBraceExprComma BlockLevelExpression Expression SubCall Ident/) {
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
