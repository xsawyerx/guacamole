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
                 | Block

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

UseStatement ::= OpKeywordUse Ident VersionExpr Expression
               | OpKeywordUse Ident VersionExpr
               | OpKeywordUse Ident Expression
               | OpKeywordUse VersionExpr
               | OpKeywordUse Ident

NoStatement ::= OpKeywordNo Ident VersionExpr Expression
              | OpKeywordNo Ident VersionExpr
              | OpKeywordNo Ident Expression
              | OpKeywordNo VersionExpr
              | OpKeywordNo Ident

RequireStatement ::= OpKeywordRequire VersionExpr
                   | OpKeywordRequire Ident
                   | OpKeywordRequire Expression

PackageStatement ::= OpKeywordPackage Ident VersionExpr Block
                   | OpKeywordPackage Ident Block
PackageDeclaration ::= OpKeywordPackage Ident VersionExpr
                     | OpKeywordPackage Ident

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
ExprFile0     ::= OpFile      ExprFile0                 | ExprShift0 action => ::first
ExprFileL     ::= OpFile      ExprFileL                 | ExprShiftL action => ::first
ExprFileR     ::= OpFile      ExprFileR                 | ExprShiftR action => ::first
ExprNeq0      ::= ExprFile0   OpInequal ExprFile0       | ExprFile0    action => ::first
ExprNeqL      ::= ExprFile0   OpInequal ExprFileL       | ExprFileL    action => ::first
ExprNeqR      ::= ExprFile0   OpInequal ExprFileR       | ExprFileR    action => ::first
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
NonBraceExprFile0     ::= OpFile      ExprFile0                 | NonBraceExprShift0 action => ::first
NonBraceExprFileL     ::= OpFile      ExprFileL                 | NonBraceExprShiftL action => ::first
NonBraceExprFileR     ::= OpFile      ExprFileR                 | NonBraceExprShiftR action => ::first
NonBraceExprNeq0      ::= NonBraceExprFile0   OpInequal ExprFile0       | NonBraceExprFile0    action => ::first
NonBraceExprNeqL      ::= NonBraceExprFile0   OpInequal ExprFileL       | NonBraceExprFileL    action => ::first
NonBraceExprNeqR      ::= NonBraceExprFile0   OpInequal ExprFileR       | NonBraceExprFileR    action => ::first
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

NonLiteral ::= GlobalVariable
             | Variable
             | DerefVariable
             | Modifier Variable
             | Modifier ParenExpr
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
CoreSubLetters   ~ [a-zA-z0-9_]*

# SubNameNonQLike is for function calls
# They are not allowed to be:
# q / qq / qw / qr / qx
# s / m / y / tr
SubNameNonQLike ~
                  NonQLikeLetters                      # [non-qlike]
                | NonQLikeLetters AllSubLetters        # [non-qlike][*]
                | QLetter NonQRWXLetters               # q[non-qrwx]
                | QLetter NonQRWXLetters AllSubLetters # q[non-qrwx][*]
                | QLetter QLetter AllSubLetters        # qq[*]
                | QLetter RLetter AllSubLetters        # qr[*]
                | QLetter WLetter AllSubLetters        # qw[*]
                | QLetter XLetter AllSubLetters        # qx[*]
                | TLetter                              # t
                | TLetter NonRLetter                   # t[non-r]
                | TLetter NonRLetter AllSubLetters     # t[non-r][*]
                | TLetter RLetter AllSubLetters        # tr[*]
                | SLetter AllSubLetters                # s[*]
                | MLetter AllSubLetters                # m[*]
                | YLetter AllSubLetters                # y[*]

# These are sort of the same but not
# Idents are defined differently for different purposes
# Subroutine names are defined using one ident
# Calling subroutines with arrow are defined using another ident
# Variables are defined using a different ident
# Namespaced variables ($x::y) are defined with another ident
Ident ::= SubNameExpr

CallArgs ::= ParenExpr

Block ::= LBrace RBrace
        | LBrace StatementSeq RBrace

ArrayElem ::= LBracket Expression RBracket

HashElem ::= LBrace Expression RBrace

NonBraceLiteral ::= LitNumber
                  | LitArray
                  | LitString
                  | InterpolString

Literal         ::= NonBraceLiteral
                  | LitHash

LitArray       ::= LBracket Expression RBracket
                 | LBracket RBracket

LitHash        ::= LBrace Expression RBrace
                 | LBrace RBrace

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

DerefVariableSlice ::= '@[' Expression ']'
                     | '@{' Expression '}'
                     | '%[' Expression ']'
                     | '%{' Expression '}'

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

OpKeywordAbsExpr              ::= OpKeywordAbs OpUnaryKeywordArg
                                | OpKeywordAbs

OpKeywordAcceptExpr           ::= OpKeywordAccept OpListKeywordArg

OpKeywordAlarmExpr            ::= OpKeywordAlarm OpUnaryKeywordArg
                                | OpKeywordAlarm

OpKeywordAtan2Expr            ::= OpKeywordAtan2 OpListKeywordArg

OpKeywordBindExpr             ::= OpKeywordBind OpListKeywordArg

OpKeywordBinmodeExpr          ::= OpKeywordBinmode OpListKeywordArg

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

OpKeywordDoExpr               ::= OpKeywordDo Block
                                | OpKeywordDo OpUnaryKeywordArgNonBrace

OpKeywordDumpExpr             ::= OpKeywordDump OpAssignKeywordArg
                                | OpKeywordDump Label
                                | OpKeywordDump

OpKeywordEachExpr             ::= OpKeywordEach OpUnaryKeywordArg

OpKeywordEofExpr              ::= OpKeywordEof OpUnaryKeywordArg
                                | OpKeywordEof

OpKeywordEvalExpr             ::= OpKeywordEval Block

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

OpKeywordForkExpr             ::= OpKeywordFork

OpKeywordGetcExpr             ::= OpKeywordGetc OpUnaryKeywordArg
                                | OpKeywordGetc

OpKeywordGetloginExpr         ::= OpKeywordGetlogin

OpKeywordGetpeernameExpr      ::= OpKeywordGetpeername OpUnaryKeywordArg

OpKeywordGetpgrpExpr          ::= OpKeywordGetpgrp OpUnaryKeywordArg

OpKeywordGetppidExpr          ::= OpKeywordGetppid

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

OpKeywordGetpwentExpr         ::= OpKeywordGetpwent

OpKeywordGetgrentExpr         ::= OpKeywordGetgrent

OpKeywordGethostentExpr       ::= OpKeywordGethostent

OpKeywordGetnetentExpr        ::= OpKeywordGetnetent

OpKeywordGetprotoentExpr      ::= OpKeywordGetprotoent

OpKeywordGetserventExpr       ::= OpKeywordGetservent

OpKeywordSetpwentExpr         ::= OpKeywordSetpwent

OpKeywordSetgrentExpr         ::= OpKeywordSetgrent

OpKeywordSethostentExpr       ::= OpKeywordSethostent OpUnaryKeywordArg

OpKeywordSetnetentExpr        ::= OpKeywordSetnetent OpUnaryKeywordArg

OpKeywordSetprotoentExpr      ::= OpKeywordSetprotoent OpUnaryKeywordArg

OpKeywordSetserventExpr       ::= OpKeywordSetservent OpUnaryKeywordArg

OpKeywordEndpwentExpr         ::= OpKeywordEndpwent

OpKeywordEndgrentExpr         ::= OpKeywordEndgrent

OpKeywordEndhostentExpr       ::= OpKeywordEndhostent

OpKeywordEndnetentExpr        ::= OpKeywordEndnetent

OpKeywordEndprotoentExpr      ::= OpKeywordEndprotoent

OpKeywordEndserventExpr       ::= OpKeywordEndservent

OpKeywordExecExpr             ::= OpKeywordExec Block OpListKeywordArg
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

OpKeywordGrepExpr             ::= OpKeywordGrep Block OpListKeywordArg
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

OpKeywordMapExpr              ::= OpKeywordMap Block OpListKeywordArg
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

OpKeywordPrintExpr            ::= OpKeywordPrint Block OpListKeywordArg
                                | OpKeywordPrint BuiltinFilehandle OpListKeywordArgNonBrace
                                | OpKeywordPrint BuiltinFilehandle
                                | OpKeywordPrint OpListKeywordArgNonBrace
                                | OpKeywordPrint Block
                                | OpKeywordPrint

OpKeywordPrintfExpr           ::= OpKeywordPrintf Block OpListKeywordArg
                                | OpKeywordPrintf BuiltinFilehandle OpListKeywordArgNonBrace
                                | OpKeywordPrintf BuiltinFilehandle
                                | OpKeywordPrintf OpListKeywordArgNonBrace
                                | OpKeywordPrintf Block

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

OpKeywordSayExpr              ::= OpKeywordSay Block OpListKeywordArg
                                | OpKeywordSay BuiltinFilehandle OpListKeywordArgNonBrace
                                | OpKeywordSay BuiltinFilehandle
                                | OpKeywordSay OpListKeywordArgNonBrace
                                | OpKeywordSay Block
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

OpKeywordSortExpr             ::= OpKeywordSort Block OpListKeywordArg
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

OpKeywordSystemExpr           ::= OpKeywordSystem Block OpListKeywordArg
                                | OpKeywordSystem OpListKeywordArgNonBrace

OpKeywordTellExpr             ::= OpKeywordTell OpUnaryKeywordArg
                                | OpKeywordTell

OpKeywordTelldirExpr          ::= OpKeywordTelldir OpUnaryKeywordArg

OpKeywordTieExpr              ::= OpKeywordTie OpListKeywordArg

OpKeywordTiedExpr             ::= OpKeywordTied OpUnaryKeywordArg

OpKeywordTimeExpr             ::= OpKeywordTime

OpKeywordTimesExpr            ::= OpKeywordTimes

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

OpKeywordWaitExpr             ::= OpKeywordWait

OpKeywordWaitpidExpr          ::= OpKeywordWaitpid OpListKeywordArg

OpKeywordWantarrayExpr        ::= OpKeywordWantarray

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

# Letters
QLetter ~ 'q'
RLetter ~ 'r'
WLetter ~ 'w'
XLetter ~ 'x'
SLetter ~ 's'
MLetter ~ 'm'
TLetter ~ 't'
YLetter ~ 'y'

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

:lexeme ~ PhaseName              priority => 1
:lexeme ~ QLikeValueExpr         priority => 1
:lexeme ~ QLikeValueExprWithMods priority => 1
:lexeme ~ VersionNumber          priority => 1

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
