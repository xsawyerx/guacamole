package Guacamole;
use strict;
use warnings;
use Marpa::R2;

my $grammar_source = q{
lexeme default = latm => 1
:default ::= action => [ name, values ]

Program ::= StatementSeq

StatementSeq ::= Statement
               | Statement Semicolon
               | Statement Semicolon StatementSeq

Statement ::= BlockLevelExpression StatementModifier
            | BlockLevelExpression
            | LoopStatement
            | Block
            | Condition
            | EllipsisStatement
            | UseStatement
            | NoStatement
            | RequireStatement
            | PackageStatement
            | SubStatement

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
                   | OpKeywordPackage Ident VersionExpr
                   | OpKeywordPackage Ident Block
                   | OpKeywordPackage Ident

SubStatement ::= PhaseStatement Block
               | OpKeywordSub PhaseStatement Block
               | OpKeywordSub Ident SubDefinition
               | OpKeywordSub Ident

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

ExprValue    ::= Value
ExprArrow    ::= ExprArrow OpArrow ArrowRHS | ExprValue action => ::first
ExprInc      ::= OpInc ExprArrow | ExprArrow OpInc |  ExprArrow action => ::first
ExprPower    ::= ExprInc OpPower ExprPower | ExprInc action => ::first
ExprUnary    ::= OpUnary ExprUnary | ExprPower action => ::first
ExprRegex    ::= ExprRegex OpRegex ExprUnary | ExprUnary action => ::first
ExprMul      ::= ExprMul OpMulti ExprRegex | ExprRegex action => ::first
ExprAdd      ::= ExprAdd OpAdd ExprMul | ExprMul action => ::first
ExprShift    ::= ExprShift OpShift ExprAdd | ExprAdd action => ::first
ExprKwUnary  ::= OpUnaryKeywordExpr | ExprShift action => ::first
ExprFile     ::= OpFile ExprFile | ExprKwUnary action => ::first
ExprNeq      ::= ExprFile OpInequal ExprFile | ExprFile action => ::first
ExprEq       ::= ExprNeq OpEqual ExprNeq | ExprNeq action => ::first
ExprBinAnd   ::= ExprBinAnd OpBinAnd ExprEq | ExprEq action => ::first
ExprBinOr    ::= ExprBinOr OpBinOr ExprBinAnd | ExprBinAnd action => ::first
ExprLogAnd   ::= ExprLogAnd OpLogAnd ExprBinOr | ExprBinOr action => ::first
ExprLogOr    ::= ExprLogOr OpLogOr ExprLogAnd | ExprLogAnd action => ::first
ExprRange    ::= ExprLogOr OpRange ExprLogOr | ExprLogOr action => ::first
ExprCond     ::= ExprRange OpTriThen ExprRange OpTriElse ExprCond | ExprRange action => ::first
ExprAssign   ::= ExprCond OpAssign ExprAssign | ExprCond action => ::first
ExprKwAssign ::= OpAssignKeywordExpr | ExprAssign action => ::first
ExprComma    ::= ExprComma OpComma ExprKwAssign | ExprKwAssign action => ::first
ExprKwList   ::= OpListKeywordExpr | ExprComma action => ::first
ExprNameNot  ::= OpNameNot ExprNameNot | ExprKwList action => ::first
ExprNameAnd  ::= ExprNameAnd OpNameAnd ExprNameNot | ExprNameNot action => ::first
ExprNameOr   ::= ExprNameOr OpNameOr ExprNameAnd | ExprNameAnd action => ::first
Expression   ::= ExprNameOr action => ::first

NonBraceExprValue    ::= NonBraceValue
NonBraceExprArrow    ::= NonBraceExprArrow OpArrow ArrowRHS | NonBraceExprValue action => ::first
NonBraceExprInc      ::= OpInc ExprArrow | NonBraceExprArrow OpInc | NonBraceExprArrow action => ::first
NonBraceExprPower    ::= NonBraceExprInc OpPower ExprPower | NonBraceExprInc action => ::first
NonBraceExprUnary    ::= OpUnary ExprUnary | NonBraceExprPower action => ::first
NonBraceExprRegex    ::= NonBraceExprRegex OpRegex ExprUnary | NonBraceExprUnary action => ::first
NonBraceExprMul      ::= NonBraceExprMul OpMulti ExprRegex | NonBraceExprRegex action => ::first
NonBraceExprAdd      ::= NonBraceExprAdd OpAdd ExprMul | NonBraceExprMul action => ::first
NonBraceExprShift    ::= NonBraceExprShift OpShift ExprAdd | NonBraceExprAdd action => ::first
NonBraceExprKwUnary  ::= OpUnaryKeywordExpr | NonBraceExprShift action => ::first
NonBraceExprFile     ::= OpFile ExprFile | NonBraceExprKwUnary action => ::first
NonBraceExprNeq      ::= NonBraceExprFile OpInequal ExprFile | NonBraceExprFile action => ::first
NonBraceExprEq       ::= NonBraceExprNeq OpEqual ExprNeq | NonBraceExprNeq action => ::first
NonBraceExprBinAnd   ::= NonBraceExprBinAnd OpBinAnd ExprEq | NonBraceExprEq action => ::first
NonBraceExprBinOr    ::= NonBraceExprBinOr OpBinOr ExprBinAnd | NonBraceExprBinAnd action => ::first
NonBraceExprLogAnd   ::= NonBraceExprLogAnd OpLogAnd ExprBinOr | NonBraceExprBinOr action => ::first
NonBraceExprLogOr    ::= NonBraceExprLogOr OpLogOr ExprLogAnd | NonBraceExprLogAnd action => ::first
NonBraceExprRange    ::= NonBraceExprLogOr OpRange ExprLogOr | NonBraceExprLogOr action => ::first
NonBraceExprCond     ::= NonBraceExprRange OpTriThen ExprRange OpTriElse ExprCond | NonBraceExprRange action => ::first
NonBraceExprAssign   ::= NonBraceExprCond OpAssign ExprAssign | NonBraceExprCond action => ::first
NonBraceExprKwAssign ::= OpAssignKeywordExpr | NonBraceExprAssign action => ::first
NonBraceExprComma    ::= NonBraceExprComma OpComma ExprKwAssign | NonBraceExprKwAssign action => ::first
NonBraceExprKwList   ::= OpListKeywordExpr | NonBraceExprComma action => ::first

# Comma is only allowed if it follows a keyword operator, to avoid block/hash disambiguation in perl.
BlockLevelExprKwList ::= OpListKeywordExpr | NonBraceExprAssign action => ::first
BlockLevelExprNameNot ::= OpNameNot ExprNameNot | BlockLevelExprKwList action => ::first
BlockLevelExprNameAnd ::= BlockLevelExprNameAnd OpNameAnd ExprNameNot | BlockLevelExprNameNot action => ::first
BlockLevelExprNameOr ::= BlockLevelExprNameOr OpNameOr ExprNameAnd | BlockLevelExprNameAnd action => ::first
BlockLevelExpression ::= BlockLevelExprNameOr action => ::first

Value         ::= Literal | NonLiteral | QLikeValue

# Same as Value above, but with a NonBraceLiteral
NonBraceValue ::= NonBraceLiteral | NonLiteral | QLikeValue

NonLiteral ::= Variable action => ::first
             | Modifier Variable action => ::first
             | Modifier ParenExpr action => ::first
             | UnderscoreValues action => ::first
             | SubCall action => ::first
             | ParenExpr action => ::first
             | OpNullaryKeywordExpr action => ::first

ParenExpr ::= LParen Expression RParen
            | LParen RParen # support ()

Modifier  ::= OpKeywordMy | OpKeywordOur | OpKeywordLocal | OpKeywordState

# UnderscoreData and UnderscoreEnd are not values
UnderscoreValues ::= UnderscorePackage
                   | UnderscoreFile
                   | UnderscoreLine
                   | UnderscoreSub

UnderscoreTokens ::= UnderscoreValues
                   | UnderscoreData
                   | UnderscoreEnd

Variable ::= VarScalar
           | VarArray
           | VarHash
           | VarCode
           | VarGlob
           | VarArrayTop

VarScalar   ::= SigilScalar VarName
VarArray    ::= SigilArray VarName
VarHash     ::= SigilHash VarName
VarCode     ::= SigilCode VarName
VarGlob     ::= SigilGlob VarName
VarArrayTop ::= SigilArrayTop VarName

VarName ::= Ident
          | VarScalar

SubCall ::= NonQLikeIdent CallArgs
          | VarCode CallArgs

NonQLikeIdent ::= NonQLikeFunctionName
                | NonQLikeFunctionName Ident

CallArgs ::= ParenExpr

Block ::= LBrace RBrace
        | LBrace StatementSeq RBrace

ArrayElem ::= LBracket Expression RBracket

HashElem ::= LBrace Expression RBrace

Ident ::= IdentComp
        | IdentComp PackageSep Ident
        | Ident PackageSep

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
           | ArrowMethodCall
           | ArrowIndirectCall
           | ArrayElem
           | HashElem

ArrowDerefCall    ::= CallArgs
ArrowMethodCall   ::= Ident CallArgs
ArrowIndirectCall ::= SigilScalar Ident CallArgs

# TODO: (Add the following above)
#| OpKeywordSplitExpr

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

OpKeywordAbsExpr              ::= OpKeywordAbs ExprKwUnary
                                | OpKeywordAbs

OpKeywordAcceptExpr           ::= OpKeywordAccept ExprKwList

OpKeywordAlarmExpr            ::= OpKeywordAlarm ExprKwUnary
                                | OpKeywordAlarm

OpKeywordAtan2Expr            ::= OpKeywordAtan2 ExprKwList

OpKeywordBindExpr             ::= OpKeywordBind ExprKwList

OpKeywordBinmodeExpr          ::= OpKeywordBinmode ExprKwList

OpKeywordBlessExpr            ::= OpKeywordBless ExprKwList

OpKeywordBreakExpr            ::= OpKeywordBreak Label
                                | OpKeywordBreak

OpKeywordCallerExpr           ::= OpKeywordCaller ExprKwUnary
                                | OpKeywordCaller

OpKeywordChdirExpr            ::= OpKeywordChdir ExprKwUnary
                                | OpKeywordChdir

OpKeywordChmodExpr            ::= OpKeywordChmod ExprKwList

OpKeywordChompExpr            ::= OpKeywordChomp ExprKwUnary
                                | OpKeywordChomp

OpKeywordChopExpr             ::= OpKeywordChop ExprKwUnary
                                | OpKeywordChop

OpKeywordChownExpr            ::= OpKeywordChown ExprKwList

OpKeywordChrExpr              ::= OpKeywordChr ExprKwUnary
                                | OpKeywordChr

OpKeywordChrootExpr           ::= OpKeywordChroot ExprKwUnary
                                | OpKeywordChroot

OpKeywordCloseExpr            ::= OpKeywordClose ExprKwUnary
                                | OpKeywordClose

OpKeywordClosedirExpr         ::= OpKeywordClosedir ExprKwUnary

OpKeywordConnectExpr          ::= OpKeywordConnect ExprKwList

OpKeywordCosExpr              ::= OpKeywordCos ExprKwUnary

OpKeywordCryptExpr            ::= OpKeywordCrypt ExprKwList

OpKeywordDbmcloseExpr         ::= OpKeywordDbmclose ExprKwUnary

OpKeywordDbmopenExpr          ::= OpKeywordDbmopen ExprKwList

OpKeywordDefinedExpr          ::= OpKeywordDefined ExprKwUnary
                                | OpKeywordDefined

OpKeywordDeleteExpr           ::= OpKeywordDelete ExprKwUnary

OpKeywordDieExpr              ::= OpKeywordDie ExprKwList

OpKeywordDoExpr               ::= OpKeywordDo Block
                                | OpKeywordDo NonBraceExprKwUnary

OpKeywordDumpExpr             ::= OpKeywordDump ExprKwAssign
                                | OpKeywordDump Label
                                | OpKeywordDump

OpKeywordEachExpr             ::= OpKeywordEach ExprKwUnary

OpKeywordEofExpr              ::= OpKeywordEof ExprKwUnary
                                | OpKeywordEof

OpKeywordEvalExpr             ::= OpKeywordEval Block

OpKeywordEvalbytesExpr        ::= OpKeywordEvalbytes ExprKwUnary
                                | OpKeywordEvalbytes

OpKeywordExistsExpr           ::= OpKeywordExists ExprKwUnary

OpKeywordExitExpr             ::= OpKeywordExit ExprKwUnary
                                | OpKeywordExit

OpKeywordExpExpr              ::= OpKeywordExp ExprKwUnary
                                | OpKeywordExp

OpKeywordFcExpr               ::= OpKeywordFc ExprKwUnary
                                | OpKeywordFc

OpKeywordFcntlExpr            ::= OpKeywordFcntl ExprKwList

OpKeywordFilenoExpr           ::= OpKeywordFileno ExprKwUnary

OpKeywordFlockExpr            ::= OpKeywordFlock ExprKwList

OpKeywordForkExpr             ::= OpKeywordFork

OpKeywordGetcExpr             ::= OpKeywordGetc ExprKwUnary
                                | OpKeywordGetc

OpKeywordGetloginExpr         ::= OpKeywordGetlogin

OpKeywordGetpeernameExpr      ::= OpKeywordGetpeername ExprKwUnary

OpKeywordGetpgrpExpr          ::= OpKeywordGetpgrp ExprKwUnary

OpKeywordGetppidExpr          ::= OpKeywordGetppid

OpKeywordGetpriorityExpr      ::= OpKeywordGetpriority ExprKwList

OpKeywordGetpwnamExpr         ::= OpKeywordGetpwnam ExprKwUnary

OpKeywordGetgrnamExpr         ::= OpKeywordGetgrnam ExprKwUnary

OpKeywordGethostbynameExpr    ::= OpKeywordGethostbyname ExprKwUnary

OpKeywordGetnetbynameExpr     ::= OpKeywordGetnetbyname ExprKwUnary

OpKeywordGetprotobynameExpr   ::= OpKeywordGetprotobyname ExprKwUnary

OpKeywordGetpwuidExpr         ::= OpKeywordGetpwuid ExprKwUnary

OpKeywordGetgrgidExpr         ::= OpKeywordGetgrgid ExprKwUnary

OpKeywordGetservbynameExpr    ::= OpKeywordGetservbyname ExprKwList

OpKeywordGethostbyaddrExpr    ::= OpKeywordGethostbyaddr ExprKwList

OpKeywordGetnetbyaddrExpr     ::= OpKeywordGetnetbyaddr ExprKwList

OpKeywordGetprotobynumberExpr ::= OpKeywordGetprotobynumber ExprKwUnary

OpKeywordGetservbyportExpr    ::= OpKeywordGetservbyport ExprKwList

OpKeywordGetpwentExpr         ::= OpKeywordGetpwent

OpKeywordGetgrentExpr         ::= OpKeywordGetgrent

OpKeywordGethostentExpr       ::= OpKeywordGethostent

OpKeywordGetnetentExpr        ::= OpKeywordGetnetent

OpKeywordGetprotoentExpr      ::= OpKeywordGetprotoent

OpKeywordGetserventExpr       ::= OpKeywordGetservent

OpKeywordSetpwentExpr         ::= OpKeywordSetpwent

OpKeywordSetgrentExpr         ::= OpKeywordSetgrent

OpKeywordSethostentExpr       ::= OpKeywordSethostent ExprKwUnary

OpKeywordSetnetentExpr        ::= OpKeywordSetnetent ExprKwUnary

OpKeywordSetprotoentExpr      ::= OpKeywordSetprotoent ExprKwUnary

OpKeywordSetserventExpr       ::= OpKeywordSetservent ExprKwUnary

OpKeywordEndpwentExpr         ::= OpKeywordEndpwent

OpKeywordEndgrentExpr         ::= OpKeywordEndgrent

OpKeywordEndhostentExpr       ::= OpKeywordEndhostent

OpKeywordEndnetentExpr        ::= OpKeywordEndnetent

OpKeywordEndprotoentExpr      ::= OpKeywordEndprotoent

OpKeywordEndserventExpr       ::= OpKeywordEndservent

OpKeywordExecExpr             ::= OpKeywordExec Block ExprKwList
                                | OpKeywordExec NonBraceExprKwList

OpKeywordGetsocknameExpr      ::= OpKeywordGetsockname ExprKwUnary

OpKeywordGetsockoptExpr       ::= OpKeywordGetsockopt ExprKwList

OpKeywordGlobExpr             ::= OpKeywordGlob ExprKwList
                                | OpKeywordGlob

OpKeywordGmtimeExpr           ::= OpKeywordGmtime ExprKwUnary
                                | OpKeywordGmtime

# &NAME is an expression too
OpKeywordGotoExpr             ::= OpKeywordGoto ExprKwAssign
                                | OpKeywordGoto Label

OpKeywordGrepExpr             ::= OpKeywordGrep Block ExprKwList
                                | OpKeywordGrep NonBraceExprKwList

OpKeywordHexExpr              ::= OpKeywordHex ExprKwUnary
                                | OpKeywordHex

OpKeywordIndexExpr            ::= OpKeywordIndex ExprKwList

OpKeywordIntExpr              ::= OpKeywordInt ExprKwUnary
                                | OpKeywordInt

OpKeywordIoctlExpr            ::= OpKeywordIoctl ExprKwList

OpKeywordJoinExpr             ::= OpKeywordJoin ExprKwList

OpKeywordKeysExpr             ::= OpKeywordKeys ExprKwUnary

OpKeywordKillExpr             ::= OpKeywordKill ExprKwList
                                | OpKeywordKill Expression

OpKeywordLastExpr             ::= OpKeywordLast ExprKwAssign
                                | OpKeywordLast Label
                                | OpKeywordLast

OpKeywordLcExpr               ::= OpKeywordLc ExprKwUnary
                                | OpKeywordLc

OpKeywordLcfirstExpr          ::= OpKeywordLcfirst ExprKwUnary
                                | OpKeywordLcfirst

OpKeywordLengthExpr           ::= OpKeywordLength ExprKwUnary
                                | OpKeywordLength

OpKeywordLinkExpr             ::= OpKeywordLink ExprKwList

OpKeywordListenExpr           ::= OpKeywordListen ExprKwList

OpKeywordLocaltimeExpr        ::= OpKeywordLocaltime ExprKwUnary
                                | OpKeywordLocaltime

OpKeywordLockExpr             ::= OpKeywordLock ExprKwUnary

OpKeywordLogExpr              ::= OpKeywordLog ExprKwUnary
                                | OpKeywordLog

OpKeywordLstatExpr            ::= OpKeywordLstat ExprKwUnary
                                | OpKeywordLstat

OpKeywordMapExpr              ::= OpKeywordMap Block ExprKwList
                                | OpKeywordMap NonBraceExprKwList

OpKeywordMkdirExpr            ::= OpKeywordMkdir ExprKwList
                                | OpKeywordMkdir

OpKeywordMsgctlExpr           ::= OpKeywordMsgctl ExprKwList

OpKeywordMsggetExpr           ::= OpKeywordMsgget ExprKwList

OpKeywordMsgrcvExpr           ::= OpKeywordMsgrcv ExprKwList

OpKeywordMsgsndExpr           ::= OpKeywordMsgsnd ExprKwList

OpKeywordNextExpr             ::= OpKeywordNext ExprKwAssign
                                | OpKeywordNext Label
                                | OpKeywordNext

OpKeywordOctExpr              ::= OpKeywordOct ExprKwUnary
                                | OpKeywordOct

OpKeywordOpenExpr             ::= OpKeywordOpen ExprKwList

OpKeywordOpendirExpr          ::= OpKeywordOpendir ExprKwList

OpKeywordOrdExpr              ::= OpKeywordOrd ExprKwUnary
                                | OpKeywordOrd

OpKeywordPackExpr             ::= OpKeywordPack ExprKwList

OpKeywordPipeExpr             ::= OpKeywordPipe ExprKwList

OpKeywordPopExpr              ::= OpKeywordPop ExprKwUnary
                                | OpKeywordPop

OpKeywordPosExpr              ::= OpKeywordPos ExprKwUnary
                                | OpKeywordPos

OpKeywordPrintExpr            ::= OpKeywordPrint Block ExprKwList
                                | OpKeywordPrint NonBraceExprKwList
                                | OpKeywordPrint Block
                                | OpKeywordPrint

OpKeywordPrintfExpr           ::= OpKeywordPrintf Block ExprKwList
                                | OpKeywordPrintf NonBraceExprKwList
                                | OpKeywordPrintf Block

OpKeywordPrototypeExpr        ::= OpKeywordPrototype ExprKwUnary
                                | OpKeywordPrototype

OpKeywordPushExpr             ::= OpKeywordPush ExprKwList

OpKeywordQuotemetaExpr        ::= OpKeywordQuotemeta ExprKwUnary
                                | OpKeywordQuotemeta

OpKeywordRandExpr             ::= OpKeywordRand ExprKwUnary
                                | OpKeywordRand

OpKeywordReadExpr             ::= OpKeywordRead ExprKwList

OpKeywordReaddirExpr          ::= OpKeywordReaddir ExprKwUnary

OpKeywordReadlineExpr         ::= OpKeywordReadline ExprKwUnary
                                | OpKeywordReadline

OpKeywordReadlinkExpr         ::= OpKeywordReadlink ExprKwUnary
                                | OpKeywordReadlink

OpKeywordReadpipeExpr         ::= OpKeywordReadpipe ExprKwUnary
                                | OpKeywordReadpipe

OpKeywordRecvExpr             ::= OpKeywordRecv ExprKwList

OpKeywordRedoExpr             ::= OpKeywordRedo ExprKwAssign
                                | OpKeywordRedo Label
                                | OpKeywordRedo

OpKeywordRefExpr              ::= OpKeywordRef ExprKwUnary
                                | OpKeywordRef

OpKeywordRenameExpr           ::= OpKeywordRename ExprKwList

OpKeywordResetExpr            ::= OpKeywordReset ExprKwUnary
                                | OpKeywordReset

OpKeywordReturnExpr           ::= OpKeywordReturn ExprKwList
                                | OpKeywordReturn

OpKeywordReverseExpr          ::= OpKeywordReverse ExprKwList

OpKeywordRewinddirExpr        ::= OpKeywordRewinddir ExprKwUnary
                                | OpKeywordRewinddir

OpKeywordRindexExpr           ::= OpKeywordRindex ExprKwList
                                | OpKeywordRindex

OpKeywordRmdirExpr            ::= OpKeywordRmdir ExprKwUnary
                                | OpKeywordRmdir

OpKeywordSayExpr              ::= OpKeywordSay Block ExprKwList
                                | OpKeywordSay NonBraceExprKwList
                                | OpKeywordSay Block
                                | OpKeywordSay

OpKeywordScalarExpr           ::= OpKeywordScalar ExprKwUnary

OpKeywordSeekExpr             ::= OpKeywordSeek ExprKwList

OpKeywordSeekdirExpr          ::= OpKeywordSeekdir ExprKwList

OpKeywordSelectExpr           ::= OpKeywordSelect ExprKwList

OpKeywordSemctlExpr           ::= OpKeywordSemctl ExprKwList

OpKeywordSemgetExpr           ::= OpKeywordSemget ExprKwList

OpKeywordSemopExpr            ::= OpKeywordSemop ExprKwList

OpKeywordSendExpr             ::= OpKeywordSend ExprKwList

OpKeywordSetpgrpExpr          ::= OpKeywordSetpgrp ExprKwList

OpKeywordSetpriorityExpr      ::= OpKeywordSetpriority ExprKwList

OpKeywordSetsockoptExpr       ::= OpKeywordSetsockopt ExprKwList

OpKeywordShiftExpr            ::= OpKeywordShift ExprKwUnary
                                | OpKeywordShift

OpKeywordShmctlExpr           ::= OpKeywordShmctl ExprKwList

OpKeywordShmgetExpr           ::= OpKeywordShmget ExprKwList

OpKeywordShmreadExpr          ::= OpKeywordShmread ExprKwList

OpKeywordShmwriteExpr         ::= OpKeywordShmwrite ExprKwList

OpKeywordShutdownExpr         ::= OpKeywordShutdown ExprKwList

OpKeywordSinExpr              ::= OpKeywordSin ExprKwUnary
                                | OpKeywordSin

OpKeywordSleepExpr            ::= OpKeywordSleep ExprKwUnary
                                | OpKeywordSleep

OpKeywordSocketExpr           ::= OpKeywordSocket ExprKwList

OpKeywordSocketpairExpr       ::= OpKeywordSocketpair ExprKwList

OpKeywordSortExpr             ::= OpKeywordSort Block ExprKwList
                                | OpKeywordSort VarScalar ExprKwList
                                | OpKeywordSort NonBraceExprKwList

OpKeywordSpliceExpr           ::= OpKeywordSplice ExprKwList

OpKeywordSplitExpr            ::= OpKeywordSplit ExprKwList

OpKeywordSprintfExpr          ::= OpKeywordSprintf ExprKwList

OpKeywordSqrtExpr             ::= OpKeywordSqrt ExprKwUnary
                                | OpKeywordSqrt

OpKeywordSrandExpr            ::= OpKeywordSrand ExprKwUnary
                                | OpKeywordSrand

OpKeywordStatExpr             ::= OpKeywordStat ExprKwUnary
                                | OpKeywordStat

OpKeywordStudyExpr            ::= OpKeywordStudy ExprKwUnary
                                | OpKeywordStudy

OpKeywordSubExpr              ::= OpKeywordSub SubDefinition

OpKeywordSubstrExpr           ::= OpKeywordSubstr ExprKwList

OpKeywordSymlinkExpr          ::= OpKeywordSymlink ExprKwList

OpKeywordSyscallExpr          ::= OpKeywordSyscall ExprKwList

OpKeywordSysopenExpr          ::= OpKeywordSysopen ExprKwList

OpKeywordSysreadExpr          ::= OpKeywordSysread ExprKwList

OpKeywordSysseekExpr          ::= OpKeywordSysseek ExprKwList

OpKeywordSyswriteExpr         ::= OpKeywordSyswrite ExprKwList

OpKeywordSystemExpr           ::= OpKeywordSystem Block ExprKwList
                                | OpKeywordSystem NonBraceExprKwList

OpKeywordTellExpr             ::= OpKeywordTell ExprKwUnary
                                | OpKeywordTell

OpKeywordTelldirExpr          ::= OpKeywordTelldir ExprKwUnary

OpKeywordTieExpr              ::= OpKeywordTie ExprKwList

OpKeywordTiedExpr             ::= OpKeywordTied ExprKwUnary

OpKeywordTimeExpr             ::= OpKeywordTime

OpKeywordTimesExpr            ::= OpKeywordTimes

OpKeywordTruncateExpr         ::= OpKeywordTruncate ExprKwList

OpKeywordUcExpr               ::= OpKeywordUc ExprKwUnary
                                | OpKeywordUc

OpKeywordUcfirstExpr          ::= OpKeywordUcfirst ExprKwUnary
                                | OpKeywordUcfirst

OpKeywordUmaskExpr            ::= OpKeywordUmask ExprKwUnary
                                | OpKeywordUmask

OpKeywordUndefExpr            ::= OpKeywordUndef ExprKwUnary
                                | OpKeywordUndef

OpKeywordUnlinkExpr           ::= OpKeywordUnlink ExprKwUnary
                                | OpKeywordUnlink

OpKeywordUnpackExpr           ::= OpKeywordUnpack ExprKwList

OpKeywordUnshiftExpr          ::= OpKeywordUnshift ExprKwList

OpKeywordUntieExpr            ::= OpKeywordUntie ExprKwUnary

OpKeywordUtimeExpr            ::= OpKeywordUtime ExprKwUnary

OpKeywordValuesExpr           ::= OpKeywordValues ExprKwUnary

OpKeywordVecExpr              ::= OpKeywordVec ExprKwList

OpKeywordWaitExpr             ::= OpKeywordWait

OpKeywordWaitpidExpr          ::= OpKeywordWaitpid ExprKwList

OpKeywordWantarrayExpr        ::= OpKeywordWantarray

OpKeywordWarnExpr             ::= OpKeywordWarn ExprKwList
                                | OpKeywordWarn

OpKeywordWriteExpr            ::= OpKeywordWrite ExprKwList
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

QLikeValue ::= QLikeValueExpr

QLikeValueExpr ~ QLikeFunction '(' NonRParenOrEscapedParens_Many               ')'
               | QLikeFunction '{' NonRBraceOrEscapedBraces_Many               '}'
               | QLikeFunction '<' NonRAngleOrEscapedAngles_Many               '>'
               | QLikeFunction '[' NonRBracketOrEscapedBrackets_Many           ']'
               | QLikeFunction '/' NonForwardSlashOrEscapedForwardSlashes_Many '/'
               | QLikeFunction '!' NonExclamPointOrEscapedExclamPoints_Many    '!'
               | QLikeFunction '()'
               | QLikeFunction '{}'
               | QLikeFunction '<>'
               | QLikeFunction '[]'
               | QLikeFunction '//'
               | QLikeFunction '!!'

QLikeFunction ~ OpKeywordQ
              | OpKeywordQq
              | OpKeywordQx
              | OpKeywordQw
              | OpKeywordQr

###

NonQLikeFunctionName ::= NonQLetter
                       | NonQLetter NonQLetter
                       | NonQLetter NonWLetter
                       | NonQLetter NonXLetter
                       | NonQLetter NonRLetter

NonQLetter ~ [a-pr-z]
NonWLetter ~ [a-vx-z]
NonRLetter ~ [a-qs-z]
NonXLetter ~ [a-wy-z]

IdentComp  ~ [a-zA-Z_]+
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

LitNumber ::= Negative Digits Period Digits
            | Digits Period Digits
            | Negative Digits
            | Negative Infinite
            | Infinite
            | Digits

Infinite    ~ 'Inf'
Negative    ~ '-'
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

NonRParenOrEscapedParens_Many ~ NonRParenOrEscapedParens+
NonRParenOrEscapedParens      ~ EscapedParens | NonRParen
EscapedParens                 ~ EscapedLParen | EscapedRParen
EscapedLParen                 ~ Escape [(]
EscapedRParen                 ~ Escape [)]
NonRParen                     ~ [^)]

NonRBracketOrEscapedBrackets_Many ~ NonRBracketOrEscapedBrackets+
NonRBracketOrEscapedBrackets      ~ EscapedBrackets | NonRBracket
EscapedBrackets                   ~ EscapedLBracket | EscapedRBracket
EscapedLBracket                   ~ Escape [\[]
EscapedRBracket                   ~ Escape [\]]
NonRBracket                       ~ [^\]]

NonRBraceOrEscapedBraces_Many ~ NonRBraceOrEscapedBraces+
NonRBraceOrEscapedBraces      ~ EscapedBraces | NonRBrace
EscapedBraces                 ~ EscapedLBrace | EscapedRBrace
EscapedLBrace                 ~ Escape [\{]
EscapedRBrace                 ~ Escape [\}]
NonRBrace                     ~ [^\}]

NonRAngleOrEscapedAngles_Many ~ NonRAngleOrEscapedAngles+
NonRAngleOrEscapedAngles      ~ EscapedAngles | NonRAngle
EscapedAngles                 ~ EscapedLAngle | EscapedRAngle
EscapedLAngle                 ~ Escape [<]
EscapedRAngle                 ~ Escape [>]
NonRAngle                     ~ [^>]

NonForwardSlashOrEscapedForwardSlashes_Many ~ NonForwardSlashOrEscapedForwardSlashes+
NonForwardSlashOrEscapedForwardSlashes      ~ EscapedForwardSlash | NonForwardSlash
EscapedForwardSlash                         ~ Escape [/]
NonForwardSlash                             ~ [^\/]

NonExclamPointOrEscapedExclamPoints_Many ~ NonExclamPointOrEscapedExclamPoints+
NonExclamPointOrEscapedExclamPoints      ~ EscapedExclamPoint | NonExclamPoint
EscapedExclamPoint                       ~ Escape [!]
NonExclamPoint                           ~ [^\!]

Ellipsis ~ '...'

UnderscorePackage ~ '__PACKAGE__'
UnderscoreFile    ~ '__FILE__'
UnderscoreLine    ~ '__LINE__'
UnderscoreSub     ~ '__SUB__'
UnderscoreData    ~ '__DATA__'
UnderscoreEnd     ~ '__END__'

PhaseName ~ 'BEGIN' | 'CHECK' | 'INIT' | 'UNITCHECK' | 'END'

SubAttrArgs ~ '(' NonRParenOrEscapedParens_Many ')'
            | '(' ')'

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

};

our $grammar = Marpa::R2::Scanless::G->new({ source => \$grammar_source });

sub parse {
    my ($class, $text) = @_;

    my $rec = Marpa::R2::Scanless::R->new({ grammar => $grammar });

    my @values;

    $rec->read(\$text);
    while (my $value = $rec->value()) {
        push @values, $$value;
    }

    if (!@values) {
        for my $nterm (reverse qw/Program Statement Expression SubCall Ident/) {
            my ($start, $length) = $rec->last_completed($nterm);
            next unless defined $start;
            my $range = $rec->substring($start, $length);
            my $expect = $rec->terminals_expected();
            my $progress = $rec->show_progress();
            die "Failed to parse past: $range (char $start, length $length), expected @$expect\n$progress";
        }
        die "Failed to parse, dunno why.";
    }

    return @values;
}

1;
