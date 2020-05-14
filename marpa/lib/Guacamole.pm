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

Statement ::= NonBraceExpression StatementModifier
            | NonBraceExpression
            | LoopStatement
            | Block
            | Condition
            | EllipsisStatement
            | QLikeExpression
            | UseStatement
            | NoStatement

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

Expression ::= Value
            || Expression OpArrow ArrowRHS     assoc=>left
            || Expression OpInc
            || OpInc Expression
            || Expression OpPower Expression   assoc=>right
            || OpUnary Expression              assoc=>right
            || Expression OpRegex Expression   assoc=>left
            || Expression OpMulti Expression   assoc=>left
            || Expression OpAdd Expression     assoc=>left
            || Expression OpShift Expression   assoc=>left
            || OpKeywordExpr
            || OpFileExpr
            || Expression OpInequal Expression
            || Expression OpEqual Expression
            || Expression OpBinAnd Expression  assoc=>left
            || Expression OpBinOr Expression   assoc=>left
            || Expression OpLogAnd Expression  assoc=>left
            || Expression OpLogOr Expression   assoc=>left
            || Expression OpRange Expression
            || Expression OpTriThen Expression OpTriElse Expression  assoc=>right
            || Expression OpAssign Expression  assoc=>right
            || Expression OpComma Expression   assoc=>left
            || OpNameNot Expression            assoc=>right
            || Expression OpNameAnd Expression assoc=>left
            || Expression OpNameOr Expression  assoc=>left

# Same as Expression, but since it's a top-level expresison,
# it can only use NonBraceValue and NonBraceExpressions
NonBraceExpression ::= NonBraceValue
                    || NonBraceExpression OpArrow ArrowRHS     assoc=>left
                    || NonBraceExpression OpInc
                    || OpInc Expression
                    || NonBraceExpression OpPower Expression   assoc=>right
                    || OpUnary Expression                      assoc=>right
                    || NonBraceExpression OpRegex Expression   assoc=>left
                    || NonBraceExpression OpMulti Expression   assoc=>left
                    || NonBraceExpression OpAdd Expression     assoc=>left
                    || NonBraceExpression OpShift Expression   assoc=>left
                    || OpKeywordExpr
                    || NonBraceExpression OpInequal Expression
                    || NonBraceExpression OpEqual Expression
                    || NonBraceExpression OpBinAnd Expression  assoc=>left
                    || NonBraceExpression OpBinOr Expression   assoc=>left
                    || NonBraceExpression OpLogAnd Expression  assoc=>left
                    || NonBraceExpression OpLogOr Expression   assoc=>left
                    || NonBraceExpression OpRange Expression
                    || NonBraceExpression OpTriThen Expression OpTriElse Expression  assoc=>right
                    || NonBraceExpression OpAssign Expression  assoc=>right
                    || OpNameNot Expression                    assoc=>right
                    || NonBraceExpression OpNameAnd Expression assoc=>left
                    || NonBraceExpression OpNameOr Expression  assoc=>left

Value         ::= Modifier Variable
                | Modifier ParenExpr
                | Literal
                | Variable
                | UnderscoreValues
                | SubCall
                | ParenExpr

# Same as Value above, but with a NonBraceLiteral
NonBraceValue ::= Modifier Variable
                | Modifier ParenExpr
                | NonBraceLiteral
                | Variable
                | UnderscoreValues
                | SubCall
                | ParenExpr

ParenExpr ::= LParen Expression RParen
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

SubCall ::= Ident CallArgs
          | VarCode CallArgs

CallArgs ::= ParenExpr
           | LParen RParen

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

# All keywords
OpKeywordExpr ::= OpKeywordAbsExpr
                | OpKeywordAcceptExpr
                | OpKeywordAlarmExpr
                | OpKeywordAtan2Expr
                | OpKeywordBindExpr
                | OpKeywordBinmodeExpr
                | OpKeywordBlessExpr
                | OpKeywordBreakExpr
                | OpKeywordCallerExpr
                | OpKeywordChdirExpr
                | OpKeywordChmodExpr
                | OpKeywordChompExpr
                | OpKeywordChopExpr
                | OpKeywordChownExpr
                | OpKeywordChrExpr
                | OpKeywordChrootExpr
                | OpKeywordCloseExpr
                | OpKeywordClosedirExpr
                | OpKeywordConnectExpr
                | OpKeywordCosExpr
                | OpKeywordCryptExpr
                | OpKeywordDbmcloseExpr
                | OpKeywordDbmopenExpr
                | OpKeywordDefinedExpr
                | OpKeywordDeleteExpr
                | OpKeywordDieExpr
                | OpKeywordDoExpr
                | OpKeywordDumpExpr
                | OpKeywordEachExpr
                | OpKeywordEofExpr
                | OpKeywordEvalExpr
                | OpKeywordEvalbytesExpr
                | OpKeywordExistsExpr
                | OpKeywordExitExpr
                | OpKeywordExpExpr
                | OpKeywordFcExpr
                | OpKeywordFcntlExpr
                | OpKeywordFilenoExpr
                | OpKeywordFlockExpr
                | OpKeywordForkExpr
                | OpKeywordGetcExpr
                | OpKeywordGetloginExpr
                | OpKeywordGetpeernameExpr
                | OpKeywordGetpgrpExpr
                | OpKeywordGetppidExpr
                | OpKeywordGetpriorityExpr
                | OpKeywordGetpwnamExpr
                | OpKeywordGetgrnamExpr
                | OpKeywordGethostbynameExpr
                | OpKeywordGetnetbynameExpr
                | OpKeywordGetprotobynameExpr
                | OpKeywordGetpwuidExpr
                | OpKeywordGetgrgidExpr
                | OpKeywordGetservbynameExpr
                | OpKeywordGethostbyaddrExpr
                | OpKeywordGetnetbyaddrExpr
                | OpKeywordGetprotobynumberExpr
                | OpKeywordGetservbyportExpr
                | OpKeywordGetpwentExpr
                | OpKeywordGetgrentExpr
                | OpKeywordGethostentExpr
                | OpKeywordGetnetentExpr
                | OpKeywordGetprotoentExpr
                | OpKeywordGetserventExpr
                | OpKeywordSetpwentExpr
                | OpKeywordSetgrentExpr
                | OpKeywordSethostentExpr
                | OpKeywordSetnetentExpr
                | OpKeywordSetprotoentExpr
                | OpKeywordSetserventExpr
                | OpKeywordEndpwentExpr
                | OpKeywordEndgrentExpr
                | OpKeywordEndhostentExpr
                | OpKeywordEndnetentExpr
                | OpKeywordEndprotoentExpr
                | OpKeywordEndserventExpr
                | OpKeywordExecExpr
                | OpKeywordGetsocknameExpr
                | OpKeywordGetsockoptExpr
                | OpKeywordGlobExpr
                | OpKeywordGmtimeExpr
                | OpKeywordGotoExpr
                | OpKeywordGrepExpr
                | OpKeywordHexExpr
                | OpKeywordIndexExpr
                | OpKeywordIntExpr
                | OpKeywordIoctlExpr
                | OpKeywordJoinExpr
                | OpKeywordKeysExpr
                | OpKeywordKillExpr
                | OpKeywordLastExpr
                | OpKeywordLcExpr
                | OpKeywordLcfirstExpr
                | OpKeywordLengthExpr
                | OpKeywordLinkExpr
                | OpKeywordListenExpr
                | OpKeywordLocaltimeExpr
                | OpKeywordLockExpr
                | OpKeywordLogExpr
                | OpKeywordLstatExpr
                | OpKeywordMapExpr
                | OpKeywordMkdirExpr
                | OpKeywordMsgctlExpr
                | OpKeywordMsggetExpr
                | OpKeywordMsgrcvExpr
                | OpKeywordMsgsndExpr
                | OpKeywordNextExpr
                | OpKeywordOctExpr
                | OpKeywordOpenExpr
                | OpKeywordOpendirExpr
                | OpKeywordOrdExpr
                | OpKeywordPackExpr
                | OpKeywordPipeExpr
                | OpKeywordPopExpr
                | OpKeywordPosExpr
                | OpKeywordPrintExpr
                | OpKeywordPrintfExpr
                | OpKeywordPrototypeExpr
                | OpKeywordPushExpr
                | OpKeywordQuotemetaExpr
                | OpKeywordRandExpr
                | OpKeywordReadExpr
                | OpKeywordReaddirExpr
                | OpKeywordReadlineExpr
                | OpKeywordReadlinkExpr
                | OpKeywordReadpipeExpr
                | OpKeywordRecvExpr
                | OpKeywordRedoExpr
                | OpKeywordRefExpr
                | OpKeywordRenameExpr
                | OpKeywordResetExpr
                | OpKeywordReturnExpr
                | OpKeywordReverseExpr
                | OpKeywordRewinddirExpr
                | OpKeywordRindexExpr
                | OpKeywordRmdirExpr
                | OpKeywordSayExpr
                | OpKeywordScalarExpr
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
                | OpKeywordShiftExpr
                | OpKeywordShmctlExpr
                | OpKeywordShmgetExpr
                | OpKeywordShmreadExpr
                | OpKeywordShmwriteExpr
                | OpKeywordShutdownExpr
                | OpKeywordSinExpr
                | OpKeywordSleepExpr
                | OpKeywordSocketExpr
                | OpKeywordSocketpairExpr
                | OpKeywordSortExpr
                | OpKeywordSpliceExpr
                | OpKeywordSprintfExpr
                | OpKeywordSqrtExpr
                | OpKeywordSrandExpr
                | OpKeywordStatExpr
                | OpKeywordStudyExpr
                | OpKeywordSubstrExpr
                | OpKeywordSymlinkExpr
                | OpKeywordSyscallExpr
                | OpKeywordSysopenExpr
                | OpKeywordSysreadExpr
                | OpKeywordSysseekExpr
                | OpKeywordSystemExpr
                | OpKeywordSyswriteExpr
                | OpKeywordTellExpr
                | OpKeywordTelldirExpr
                | OpKeywordTieExpr
                | OpKeywordTiedExpr
                | OpKeywordTimeExpr
                | OpKeywordTimesExpr
                | OpKeywordTruncateExpr
                | OpKeywordUcExpr
                | OpKeywordUcfirstExpr
                | OpKeywordUmaskExpr
                | OpKeywordUndefExpr
                | OpKeywordUnlinkExpr
                | OpKeywordUnpackExpr
                | OpKeywordUnshiftExpr
                | OpKeywordUntieExpr
                | OpKeywordUtimeExpr
                | OpKeywordValuesExpr
                | OpKeywordVecExpr
                | OpKeywordWaitExpr
                | OpKeywordWaitpidExpr
                | OpKeywordWantarrayExpr
                | OpKeywordWarnExpr
                | OpKeywordWriteExpr

# TODO: (Add the following above)
#| OpKeywordPackageExpr
#| OpKeywordRequireExpr
#| OpKeywordSplitExpr
#| OpKeywordSubExpr

OpFileExpr ::= OpFileReadableEffectiveExpr
             | OpFileWritableEffectiveExpr
             | OpFileRExecutableEffectiveExpr
             | OpFileOwnedEffectiveExpr
             | OpFileReadableRealExpr
             | OpFileWritableRealExpr
             | OpFileRExecutableRealExpr
             | OpFileOwnedRealExpr
             | OpFileExistsExpr
             | OpFileEmptyExpr
             | OpFileNonEmptyExpr
             | OpFilePlainExpr
             | OpFileDirectoryExpr
             | OpFileSymbolicExpr
             | OpFileNamedPipeExpr
             | OpFileSocketExpr
             | OpFileBlockExpr
             | OpFileCharacterExpr
             | OpFileOpenedTtyExpr
             | OpFileSetuidExpr
             | OpFileSetgidExpr
             | OpFileStickyExpr
             | OpFileAsciiUtf8Expr
             | OpFileBinaryExpr
             | OpFileStartTimeExpr
             | OpFileAccessTimeExpr
             | OpFileChangeTimeExpr

# Grammar for keywords
OpKeywordAbsExpr              ::= OpKeywordAbs Expression
                                | OpKeywordAbs

OpKeywordAcceptExpr           ::= OpKeywordAccept Expression OpComma Expression

OpKeywordAlarmExpr            ::= OpKeywordAlarm Expression
                                | OpKeywordAlarm

OpKeywordAtan2Expr            ::= OpKeywordAtan2 Expression OpComma Expression

OpKeywordBindExpr             ::= OpKeywordBind Expression OpComma Expression

OpKeywordBinmodeExpr          ::= OpKeywordBinmode Expression OpComma Expression
                                | OpKeywordBinmode Expression

OpKeywordBlessExpr            ::= OpKeywordBless Expression OpComma Expression
                                | OpKeywordBless Expression

OpKeywordBreakExpr            ::= OpKeywordBreak

OpKeywordCallerExpr           ::= OpKeywordCaller Expression
                                | OpKeywordCaller

OpKeywordChdirExpr            ::= OpKeywordChdir Expression
                                | OpKeywordChdir

OpKeywordChmodExpr            ::= OpKeywordChmod Expression OpComma Expression

OpKeywordChompExpr            ::= OpKeywordChomp Expression
                                | OpKeywordChomp

OpKeywordChopExpr             ::= OpKeywordChop Expression
                                | OpKeywordChop

OpKeywordChownExpr            ::= OpKeywordChown Expression OpComma Expression OpComma Expression

OpKeywordChrExpr              ::= OpKeywordChr Expression
                                | OpKeywordChr

OpKeywordChrootExpr           ::= OpKeywordChroot Expression
                                | OpKeywordChroot

OpKeywordCloseExpr            ::= OpKeywordClose Expression
                                | OpKeywordClose

OpKeywordClosedirExpr         ::= OpKeywordClosedir Expression

OpKeywordConnectExpr          ::= OpKeywordConnect Expression OpComma Expression

OpKeywordCosExpr              ::= OpKeywordCos Expression

OpKeywordCryptExpr            ::= OpKeywordCrypt Expression OpComma Expression

OpKeywordDbmcloseExpr         ::= OpKeywordDbmclose VarHash

OpKeywordDbmopenExpr          ::= OpKeywordDbmopen VarHash OpComma Expression OpComma Expression

OpKeywordDefinedExpr          ::= OpKeywordDefined Expression
                                | OpKeywordDefined

OpKeywordDeleteExpr           ::= OpKeywordDelete Expression

OpKeywordDieExpr              ::= OpKeywordDie Expression

OpKeywordDoExpr               ::= OpKeywordDo Block
                                | OpKeywordDo NonBraceExpression

OpKeywordDumpExpr             ::= OpKeywordDump Label
                                | OpKeywordDump Expression
                                | OpKeywordDump

OpKeywordEachExpr             ::= OpKeywordEach Expression

OpKeywordEofExpr              ::= OpKeywordEof Expression
                                | OpKeywordEof

OpKeywordEvalExpr             ::= OpKeywordEval Block

OpKeywordEvalbytesExpr        ::= OpKeywordEvalbytes Expression
                                | OpKeywordEvalbytes

OpKeywordExistsExpr           ::= OpKeywordExists Expression

OpKeywordExitExpr             ::= OpKeywordExit Expression
                                | OpKeywordExit

OpKeywordExpExpr              ::= OpKeywordExp Expression
                                | OpKeywordExp

OpKeywordFcExpr               ::= OpKeywordFc Expression
                                | OpKeywordFc

OpKeywordFcntlExpr            ::= OpKeywordFcntl Expression OpComma Expression OpComma Expression

OpKeywordFilenoExpr           ::= OpKeywordFileno Expression

OpKeywordFlockExpr            ::= OpKeywordFlock Expression OpComma Expression

OpKeywordForkExpr             ::= OpKeywordFork

OpKeywordGetcExpr             ::= OpKeywordGetc Expression
                                | OpKeywordGetc

OpKeywordGetloginExpr         ::= OpKeywordGetlogin

OpKeywordGetpeernameExpr      ::= OpKeywordGetpeername Expression

OpKeywordGetpgrpExpr          ::= OpKeywordGetpgrp Expression

OpKeywordGetppidExpr          ::= OpKeywordGetppid

OpKeywordGetpriorityExpr      ::= OpKeywordGetpriority Expression OpComma Expression

OpKeywordGetpwnamExpr         ::= OpKeywordGetpwnam Expression

OpKeywordGetgrnamExpr         ::= OpKeywordGetgrnam Expression

OpKeywordGethostbynameExpr    ::= OpKeywordGethostbyname Expression

OpKeywordGetnetbynameExpr     ::= OpKeywordGetnetbyname Expression

OpKeywordGetprotobynameExpr   ::= OpKeywordGetprotobyname Expression

OpKeywordGetpwuidExpr         ::= OpKeywordGetpwuid Expression

OpKeywordGetgrgidExpr         ::= OpKeywordGetgrgid Expression

OpKeywordGetservbynameExpr    ::= OpKeywordGetservbyname Expression OpComma Expression

OpKeywordGethostbyaddrExpr    ::= OpKeywordGethostbyaddr Expression OpComma Expression

OpKeywordGetnetbyaddrExpr     ::= OpKeywordGetnetbyaddr Expression OpComma Expression

OpKeywordGetprotobynumberExpr ::= OpKeywordGetprotobynumber Expression

OpKeywordGetservbyportExpr    ::= OpKeywordGetservbyport Expression OpComma Expression

OpKeywordGetpwentExpr         ::= OpKeywordGetpwent

OpKeywordGetgrentExpr         ::= OpKeywordGetgrent

OpKeywordGethostentExpr       ::= OpKeywordGethostent

OpKeywordGetnetentExpr        ::= OpKeywordGetnetent

OpKeywordGetprotoentExpr      ::= OpKeywordGetprotoent

OpKeywordGetserventExpr       ::= OpKeywordGetservent

OpKeywordSetpwentExpr         ::= OpKeywordSetpwent

OpKeywordSetgrentExpr         ::= OpKeywordSetgrent

OpKeywordSethostentExpr       ::= OpKeywordSethostent Expression

OpKeywordSetnetentExpr        ::= OpKeywordSetnetent Expression

OpKeywordSetprotoentExpr      ::= OpKeywordSetprotoent Expression

OpKeywordSetserventExpr       ::= OpKeywordSetservent Expression

OpKeywordEndpwentExpr         ::= OpKeywordEndpwent

OpKeywordEndgrentExpr         ::= OpKeywordEndgrent

OpKeywordEndhostentExpr       ::= OpKeywordEndhostent

OpKeywordEndnetentExpr        ::= OpKeywordEndnetent

OpKeywordEndprotoentExpr      ::= OpKeywordEndprotoent

OpKeywordEndserventExpr       ::= OpKeywordEndservent

OpKeywordExecExpr             ::= OpKeywordExec Block Expression
                                | OpKeywordExec NonBraceExpression

OpKeywordGetsocknameExpr      ::= OpKeywordGetsockname Expression

OpKeywordGetsockoptExpr       ::= OpKeywordGetsockopt Expression OpComma Expression OpComma Expression

OpKeywordGlobExpr             ::= OpKeywordGlob Expression
                                | OpKeywordGlob

OpKeywordGmtimeExpr           ::= OpKeywordGmtime Expression
                                | OpKeywordGmtime

OpKeywordGotoExpr             ::= OpKeywordGoto Label
                                | OpKeywordGoto Expression
                                | OpKeywordGoto SigilCode IdentComp

OpKeywordGrepExpr             ::= OpKeywordGrep Block Expression
                                | OpKeywordGrep NonBraceExpression OpComma Expression

OpKeywordHexExpr              ::= OpKeywordHex Expression
                                | OpKeywordHex

OpKeywordIndexExpr            ::= OpKeywordIndex Expression OpComma Expression OpComma Expression
                                | OpKeywordIndex Expression OpComma Expression

OpKeywordIntExpr              ::= OpKeywordInt Expression
                                | OpKeywordInt

OpKeywordIoctlExpr            ::= OpKeywordIoctl Expression OpComma Expression OpComma Expression

OpKeywordJoinExpr             ::= OpKeywordJoin Expression OpComma Expression

OpKeywordKeysExpr             ::= OpKeywordKeys VarHash
                                | OpKeywordKeys VarArray
                                | OpKeywordKeys Expression

OpKeywordKillExpr             ::= OpKeywordKill Expression OpComma Expression
                                | OpKeywordKill Expression

OpKeywordLastExpr             ::= OpKeywordLast Label
                                | OpKeywordLast Expression
                                | OpKeywordLast

OpKeywordLcExpr               ::= OpKeywordLc Expression
                                | OpKeywordLc

OpKeywordLcfirstExpr          ::= OpKeywordLcfirst Expression
                                | OpKeywordLcfirst

OpKeywordLengthExpr           ::= OpKeywordLength Expression
                                | OpKeywordLength

OpKeywordLinkExpr             ::= OpKeywordLink Expression OpComma Expression

OpKeywordListenExpr           ::= OpKeywordListen Expression OpComma Expression

OpKeywordLocaltimeExpr        ::= OpKeywordLocaltime Expression
                                | OpKeywordLocaltime

OpKeywordLockExpr             ::= OpKeywordLock Expression

OpKeywordLogExpr              ::= OpKeywordLog Expression
                                | OpKeywordLog

OpKeywordLstatExpr            ::= OpKeywordLstat Expression
                                | OpKeywordLstat

OpKeywordMapExpr              ::= OpKeywordMap Block Expression
                                | OpKeywordMap NonBraceExpression OpComma Expression

OpKeywordMkdirExpr            ::= OpKeywordMkdir Expression OpComma Expression
                                | OpKeywordMkdir Expression
                                | OpKeywordMkdir

OpKeywordMsgctlExpr           ::= OpKeywordMsgctl Expression OpComma Expression OpComma Expression

OpKeywordMsggetExpr           ::= OpKeywordMsgget Expression OpComma Expression

OpKeywordMsgrcvExpr           ::= OpKeywordMsgrcv Expression OpComma Expression OpComma Expression OpComma Expression OpComma Expression

OpKeywordMsgsndExpr           ::= OpKeywordMsgsnd Expression OpComma Expression OpComma Expression

OpKeywordNextExpr             ::= OpKeywordNext Label
                                | OpKeywordNext Expression
                                | OpKeywordNext

OpKeywordOctExpr              ::= OpKeywordOct Expression
                                | OpKeywordOct

OpKeywordOpenExpr             ::= OpKeywordOpen Expression OpComma Expression OpComma Expression OpComma Expression
                                | OpKeywordOpen Expression OpComma Expression OpComma Expression

OpKeywordOpendirExpr          ::= OpKeywordOpendir Expression OpComma Expression

OpKeywordOrdExpr              ::= OpKeywordOrd Expression
                                | OpKeywordOrd

OpKeywordPackExpr             ::= OpKeywordPack Expression OpComma Expression

# TODO: OpKeywordPackageExpr ::= OpKeywordPackage

OpKeywordPipeExpr             ::= OpKeywordPipe Expression OpComma Expression

OpKeywordPopExpr              ::= OpKeywordPop Expression
                                | OpKeywordPop

OpKeywordPosExpr              ::= OpKeywordPos Expression
                                | OpKeywordPos

OpKeywordPrintExpr            ::= OpKeywordPrint Block Expression
                                | OpKeywordPrint NonBraceExpression OpComma Expression
                                | OpKeywordPrint NonBraceExpression
                                | OpKeywordPrint Block
                                | OpKeywordPrint

OpKeywordPrintfExpr           ::= OpKeywordPrintf Block NonBraceExpression OpComma Expression
                                | OpKeywordPrintf NonBraceExpression OpComma Expression
                                | OpKeywordPrintf Block

OpKeywordPrototypeExpr        ::= OpKeywordPrototype Expression
                                | OpKeywordPrototype

OpKeywordPushExpr             ::= OpKeywordPush Expression OpComma Expression

OpKeywordQuotemetaExpr        ::= OpKeywordQuotemeta Expression
                                | OpKeywordQuotemeta

OpKeywordRandExpr             ::= OpKeywordRand Expression
                                | OpKeywordRand

OpKeywordReadExpr             ::= OpKeywordRead Expression OpComma Expression OpComma Expression OpComma Expression
                                | OpKeywordRead Expression OpComma Expression OpComma Expression

OpKeywordReaddirExpr          ::= OpKeywordReaddir Expression

OpKeywordReadlineExpr         ::= OpKeywordReadline Expression
                                | OpKeywordReadline

OpKeywordReadlinkExpr         ::= OpKeywordReadlink Expression
                                | OpKeywordReadlink

OpKeywordReadpipeExpr         ::= OpKeywordReadpipe Expression
                                | OpKeywordReadpipe

OpKeywordRecvExpr             ::= OpKeywordRecv Expression OpComma Expression OpComma Expression OpComma Expression

OpKeywordRedoExpr             ::= OpKeywordRedo Label
                                | OpKeywordRedo Expression
                                | OpKeywordRedo

OpKeywordRefExpr              ::= OpKeywordRef Expression
                                | OpKeywordRef

OpKeywordRenameExpr           ::= OpKeywordRename Expression OpComma Expression

OpKeywordResetExpr            ::= OpKeywordReset Expression
                                | OpKeywordReset

OpKeywordReturnExpr           ::= OpKeywordReturn Expression
                                | OpKeywordReturn

OpKeywordReverseExpr          ::= OpKeywordReverse Expression

OpKeywordRewinddirExpr        ::= OpKeywordRewinddir Expression
                                | OpKeywordRewinddir

OpKeywordRindexExpr           ::= OpKeywordRindex Expression OpComma Expression OpComma Expression
                                | OpKeywordRindex Expression OpComma Expression

OpKeywordRmdirExpr            ::= OpKeywordRmdir Expression
                                | OpKeywordRmdir

OpKeywordSayExpr              ::= OpKeywordSay Block Expression
                                | OpKeywordSay NonBraceExpression OpComma Expression
                                | OpKeywordSay NonBraceExpression
                                | OpKeywordSay Block
                                | OpKeywordSay

OpKeywordScalarExpr           ::= OpKeywordScalar Expression

OpKeywordSeekExpr             ::= OpKeywordSeek Expression OpComma Expression OpComma Expression

OpKeywordSeekdirExpr          ::= OpKeywordSeekdir Expression OpComma Expression

OpKeywordSelectExpr           ::= OpKeywordSelect Expression OpComma Expression OpComma Expression OpComma Expression
                                | OpKeywordSelect Expression

OpKeywordSemctlExpr           ::= OpKeywordSemctl Expression OpComma Expression OpComma Expression OpComma Expression

OpKeywordSemgetExpr           ::= OpKeywordSemget Expression OpComma Expression OpComma Expression

OpKeywordSemopExpr            ::= OpKeywordSemop Expression OpComma Expression

OpKeywordSendExpr             ::= OpKeywordSend Expression OpComma Expression OpComma Expression OpComma Expression
                                | OpKeywordSend Expression OpComma Expression OpComma Expression

OpKeywordSetpgrpExpr          ::= OpKeywordSetpgrp Expression OpComma Expression

OpKeywordSetpriorityExpr      ::= OpKeywordSetpriority Expression OpComma Expression OpComma Expression

OpKeywordSetsockoptExpr       ::= OpKeywordSetsockopt Expression OpComma Expression OpComma Expression OpComma Expression

OpKeywordShiftExpr            ::= OpKeywordShift Expression
                                | OpKeywordShift

OpKeywordShmctlExpr           ::= OpKeywordShmctl Expression OpComma Expression OpComma Expression

OpKeywordShmgetExpr           ::= OpKeywordShmget Expression OpComma Expression OpComma Expression

OpKeywordShmreadExpr          ::= OpKeywordShmread Expression OpComma Expression OpComma Expression OpComma Expression

OpKeywordShmwriteExpr         ::= OpKeywordShmwrite Expression OpComma Expression OpComma Expression OpComma Expression

OpKeywordShutdownExpr         ::= OpKeywordShutdown Expression OpComma Expression

OpKeywordSinExpr              ::= OpKeywordSin Expression
                                | OpKeywordSin

OpKeywordSleepExpr            ::= OpKeywordSleep Expression
                                | OpKeywordSleep

OpKeywordSocketExpr           ::= OpKeywordSocket Expression OpComma Expression OpComma Expression OpComma Expression

OpKeywordSocketpairExpr       ::= OpKeywordSocketpair Expression OpComma Expression OpComma Expression OpComma Expression OpComma Expression

OpKeywordSortExpr             ::= OpKeywordSort Block Expression
                                | OpKeywordSort VarScalar Expression
                                | OpKeywordSort NonBraceExpression

OpKeywordSpliceExpr           ::= OpKeywordSplice Expression OpComma Expression OpComma Expression OpComma Expression
                                | OpKeywordSplice Expression OpComma Expression OpComma Expression
                                | OpKeywordSplice Expression OpComma Expression
                                | OpKeywordSplice Expression

# TODO: OpKeywordSplitExpr ::= OpKeywordSplit

OpKeywordSprintfExpr          ::= OpKeywordSprintf Expression

OpKeywordSqrtExpr             ::= OpKeywordSqrt Expression
                                | OpKeywordSqrt

OpKeywordSrandExpr            ::= OpKeywordSrand Expression
                                | OpKeywordSrand

OpKeywordStatExpr             ::= OpKeywordStat Expression
                                | OpKeywordStat

OpKeywordStudyExpr            ::= OpKeywordStudy Expression
                                | OpKeywordStudy

# TODO: OpKeywordSubExpr ::= OpKeywordSub

OpKeywordSubstrExpr           ::= OpKeywordSubstr Expression OpComma Expression OpComma Expression OpComma Expression
                                | OpKeywordSubstr Expression OpComma Expression OpComma Expression
                                | OpKeywordSubstr Expression OpComma Expression

OpKeywordSymlinkExpr          ::= OpKeywordSymlink Expression OpComma Expression

OpKeywordSyscallExpr          ::= OpKeywordSyscall Expression OpComma Expression

OpKeywordSysopenExpr          ::= OpKeywordSysopen Expression OpComma Expression OpComma Expression OpComma Expression
                                | OpKeywordSysopen Expression OpComma Expression OpComma Expression

OpKeywordSysreadExpr          ::= OpKeywordSysread Expression OpComma Expression OpComma Expression OpComma Expression
                                | OpKeywordSysread Expression OpComma Expression OpComma Expression

OpKeywordSysseekExpr          ::= OpKeywordSysseek Expression OpComma Expression OpComma Expression

OpKeywordSyswriteExpr         ::= OpKeywordSyswrite Expression OpComma Expression OpComma Expression OpComma Expression
                                | OpKeywordSyswrite Expression OpComma Expression OpComma Expression
                                | OpKeywordSyswrite Expression OpComma Expression

OpKeywordSystemExpr           ::= OpKeywordSystem Block Expression
                                | OpKeywordSystem NonBraceExpression

OpKeywordTellExpr             ::= OpKeywordTell Expression
                                | OpKeywordTell

OpKeywordTelldirExpr          ::= OpKeywordTelldir Expression

OpKeywordTieExpr              ::= OpKeywordTie Expression OpComma Expression OpComma Expression

OpKeywordTiedExpr             ::= OpKeywordTied Expression

OpKeywordTimeExpr             ::= OpKeywordTime

OpKeywordTimesExpr            ::= OpKeywordTimes

OpKeywordTruncateExpr         ::= OpKeywordTruncate Expression OpComma Expression

OpKeywordUcExpr               ::= OpKeywordUc Expression
                                | OpKeywordUc

OpKeywordUcfirstExpr          ::= OpKeywordUcfirst Expression
                                | OpKeywordUcfirst

OpKeywordUmaskExpr            ::= OpKeywordUmask Expression
                                | OpKeywordUmask

OpKeywordUndefExpr            ::= OpKeywordUndef Expression
                                | OpKeywordUndef

OpKeywordUnlinkExpr           ::= OpKeywordUnlink Expression
                                | OpKeywordUnlink

OpKeywordUnpackExpr           ::= OpKeywordUnpack Expression OpComma Expression
                                | OpKeywordUnpack Expression

OpKeywordUnshiftExpr          ::= OpKeywordUnshift Value Expression

OpKeywordUntieExpr            ::= OpKeywordUntie Expression

OpKeywordUtimeExpr            ::= OpKeywordUtime Expression

OpKeywordValuesExpr           ::= OpKeywordValues Expression

OpKeywordVecExpr              ::= OpKeywordVec Expression OpComma Expression OpComma Expression

OpKeywordWaitExpr             ::= OpKeywordWait

OpKeywordWaitpidExpr          ::= OpKeywordWaitpid Expression OpComma Expression

OpKeywordWantarrayExpr        ::= OpKeywordWantarray

OpKeywordWarnExpr             ::= OpKeywordWarn Expression

OpKeywordWriteExpr            ::= OpKeywordWrite Expression
                                | OpKeywordWrite

OpFileReadableEffectiveExpr     ::= OpFileReadableEffective    Expression
OpFileWritableEffectiveExpr     ::= OpFileWritableEffective    Expression
OpFileRExecutableEffectiveExpr  ::= OpFileRExecutableEffective Expression
OpFileOwnedEffectiveExpr        ::= OpFileOwnedEffective       Expression
OpFileReadableRealExpr          ::= OpFileReadableReal         Expression
OpFileWritableRealExpr          ::= OpFileWritableReal         Expression
OpFileRExecutableRealExpr       ::= OpFileRExecutableReal      Expression
OpFileOwnedRealExpr             ::= OpFileOwnedReal            Expression
OpFileExistsExpr                ::= OpFileExists               Expression
OpFileEmptyExpr                 ::= OpFileEmpty                Expression
OpFileNonEmptyExpr              ::= OpFileNonEmpty             Expression
OpFilePlainExpr                 ::= OpFilePlain                Expression
OpFileDirectoryExpr             ::= OpFileDirectory            Expression
OpFileSymbolicExpr              ::= OpFileSymbolic             Expression
OpFileNamedPipeExpr             ::= OpFileNamedPipe            Expression
OpFileSocketExpr                ::= OpFileSocket               Expression
OpFileBlockExpr                 ::= OpFileBlock                Expression
OpFileCharacterExpr             ::= OpFileCharacter            Expression
OpFileOpenedTtyExpr             ::= OpFileOpenedTty            Expression
OpFileSetuidExpr                ::= OpFileSetuid               Expression
OpFileSetgidExpr                ::= OpFileSetgid               Expression
OpFileStickyExpr                ::= OpFileSticky               Expression
OpFileAsciiUtf8Expr             ::= OpFileAsciiUtf8            Expression
OpFileBinaryExpr                ::= OpFileBinary               Expression
OpFileStartTimeExpr             ::= OpFileStartTime            Expression
OpFileAccessTimeExpr            ::= OpFileAccessTime           Expression
OpFileChangeTimeExpr            ::= OpFileChangeTime           Expression

QLikeExpression ::= QLikeExpressionExpr

QLikeExpressionExpr ~ QLikeFunction '(' NonRParenOrEscapedParens_Many               ')'
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

LitNumber   ~ [0-9]+
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
# TODO: OpKeywordPackage          ~ 'package'
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
# TODO: OpKeywordRequire          ~ 'require'
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
# TODO: OpKeywordSplit            ~ 'split'
OpKeywordSprintf          ~ 'sprintf'
OpKeywordSqrt             ~ 'sqrt'
OpKeywordSrand            ~ 'srand'
OpKeywordStat             ~ 'stat'
OpKeywordState            ~ 'state'
OpKeywordStudy            ~ 'study'
# TODO: OpKeywordSub              ~ 'sub'
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
