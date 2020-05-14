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

Statement ::= NonBraceExpression
            | Block

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
            || OpKeyword
            || OpFile
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
                    || OpKeyword
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

Value         ::= Literal
                | Variable
                | Modifier Variable
                | Modifier ParenExpr
                | UnderscoreValues
                | SubCall
                | ParenExpr

# Same as Value above, but with a NonBraceLiteral
NonBraceValue ::= NonBraceLiteral
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

LitString      ::= SingleQuote NonSingleQuote SingleQuote
                 | SingleQuote SingleQuote

InterpolString ::= DoubleQuote NonDoubleQuote DoubleQuote
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
OpKeyword ::= OpKeywordAbsExpr
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
#| OpKeywordNoExpr
#| OpKeywordPackageExpr
#| OpKeywordRequireExpr
#| OpKeywordSplitExpr
#| OpKeywordSubExpr
#| OpKeywordUseExpr

OpFile ::= OpFileReadableEffectiveExpr
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

# TODO: OpKeywordNoExpr ::= OpKeywordNo Expression

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

# TODO: OpKeywordUseExpr ::= OpKeywordUse

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

###

IdentComp  ~ [a-zA-Z_]+
PackageSep ~ '::'

LitNumber      ~ [0-9]+
SingleQuote    ~ [']
DoubleQuote    ~ ["]
NonDoubleQuote ~ [^"]+
NonSingleQuote ~ [^']+

Colon     ~ ':'
Semicolon ~ ';'

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
# TODO: OpKeywordNo               ~ 'no'
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
# TODO: OpKeywordUse              ~ 'use'
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

# These are some parsing rules for the Expressions for them:
# ----
# (from Perl 5.28.1)
# perldoc -m perlfunc | grep "^=item\s\+[a-z]" | awk '{$1=""; print $0}'
# ---
# * OpKeywordAlarm:  alarm SECONDS
#                    alarm
# * OpKeywordAtan2 : atan2 Y,X
# * OpKeywordBind : bind SOCKET,NAME
# * OpKeywordBinmode: binmode FILEHANDLE, LAYER
#                     binmode FILEHANDLE
# * OpKeywordBless: bless REF,CLASSNAME
#                   bless REF
# * OpKeywordBreak: break
# * OpKeywordCaller: caller EXPR
#                    caller
# * OpKeywordChdir: chdir EXPR
#                   chdir FILEHANDLE
#                   chdir DIRHANDLE
#                   chdir
# * OpKeywordChmod: chmod LIST
# * OpKeywordChomp: chomp VARIABLE
#                   chomp( LIST )
#                   chomp
# * OpKeywordChop: chop VARIABLE
#                  chop( LIST )
#                  chop
# * OpKeywordChown: chown LIST
# * OpKeywordChr: chr NUMBER
#                 chr
# * OpKeywordChroot: chroot FILENAME
#                    chroot
# * OpKeywordClose: close FILEHANDLE
#                   close
# * OpKeywordClosedir: closedir DIRHANDLE
# * OpKeywordConnect: connect SOCKET,NAME
# * OpKeywordContinue: continue BLOCK
# *                    continue
# * OpKeywordCos: cos EXPR
#                 cos
# * OpKeywordCrypt: crypt PLAINTEXT,SALT
# * OpKeyword: dbmclose HASH
# * OpKeyword: dbmopen HASH,DBNAME,MASK
# * OpKeyword: defined EXPR
#              defined
# * OpKeyword: delete EXPR
# * OpKeyword: die LIST
# * OpKeyword: do BLOCK
# * OpKeyword: do EXPR
# * OpKeyword: dump LABEL
#              dump EXPR
#              dump
# * OpKeyword: each HASH
#              each ARRAY
# * OpKeyword: eof FILEHANDLE
#              eof ()
#              eof
# * OpKeyword: eval EXPR
#              eval BLOCK
#              eval
# * OpKeyword: evalbytes EXPR
#              evalbytes
# * OpKeyword: exec LIST
#              exec PROGRAM LIST
# * OpKeyword: exists EXPR
# * OpKeyword: exit EXPR
#              exit
# * OpKeyword: exp EXPR
#              exp
# * OpKeyword: fc EXPR
#              fc
# * OpKeyword: fcntl FILEHANDLE,FUNCTION,SCALAR
# * OpKeyword: fileno FILEHANDLE
#              fileno DIRHANDLE
# * OpKeyword: flock FILEHANDLE,OPERATION
# * OpKeyword: fork
# * OpKeyword: format
# * OpKeyword: formline PICTURE,LIST
# * OpKeyword: getc FILEHANDLE
# * OpKeyword: getc
# * OpKeyword: getlogin
# * OpKeyword: getpeername SOCKET
# * OpKeyword: getpgrp PID
# * OpKeyword: getppid
# * OpKeyword: getpriority WHICH,WHO
# * OpKeyword: getpwnam NAME
# * OpKeyword: getgrnam NAME
# * OpKeyword: gethostbyname NAME
# * OpKeyword: getnetbyname NAME
# * OpKeyword: getprotobyname NAME
# * OpKeyword: getpwuid UID
# * OpKeyword: getgrgid GID
# * OpKeyword: getservbyname NAME,PROTO
# * OpKeyword: gethostbyaddr ADDR,ADDRTYPE
# * OpKeyword: getnetbyaddr ADDR,ADDRTYPE
# * OpKeyword: getprotobynumber NUMBER
# * OpKeyword: getservbyport PORT,PROTO
# * OpKeyword: getpwent
# * OpKeyword: getgrent
# * OpKeyword: gethostent
# * OpKeyword: getnetent
# * OpKeyword: getprotoent
# * OpKeyword: getservent
# * OpKeyword: setpwent
# * OpKeyword: setgrent
# * OpKeyword: sethostent STAYOPEN
# * OpKeyword: setnetent STAYOPEN
# * OpKeyword: setprotoent STAYOPEN
# * OpKeyword: setservent STAYOPEN
# * OpKeyword: endpwent
# * OpKeyword: endgrent
# * OpKeyword: endhostent
# * OpKeyword: endnetent
# * OpKeyword: endprotoent
# * OpKeyword: endservent
# * OpKeyword: getsockname SOCKET
# * OpKeyword: getsockopt SOCKET,LEVEL,OPTNAME
# * OpKeyword: glob EXPR
#              glob
# * OpKeyword: gmtime EXPR
#              gmtime
# * OpKeyword: goto LABEL
#              goto EXPR
#              goto &NAME
# * OpKeyword: grep BLOCK LIST
#              grep EXPR,LIST
# * OpKeyword: hex EXPR
#              hex
# * OpKeyword: import LIST
# * OpKeyword: index STR,SUBSTR,POSITION
#              index STR,SUBSTR
# * OpKeyword: int EXPR
#              int
# * OpKeyword: ioctl FILEHANDLE,FUNCTION,SCALAR
# * OpKeyword: join EXPR,LIST
# * OpKeyword: keys HASH
#              keys ARRAY
# * OpKeyword: kill SIGNAL, LIST
#              kill SIGNAL
# * OpKeyword: last LABEL
#              last EXPR
#              last
# * OpKeyword: lc EXPR
#              lc
# * OpKeyword: lcfirst EXPR
#              lcfirst
# * OpKeyword: length EXPR
#              length
# * OpKeyword: link OLDFILE,NEWFILE
# * OpKeyword: listen SOCKET,QUEUESIZE
# * OpKeyword: local EXPR
# * OpKeyword: localtime EXPR
#              localtime
# * OpKeyword: lock THING
# * OpKeyword: log EXPR
#              log
# * OpKeyword: lstat FILEHANDLE
#              lstat EXPR
#              lstat DIRHANDLE
#              lstat
# * OpKeyword: map BLOCK LIST
#              map EXPR,LIST
# * OpKeyword: mkdir FILENAME,MODE
#              mkdir FILENAME
#              mkdir
# * OpKeyword: msgctl ID,CMD,ARG
# * OpKeyword: msgget KEY,FLAGS
# * OpKeyword: msgrcv ID,VAR,SIZE,TYPE,FLAGS
# * OpKeyword: msgsnd ID,MSG,FLAGS
# * OpKeyword: my VARLIST
#              my TYPE VARLIST
#              my VARLIST : ATTRS
#              my TYPE VARLIST : ATTRS
# * OpKeyword: next LABEL
#              next EXPR
#              next
# * OpKeyword: no MODULE VERSION LIST
#              no MODULE VERSION
#              no MODULE LIST
#              no MODULE
#              no VERSION
# * OpKeyword: oct EXPR
#              oct
# * OpKeyword: open FILEHANDLE,EXPR
#              open FILEHANDLE,MODE,EXPR
#              open FILEHANDLE,MODE,EXPR,LIST
#              open FILEHANDLE,MODE,REFERENCE
#              open FILEHANDLE
# * OpKeyword: opendir DIRHANDLE,EXPR
# * OpKeyword: ord EXPR
#              ord
# * OpKeyword: our VARLIST
#              our TYPE VARLIST
#              our VARLIST : ATTRS
#              our TYPE VARLIST : ATTRS
# * OpKeyword: pack TEMPLATE,LIST
# * OpKeyword: package NAMESPACE
#              package NAMESPACE VERSION
#              package NAMESPACE BLOCK
#              package NAMESPACE VERSION BLOCK
# * OpKeyword: pipe READHANDLE,WRITEHANDLE
# * OpKeyword: pop ARRAY
#              pop
# * OpKeyword: pos SCALAR
#              pos
# * OpKeyword: print FILEHANDLE LIST
#              print FILEHANDLE
#              print LIST
#              print
# * OpKeyword: printf FILEHANDLE FORMAT, LIST
#              printf FILEHANDLE
#              printf FORMAT, LIST
#              printf
# * OpKeyword: prototype FUNCTION
#              prototype
# * OpKeyword: push ARRAY,LIST
# * OpKeyword: quotemeta EXPR
#              quotemeta
# * OpKeyword: rand EXPR
#              rand
# * OpKeyword: read FILEHANDLE,SCALAR,LENGTH,OFFSET
#              read FILEHANDLE,SCALAR,LENGTH
# * OpKeyword: readdir DIRHANDLE
# * OpKeyword: readline EXPR
#              readline
# * OpKeyword: readlink EXPR
#              readlink
# * OpKeyword: readpipe EXPR
#              readpipe
# * OpKeyword: recv SOCKET,SCALAR,LENGTH,FLAGS
# * OpKeyword: redo LABEL
#              redo EXPR
#              redo
# * OpKeyword: ref EXPR
#              ref
# * OpKeyword: rename OLDNAME,NEWNAME
# * OpKeyword: require VERSION
#              require EXPR
#              require
# * OpKeyword: reset EXPR
#              reset
# * OpKeyword: return EXPR
#              return
# * OpKeyword: reverse LIST
# * OpKeyword: rewinddir DIRHANDLE
# * OpKeyword: rindex STR,SUBSTR,POSITION
#              rindex STR,SUBSTR
# * OpKeyword: rmdir FILENAME
#              rmdir
# * OpKeyword: say FILEHANDLE LIST
#              say FILEHANDLE
#              say LIST
#              say
# * OpKeyword: scalar EXPR
# * OpKeyword: seek FILEHANDLE,POSITION,WHENCE
# * OpKeyword: seekdir DIRHANDLE,POS
# * OpKeyword: select FILEHANDLE
#              select
#              select RBITS,WBITS,EBITS,TIMEOUT
# * OpKeyword: semctl ID,SEMNUM,CMD,ARG
# * OpKeyword: semget KEY,NSEMS,FLAGS
# * OpKeyword: semop KEY,OPSTRING
# * OpKeyword: send SOCKET,MSG,FLAGS,TO
#              send SOCKET,MSG,FLAGS
# * OpKeyword: setpgrp PID,PGRP
# * OpKeyword: setpriority WHICH,WHO,PRIORITY
# * OpKeyword: setsockopt SOCKET,LEVEL,OPTNAME,OPTVAL
# * OpKeyword: shift ARRAY
#              shift
# * OpKeyword: shmctl ID,CMD,ARG
# * OpKeyword: shmget KEY,SIZE,FLAGS
# * OpKeyword: shmread ID,VAR,POS,SIZE
# * OpKeyword: shmwrite ID,STRING,POS,SIZE
# * OpKeyword: shutdown SOCKET,HOW
# * OpKeyword: sin EXPR
#              sin
# * OpKeyword: sleep EXPR
#              sleep
# * OpKeyword: socket SOCKET,DOMAIN,TYPE,PROTOCOL
# * OpKeyword: socketpair SOCKET1,SOCKET2,DOMAIN,TYPE,PROTOCOL
# * OpKeyword: sort SUBNAME LIST
#              sort BLOCK LIST
#              sort LIST
# * OpKeyword: splice ARRAY,OFFSET,LENGTH,LIST
#              splice ARRAY,OFFSET,LENGTH
#              splice ARRAY,OFFSET
#              splice ARRAY
# * OpKeyword: split /PATTERN/,EXPR,LIMIT
#              split /PATTERN/,EXPR
#              split /PATTERN/
#              split
# * OpKeyword: sprintf FORMAT, LIST
# * OpKeyword: srand EXPR
#              srand
# * OpKeyword: stat FILEHANDLE
#              stat EXPR
#              stat DIRHANDLE
#              stat
# * OpKeyword: state VARLIST
#              state TYPE VARLIST
#              state VARLIST : ATTRS
#              state TYPE VARLIST : ATTRS
# * OpKeyword: study SCALAR
#              study
# * OpKeyword: sub NAME BLOCK
#              sub NAME (PROTO) BLOCK
#              sub NAME : ATTRS BLOCK
#              sub NAME (PROTO) : ATTRS BLOCK
# * OpKeyword: substr EXPR,OFFSET,LENGTH,REPLACEMENT
#              substr EXPR,OFFSET,LENGTH
#              substr EXPR,OFFSET
# * OpKeyword: symlink OLDFILE,NEWFILE
# * OpKeyword: syscall NUMBER, LIST
# * OpKeyword: sysopen FILEHANDLE,FILENAME,MODE
#              sysopen FILEHANDLE,FILENAME,MODE,PERMS
# * OpKeyword: sysread FILEHANDLE,SCALAR,LENGTH,OFFSET
#              sysread FILEHANDLE,SCALAR,LENGTH
# * OpKeyword: sysseek FILEHANDLE,POSITION,WHENCE
# * OpKeyword: system LIST
#              system PROGRAM LIST
# * OpKeyword: syswrite FILEHANDLE,SCALAR,LENGTH,OFFSET
#              syswrite FILEHANDLE,SCALAR,LENGTH
#              syswrite FILEHANDLE,SCALAR
# * OpKeyword: tell FILEHANDLE
#              tell
# * OpKeyword: telldir DIRHANDLE
# * OpKeyword: tie VARIABLE,CLASSNAME,LIST
# * OpKeyword: tied VARIABLE
# * OpKeyword: time
# * OpKeyword: times
# * OpKeyword: truncate FILEHANDLE,LENGTH
#              truncate EXPR,LENGTH
# * OpKeyword: uc EXPR
#              uc
# * OpKeyword: ucfirst EXPR
#              ucfirst
# * OpKeyword: umask EXPR
#              umask
# * OpKeyword: undef EXPR
#              undef
# * OpKeyword: unlink LIST
#              unlink
# * OpKeyword: unpack TEMPLATE,EXPR
#              unpack TEMPLATE
# * OpKeyword: unshift ARRAY,LIST
# * OpKeyword: untie VARIABLE
# * OpKeyword: use Module VERSION LIST
#              use Module VERSION
#              use Module LIST
#              use Module
#              use VERSION
# * OpKeyword: utime LIST
# * OpKeyword: values HASH
#              values ARRAY
# * OpKeyword: vec EXPR,OFFSET,BITS
# * OpKeyword: wait
# * OpKeyword: waitpid PID,FLAGS
# * OpKeyword: wantarray
# * OpKeyword: warn LIST
# * OpKeyword: write FILEHANDLE
#              write EXPR
#              write

# These are flows - more complex
# * OpKeyword: else
# * OpKeyword: elsif
# * OpKeyword: for
# * OpKeyword: foreach
# * OpKeyword: if
# * OpKeyword: unless
# * OpKeyword: until
# * OpKeyword: while
# * OpKeyword: elseif
# * OpKeyword: default
# * OpKeyword: given
# * OpKeyword: when
# * OpKeywordContinue: continue

# File ops:
# From perlfunc:
#-r  File is readable by effective uid/gid.
#-w  File is writable by effective uid/gid.
#-x  File is executable by effective uid/gid.
#-o  File is owned by effective uid.
#
#-R  File is readable by real uid/gid.
#-W  File is writable by real uid/gid.
#-X  File is executable by real uid/gid.
#-O  File is owned by real uid.
#
#-e  File exists.
#-z  File has zero size (is empty).
#-s  File has nonzero size (returns size in bytes).
#
#-f  File is a plain file.
#-d  File is a directory.
#-l  File is a symbolic link (false if symlinks aren't
#    supported by the file system).
#-p  File is a named pipe (FIFO), or Filehandle is a pipe.
#-S  File is a socket.
#-b  File is a block special file.
#-c  File is a character special file.
#-t  Filehandle is opened to a tty.
#
#-u  File has setuid bit set.
#-g  File has setgid bit set.
#-k  File has sticky bit set.
#
#-T  File is an ASCII or UTF-8 text file (heuristic guess).
#-B  File is a "binary" file (opposite of -T).
#
#-M  Script start time minus file modification time, in days.
#-A  Same for access time.
#-C  Same for inode change time (Unix, may differ for other
#    platforms)

# These are more difficult so they're removed fromt the list above
# add with caution
# * OpKeyword: m//
# * OpKeyword: q/STRING/
# * OpKeyword: qq/STRING/
# * OpKeyword: qw/STRING/
# * OpKeyword: qx/STRING/
# * OpKeyword: qr/STRING/
# * OpKeyword: s///
# * OpKeyword: tr///
# * OpKeyword: y///


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
