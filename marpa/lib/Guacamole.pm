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

Statement ::= Expression
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

Value ::= Literal
        | Variable
        | UnderscoreValues
        | SubCall
        | OpKeywordDoExpr
        | LParen Expression RParen

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

CallArgs ::= LParen Expression RParen
           | LParen RParen

Block ::= LBrace StatementSeq RBrace

ArrayElem ::= LBracket Expression RBracket

HashElem ::= LBrace Expression RBrace

Ident ::= IdentComp 
        | IdentComp PackageSep Ident
        | Ident PackageSep

Literal ::= LitNumber
          | LitArray
          | LitHash

LitArray ::= LBracket Expression RBracket

LitHash ::= LBrace Expression RBrace

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
            | OpKeywordContinueExpr
            | OpKeywordCosExpr
            | OpKeywordCryptExpr
            | OpKeywordDbmcloseExpr
            | OpKeywordDbmopenExpr
            | OpKeywordDefinedExpr
            | OpKeywordDeleteExpr
            | OpKeywordDieExpr
            | OpKeywordDumpExpr
            | OpKeywordEachExpr
            | OpKeywordEofExpr
            | OpKeywordEvalExpr
            | OpKeywordEvalbytesExpr
            | OpKeywordExecExpr
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
            | OpKeywordGetsocknameExpr
            | OpKeywordGlobExpr
            | OpKeywordGmtimeExpr
            | OpKeywordGotoExpr
            | OpKeywordHex
            | OpKeywordIndex
            | OpKeywordInt
            | OpKeywordIoctl
            | OpKeywordKeys
            | OpKeywordKill
            | OpKeywordLast
            | OpKeywordLc
            | OpKeywordLcfirst
            | OpKeywordLength
            | OpKeywordLink
            | OpKeywordListen
            | OpKeywordLocal
            | OpKeywordLocaltime
            | OpKeywordLock
            | OpKeywordLog
            | OpKeywordLstat
            | OpKeywordMkdir
            | OpKeywordMsgctl
            | OpKeywordMsgget
            | OpKeywordMsgrcv
            | OpKeywordMsgsnd
            | OpKeywordNext
            | OpKeywordOct
            | OpKeywordOpendir
            | OpKeywordOrd
            | OpKeywordPack
            | OpKeywordPipe
            | OpKeywordPop
            | OpKeywordPos
            | OpKeywordPush
            | OpKeywordQuotemeta
            | OpKeywordRand
            | OpKeywordRead
            | OpKeywordReaddir
            | OpKeywordReadline
            | OpKeywordReadlink
            | OpKeywordReadpipe
            | OpKeywordRecv
            | OpKeywordRedo
            | OpKeywordRef
            | OpKeywordRename
            | OpKeywordReset
            | OpKeywordReturn
            | OpKeywordReverse
            | OpKeywordRewinddir
            | OpKeywordRindex
            | OpKeywordRmdir
            | OpKeywordScalar
            | OpKeywordSeek
            | OpKeywordSeekdir
            | OpKeywordSelect
            | OpKeywordSemctl
            | OpKeywordSemget
            | OpKeywordSemop
            | OpKeywordSend
            | OpKeywordSetpgrp
            | OpKeywordSetpriority
            | OpKeywordSetsockopt
            | OpKeywordShift
            | OpKeywordShmctl
            | OpKeywordShmget
            | OpKeywordShmread
            | OpKeywordShmwrite
            | OpKeywordShutdown
            | OpKeywordSin
            | OpKeywordSleep
            | OpKeywordSocket
            | OpKeywordSocketpair
            #| OpKeywordSplice
            #| OpKeywordSplit
            #| OpKeywordSprintf
            #| OpKeywordSrand
            #| OpKeywordStat
            #| OpKeywordState
            #| OpKeywordStudy
            #| OpKeywordSub
            #| OpKeywordSubstr
            #| OpKeywordSymlink
            #| OpKeywordSyscall
            #| OpKeywordSysopen
            #| OpKeywordSysread
            #| OpKeywordSysseek
            #| OpKeywordSystem
            #| OpKeywordSyswrite
            #| OpKeywordTell
            #| OpKeywordTelldir
            #| OpKeywordTie
            #| OpKeywordTied
            #| OpKeywordTime
            #| OpKeywordTimes
            #| OpKeywordTruncate
            #| OpKeywordUc
            #| OpKeywordUcfirst
            #| OpKeywordUmask
            #| OpKeywordUndef
            #| OpKeywordUnlink
            #| OpKeywordUnpack
            #| OpKeywordUnshift
            #| OpKeywordUntie
            #| OpKeywordUse
            #| OpKeywordUtime
            #| OpKeywordValues
            #| OpKeywordVec
            #| OpKeywordWait
            #| OpKeywordWaitpid
            #| OpKeywordWantarray
            #| OpKeywordWarn
            #| OpKeywordWrite

# TODO: (Add the following above)
#| OpKeywordGrepExpr
#| OpKeywordJoin
#| OpKeywordMap
#| OpKeywordMy
#| OpKeywordNo
#| OpKeywordOpen
#| OpKeywordOur
#| OpKeywordPackage
#| OpKeywordPrint
#| OpKeywordPrintf
#| OpKeywordPrototype
#| OpKeywordRequire
#| OpKeywordSay
#| OpKeywordSort

# Grammar for keywords
OpKeywordAbsExpr ::= OpKeywordAbs Expression
                   | OpKeywordAbs

OpKeywordAcceptExpr ::= OpKeywordAccept Value Value

OpKeywordAlarmExpr ::= OpKeywordAlarm Expression
                     | OpKeywordAlarm

OpKeywordAtan2Expr ::= OpKeywordAtan2 Value Value

OpKeywordBindExpr ::= OpKeywordBind Value Value

OpKeywordBinmodeExpr ::= OpKeywordBinmode Value Value
                       | OpKeywordBinmode Expression

OpKeywordBlessExpr ::= OpKeywordBless Value Value
                     | OpKeywordBless Expression

OpKeywordBreakExpr ::= OpKeywordBreak

OpKeywordCallerExpr ::= OpKeywordCaller Expression
                      | OpKeywordCaller

OpKeywordChdirExpr ::= OpKeywordChdir Expression
                     | OpKeywordChdir

OpKeywordChmodExpr ::= OpKeywordChmod Value Expression

OpKeywordChompExpr ::= OpKeywordChomp Expression
                     | OpKeywordChomp

OpKeywordChopExpr ::= OpKeywordChop Expression
                    | OpKeywordChop

OpKeywordChownExpr ::= OpKeywordChown Value Value Expression

OpKeywordChrExpr ::= OpKeywordChr Expression
                   | OpKeywordChr

OpKeywordChrootExpr ::= OpKeywordChroot Expression
                      | OpKeywordChroot

OpKeywordCloseExpr ::= OpKeywordClose Expression
                     | OpKeywordClose

OpKeywordClosedirExpr ::= OpKeywordClosedir Expression

OpKeywordConnectExpr ::= OpKeywordConnect Value Value

OpKeywordContinueExpr ::= OpKeywordContinue Block
                        | OpKeywordContinue

OpKeywordCosExpr ::= OpKeywordCos Expression

OpKeywordCryptExpr ::= OpKeywordCrypt Value Value

OpKeywordDbmcloseExpr ::= OpKeywordDbmclose VarHash

OpKeywordDbmopenExpr ::= OpKeywordDbmopen VarHash Value Value

OpKeywordDefinedExpr ::= OpKeywordDefined Expression
                       | OpKeywordDefined

OpKeywordDeleteExpr ::= OpKeywordDelete Expression

OpKeywordDieExpr ::= OpKeywordDie Expression

OpKeywordDoExpr ::= OpKeywordDo Block
                  | OpKeywordDo Expression

OpKeywordDumpExpr ::= OpKeywordDump Label
                    | OpKeywordDump Expression
                    | OpKeywordDump

OpKeywordEachExpr ::= OpKeywordEach Expression

OpKeywordEofExpr ::= OpKeywordEof Expression
                   | OpKeywordEof

OpKeywordEvalExpr ::= OpKeywordEval Block

OpKeywordEvalbytesExpr ::= OpKeywordEvalbytes Expression
                         | OpKeywordEvalbytes

OpKeywordExecExpr ::= OpKeywordExec Expression

OpKeywordExistsExpr ::= OpKeywordExists Expression

OpKeywordExitExpr ::= OpKeywordExit Expression
                    | OpKeywordExit

OpKeywordExpExpr ::= OpKeywordExp Expression
                   | OpKeywordExp

OpKeywordFcExpr ::= OpKeywordFc Expression
                  | OpKeywordFc

OpKeywordFcntlExpr ::= OpKeywordFcntl Value Value Value
                     | OpKeywordFcntl Expression

OpKeywordFilenoExpr ::= OpKeywordFileno Expression

OpKeywordFlockExpr ::= OpKeywordFlock Value Value

OpKeywordForkExpr ::= OpKeywordFork

OpKeywordGetcExpr ::= OpKeywordGetc Expression
                    | OpKeywordGetc

OpKeywordGetloginExpr ::= OpKeywordGetlogin

OpKeywordGetpeernameExpr ::= OpKeywordGetpeername Expression

OpKeywordGetpgrpExpr ::= OpKeywordGetpgrp Expression

OpKeywordGetppidExpr ::= OpKeywordGetppid

OpKeywordGetpriorityExpr ::= OpKeywordGetpriority Value Value

OpKeywordGetpwnamExpr ::= OpKeywordGetpwnam Expression

OpKeywordGetgrnamExpr ::= OpKeywordGetgrnam Expression

OpKeywordGethostbynameExpr ::= OpKeywordGethostbyname Expression

OpKeywordGetnetbynameExpr ::= OpKeywordGetnetbyname Expression

OpKeywordGetprotobynameExpr ::= OpKeywordGetprotobyname Expression

OpKeywordGetpwuidExpr ::= OpKeywordGetpwuid Expression

OpKeywordGetgrgidExpr ::= OpKeywordGetgrgid Expression

OpKeywordGetservbynameExpr ::= OpKeywordGetservbyname Value Value

OpKeywordGethostbyaddrExpr ::= OpKeywordGethostbyaddr Value Value

OpKeywordGetnetbyaddrExpr ::= OpKeywordGetnetbyaddr Value Value

OpKeywordGetprotobynumberExpr ::= OpKeywordGetprotobynumber Expression

OpKeywordGetservbyportExpr ::= OpKeywordGetservbyport Value Value

OpKeywordGetpwentExpr ::= OpKeywordGetpwent

OpKeywordGetgrentExpr ::= OpKeywordGetgrent

OpKeywordGethostentExpr ::= OpKeywordGethostent

OpKeywordGetnetentExpr ::= OpKeywordGetnetent

OpKeywordGetprotoentExpr ::= OpKeywordGetprotoent

OpKeywordGetserventExpr ::= OpKeywordGetservent

OpKeywordSetpwentExpr ::= OpKeywordSetpwent

OpKeywordSetgrentExpr ::= OpKeywordSetgrent

OpKeywordSethostentExpr ::= OpKeywordSethostent Expression

OpKeywordSetnetentExpr ::= OpKeywordSetnetent Expression

OpKeywordSetprotoentExpr ::= OpKeywordSetprotoent Expression

OpKeywordSetserventExpr ::= OpKeywordSetservent Expression

OpKeywordEndpwentExpr ::= OpKeywordEndpwent

OpKeywordEndgrentExpr ::= OpKeywordEndgrent

OpKeywordEndhostentExpr ::= OpKeywordEndhostent

OpKeywordEndnetentExpr ::= OpKeywordEndnetent

OpKeywordEndprotoentExpr ::= OpKeywordEndprotoent

OpKeywordEndserventExpr ::= OpKeywordEndservent

OpKeywordGetsocknameExpr ::= OpKeywordGetsockname Expression

OpKeywordGetsockoptExpr ::= OpKeywordGetsockopt Value Value Value

OpKeywordGlobExpr ::= OpKeywordGlob Expression
                    | OpKeywordGlob

OpKeywordGmtimeExpr ::= OpKeywordGmtime Expression
                      | OpKeywordGmtime

OpKeywordGotoExpr ::= OpKeywordGoto Label
                    | OpKeywordGoto Expression
                    | OpKeywordGoto SigilCode IdentComp

OpKeywordHexExpr ::= OpKeywordHex Expression
                   | OpKeywordHex

OpKeywordIndexExpr ::= OpKeywordIndex Value Value Value
                     | OpKeywordIndex Value Value

OpKeywordIntExpr ::= OpKeywordInt Expression
                   | OpKeywordInt

OpKeywordIoctlExpr ::= OpKeywordIoctl Value Value Value

#TODO: OpKeywordJoinExpr ::=

OpKeywordKeysExpr ::= OpKeywordKeys VarHash
                    | OpKeywordKeys VarArray
                    | OpKeywordKeys Expression

OpKeywordKillExpr ::= OpKeywordKill Value Expression
                    | OpKeywordKill Expression

OpKeywordLastExpr ::= OpKeywordLast Label
                    | OpKeywordLast Expression
                    | OpKeywordLast

OpKeywordLcExpr ::= OpKeywordLc Expression
                  | OpKeywordLc

OpKeywordLcfirstExpr ::= OpKeywordLcfirst Expression
                       | OpKeywordLcfirst

OpKeywordLengthExpr ::= OpKeywordLength Expression
                      | OpKeywordLength

OpKeywordLinkExpr ::= OpKeywordLink Value Value

OpKeywordListenExpr ::= OpKeywordListen Value Value

OpKeywordLocalExpr ::= OpKeywordLocal Expression

OpKeywordLocaltimeExpr ::= OpKeywordLocaltime Expression
                         | OpKeywordLocaltime

OpKeywordLockExpr ::= OpKeywordLock Expression

OpKeywordLogExpr ::= OpKeywordLog Expression
                   | OpKeywordLog

OpKeywordLstatExpr ::= OpKeywordLstat Expression
                     | OpKeywordLstat

#TODO: OpKeywordMapExpr ::= OpKeywordMap Expression

OpKeywordMkdirExpr ::= OpKeywordMkdir Value Value
                     | OpKeywordMkdir Expression
                     | OpKeywordMkdir

OpKeywordMsgctlExpr ::= OpKeywordMsgctl Value Value Value

OpKeywordMsggetExpr ::= OpKeywordMsgget Value Value

OpKeywordMsgrcvExpr ::= OpKeywordMsgrcv Value Value Value Value Value

OpKeywordMsgsndExpr ::= OpKeywordMsgsnd Value Value Value

# TODO: OpKeywordMyExpr ::= OpKeywordMy Expression

OpKeywordNextExpr ::= OpKeywordNext Label
                    | OpKeywordNext Expression
                    | OpKeywordNext

# TODO: OpKeywordNoExpr ::= OpKeywordNo Expression

OpKeywordOctExpr ::= OpKeywordOct Expression
                   | OpKeywordOct

# TODO: OpKeywordOpenExpr ::= OpKeywordOpen

OpKeywordOpendirExpr ::= OpKeywordOpendir Value Value

OpKeywordOrdExpr ::= OpKeywordOrd Expression
                   | OpKeywordOrd

# TODO: OpKeywordOurExpr ::= OpKeywordOur Expression

OpKeywordPackExpr ::= OpKeywordPack Value Value

# TODO: OpKeywordPackageExpr ::= OpKeywordPackage

OpKeywordPipeExpr ::= OpKeywordPipe Value Value

OpKeywordPopExpr ::= OpKeywordPop Expression
                   | OpKeywordPop

OpKeywordPosExpr ::= OpKeywordPos Expression
                   | OpKeywordPos

# TODO: OpKeywordPrintExpr ::= OpKeywordPrint Expression
# TODO: OpKeywordPrintfExpr ::= OpKeywordPrintf Expression
# TODO: OpKeywordPrototypeExpr ::= OpKeywordPrototype Expression

OpKeywordPushExpr ::= OpKeywordPush Value Expression

OpKeywordQuotemetaExpr ::= OpKeywordQuotemeta Expression
                         | OpKeywordQuotemeta

OpKeywordRandExpr ::= OpKeywordRand Expression
                    | OpKeywordRand

OpKeywordReadExpr ::= OpKeywordRead Value Value Value Expression
                    | OpKeywordRead Value Value Expression

OpKeywordReaddirExpr ::= OpKeywordReaddir Expression

OpKeywordReadlineExpr ::= OpKeywordReadline Expression
                        | OpKeywordReadline

OpKeywordReadlinkExpr ::= OpKeywordReadlink Expression
                        | OpKeywordReadlink

OpKeywordReadpipeExpr ::= OpKeywordReadpipe Expression
                        | OpKeywordReadpipe

OpKeywordRecvExpr ::= OpKeywordRecv Value Value Value Value

OpKeywordRedoExpr ::= OpKeywordRedo Label
                    | OpKeywordRedo Expression
                    | OpKeywordRedo

OpKeywordRefExpr ::= OpKeywordRef Expression
                   | OpKeywordRef

OpKeywordRenameExpr ::= OpKeywordRef Value Value

OpKeywordResetExpr ::= OpKeywordReset Expression
                     | OpKeywordReset

OpKeywordReturnExpr ::= OpKeywordReturn Expression
                      | OpKeywordReturn

OpKeywordReverseExpr ::= OpKeywordReverse Expression

OpKeywordRewinddirExpr ::= OpKeywordRewinddir Expression
                         | OpKeywordRewinddir

OpKeywordRindexExpr ::= OpKeywordRindex Value Value Value
                      | OpKeywordRindex Value Value

OpKeywordRmdirExpr ::= OpKeywordRmdir Expression
                     | OpKeywordRmdir

OpKeywordScalarExpr ::= OpKeywordScalar Expression

OpKeywordSeekExpr ::= OpKeywordSeek Value Value Value

OpKeywordSeekdirExpr ::= OpKeywordSeekdir Value Value

OpKeywordSelectExpr ::= OpKeywordSelect Value Value Value Value
                      | OpKeywordSelect Expression

OpKeywordSemctlExpr ::= OpKeywordSemctl Value Value Value Value

OpKeywordSemgetExpr ::= OpKeywordSemget Value Value Value

OpKeywordSemopExpr ::= OpKeywordSemop Value Value

OpKeywordSendExpr ::= OpKeywordSend Value Value Value Value
                    | OpKeywordSend Value Value Value

OpKeywordSetpgrpExpr ::= OpKeywordSetpgrp Value Value

OpKeywordSetpriorityExpr ::= OpKeywordSetpriority Value Value Value

OpKeywordSetsockoptExpr ::= OpKeywordSetsockopt Value Value Value Value

OpKeywordShiftExpr ::= OpKeywordShift Expression
                     | OpKeywordShift

OpKeywordShmctlExpr ::= OpKeywordShmctl Value Value Value

OpKeywordShmgetExpr ::= OpKeywordShmget Value Value Value

OpKeywordShmreadExpr ::= OpKeywordShmread Value Value Value Value

OpKeywordShmwriteExpr ::= OpKeywordShmwrite Value Value Value Value

OpKeywordShutdownExpr ::= OpKeywordShutdown Value Value

OpKeywordSinExpr ::= OpKeywordSin Expression
                   | OpKeywordSin

OpKeywordSleepExpr ::= OpKeywordSleep Expression
                     | OpKeywordSleep

OpKeywordSocketExpr ::= OpKeywordSocket Value Value Value Value

OpKeywordSocketpairExpr ::= OpKeywordSocketpair Value Value Value Value Value

###

IdentComp  ~ [a-zA-Z_]+
PackageSep ~ '::'

LitNumber ~ [0-9]+

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
# TODO: #OpKeywordGrep             ~ 'grep'
OpKeywordHex              ~ 'hex'
OpKeywordIndex            ~ 'index'
OpKeywordInt              ~ 'int'
OpKeywordIoctl            ~ 'ioctl'
#TODO: OpKeywordJoin             ~ 'join'
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
# TODO: OpKeywordMap              ~ 'map'
OpKeywordMkdir            ~ 'mkdir'
OpKeywordMsgctl           ~ 'msgctl'
OpKeywordMsgget           ~ 'msgget'
OpKeywordMsgrcv           ~ 'msgrcv'
OpKeywordMsgsnd           ~ 'msgsnd'
# TODO: OpKeywordMy               ~ 'my'
OpKeywordNext             ~ 'next'
# TODO: OpKeywordNo               ~ 'no'
OpKeywordOct              ~ 'oct'
# TODO: OpKeywordOpen             ~ 'open'
OpKeywordOpendir          ~ 'opendir'
OpKeywordOrd              ~ 'ord'
# TODO: OpKeywordOur              ~ 'our'
OpKeywordPack             ~ 'pack'
# TODO: OpKeywordPackage          ~ 'package'
OpKeywordPipe             ~ 'pipe'
OpKeywordPop              ~ 'pop'
OpKeywordPos              ~ 'pos'
# TODO: OpKeywordPrint            ~ 'print'
# TODO: OpKeywordPrintf           ~ 'printf'
# TODO: #OpKeywordPrototype        ~ 'prototype'
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
# TODO: OpKeywordSay              ~ 'say'
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
# TODO: OpKeywordSort             ~ 'sort'
#OpKeywordSplice           ~ 'splice'
#OpKeywordSplit            ~ 'split'
#OpKeywordSprintf          ~ 'sprintf'
#OpKeywordSrand            ~ 'srand'
#OpKeywordStat             ~ 'stat'
#OpKeywordState            ~ 'state'
#OpKeywordStudy            ~ 'study'
#OpKeywordSub              ~ 'sub'
#OpKeywordSubstr           ~ 'substr'
#OpKeywordSymlink          ~ 'symlink'
#OpKeywordSyscall          ~ 'syscall'
#OpKeywordSysopen          ~ 'sysopen'
#OpKeywordSysread          ~ 'sysread'
#OpKeywordSysseek          ~ 'sysseek'
#OpKeywordSystem           ~ 'system'
#OpKeywordSyswrite         ~ 'syswrite'
#OpKeywordTell             ~ 'tell'
#OpKeywordTelldir          ~ 'telldir'
#OpKeywordTie              ~ 'tie'
#OpKeywordTied             ~ 'tied'
#OpKeywordTime             ~ 'time'
#OpKeywordTimes            ~ 'times'
#OpKeywordTruncate         ~ 'truncate'
#OpKeywordUc               ~ 'uc'
#OpKeywordUcfirst          ~ 'ucfirst'
#OpKeywordUmask            ~ 'umask'
#OpKeywordUndef            ~ 'undef'
#OpKeywordUnlink           ~ 'unlink'
#OpKeywordUnpack           ~ 'unpack'
#OpKeywordUnshift          ~ 'unshift'
#OpKeywordUntie            ~ 'untie'
#OpKeywordUse              ~ 'use'
#OpKeywordUtime            ~ 'utime'
#OpKeywordValues           ~ 'values'
#OpKeywordVec              ~ 'vec'
#OpKeywordWait             ~ 'wait'
#OpKeywordWaitpid          ~ 'waitpid'
#OpKeywordWantarray        ~ 'wantarray'
#OpKeywordWarn             ~ 'warn'
#OpKeywordWrite            ~ 'write'

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
