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
            || OpNamed Expression
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
        | SubCall
        | DoBlock
        | LParen Expression RParen

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

DoBlock ::= Do Block

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

###

IdentComp  ~ [a-zA-Z_]+
PackageSep ~ '::'

LitNumber ~ [0-9]+

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

OpArrow   ~ '->'
OpInc     ~ '++' | '--'
OpPower   ~ '**'
OpUnary   ~ '!' | '~' | '\' | '+' | '-'
OpRegex   ~ '=~' | '!~'
OpMulti   ~ '*' | '/' | '%' | 'x'
OpAdd     ~ '+' | '-' | '.'
OpShift   ~ '<<' | '>>'
OpNamed   ~ 'chdir' | 'rand'
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

Do ~ 'do'

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
