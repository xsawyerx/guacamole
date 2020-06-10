use strict;
use warnings;
use Test::More;
use Guacamole::Linter::Test;

my $test = Guacamole::Linter::Test->new();

$test->lint_ok( 'ProhibitSingleArgArraySlice', 'say $foo->@[ 1 ]' );
$test->lint_ok( 'ProhibitSingleArgArraySlice', 'say $foo->@[ "foo" ]' );
$test->lint_ok( 'ProhibitSingleArgArraySlice', 'say $foo->@[ $foo ]' );
$test->lint_ok( 'ProhibitSingleArgArraySlice', 'say $foo->@[ scalar foo() ]' );
$test->lint_nok( 'ProhibitSingleArgArraySlice', 'say $foo->@[ foo() ]' );
$test->lint_nok( 'ProhibitSingleArgArraySlice', 'say $foo->@[ 1, 2 ]' );
$test->lint_nok( 'ProhibitSingleArgArraySlice', 'say $foo->@[ "foo", "bar" ]' );

done_testing();
