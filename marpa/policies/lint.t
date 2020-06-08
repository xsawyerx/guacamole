use strict;
use warnings;
use Guacamole::Linter;

lint_ok( 'ProhibitSingleArgArraySlice', 'say $foo->@[ 1 ]' );
lint_ok( 'ProhibitSingleArgArraySlice', 'say $foo->@[ "foo" ]' );
lint_ok( 'ProhibitSingleArgArraySlice', 'say $foo->@[ $foo ]' );
lint_ok( 'ProhibitSingleArgArraySlice', 'say $foo->@[ scalar foo() ]' );
lint_nok( 'ProhibitSingleArgArraySlice', 'say $foo->@[ foo() ]' );
lint_nok( 'ProhibitSingleArgArraySlice', 'say $foo->@[ 1, 2 ]' );
lint_nok( 'ProhibitSingleArgArraySlice', 'say $foo->@[ "foo", "bar" ]' );

done_testing();
