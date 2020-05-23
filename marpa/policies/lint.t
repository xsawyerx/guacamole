use strict;
use warnings;
use Guacamole::Linter;

lint_fail( 'ProhibitSingleArgArraySlice', 'say $foo->@[ 1 ]' );
lint_fail( 'ProhibitSingleArgArraySlice', 'say $foo->@[ "foo" ]' );
lint_fail( 'ProhibitSingleArgArraySlice', 'say $foo->@[ $foo ]' );
lint_success( 'ProhibitSingleArgArraySlice', 'say $foo->@[ foo() ]' );
lint_success( 'ProhibitSingleArgArraySlice', 'say $foo->@[ 1, 2 ]' );
lint_success( 'ProhibitSingleArgArraySlice', 'say $foo->@[ "foo", "bar" ]' );

done_testing();
