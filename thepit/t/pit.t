use strict;
use warnings;
use 5.22.0;

use Test::More;

sub Foo { die "should not be called" }
sub Foo::bar { "ok" }
sub Foo::Bar { die "should not be called" }
sub Foo::Bar::baz { "ok" }
sub Bar { "Baz" }
sub Baz::foo { "ok" }

no pit;
is Foo->bar(), "ok";
is Foo::Bar->baz(), "ok";

use pit;
is Bar->foo(), "ok";

done_testing;
