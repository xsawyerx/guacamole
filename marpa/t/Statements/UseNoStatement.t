use strict;
use warnings;

use Guacamole::Test;

foreach my $op ( qw< use no > ) {
    # use Module
    parses("$op Module");
    parses("$op My::Module");
    parses("$op My::Module::Foo");

    # use VERSION
    parses("$op 5.010");
    parses("$op 5.010.000");
    parses("$op v5.10");
    parses("$op v5.010");
    parses("$op v5.10.0");

    # use Module LIST
    parses("$op My::Module ( 'foo', 'bar' )");
    parses("$op My::Module ( 'foo' )");
    parses("$op My::Module qw< foo >");
    parses("$op My::Module ( qw< foo > )");

    # use Module VERSION
    parses("$op Module 4.10");
    parses("$op Module 4.10.999");
    parses("$op My::Module v5.200.123");
    parses("$op My::Module::Foo 5.3.5");
    parses("$op My::Module::Foo v5.3.5");

    # use Module VERSION LIST
    parses("$op Module 4.10 ( 'foo', 'bar' )");
    parses("$op Module 4.10.999 ('foo')");
    parses("$op My::Module v5.200.123 qw< foo >");
    parses("$op My::Module::Foo 5.3.5 ( qw< foo > )");
}

done_testing();
