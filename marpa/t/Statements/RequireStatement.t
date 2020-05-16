use strict;
use warnings;

use Guacamole::Test;

parses(q{require v5.10.0});
parses(q{require 5.010.000});

parses(q{require Foo});
parses(q{require Foo::Bar});

parses(q{require "Foo.pm"});
parses(q{require 'Foo.pm'});

parses(q{require "Foo/Bar.pm"});
parses(q{require 'Foo/Bar.pm'});

parses(q{require qq<Foo.pm>});
parses(q{require q<Foo.pm>});

parses(q{require qq<Foo/Bar.pm>});
parses(q{require q<Foo/Bar.pm>});

done_testing();
