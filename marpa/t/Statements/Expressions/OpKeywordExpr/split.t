use strict;
use warnings;
use Guacamole::Test;

parses('split //');
parses('split //, $foo');
parses('split /foo/, $foo');
parses('split /foo/x, $foo');
parses('split /foo/xms, $foo');

parses('split m{}');
parses('split m{}, $foo');
parses('split m{foo}, $foo');
parses('split m{foo}x, $foo');
parses('split m{foo}xms, $foo');

done_testing();
