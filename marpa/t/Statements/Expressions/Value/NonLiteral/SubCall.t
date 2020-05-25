use strict;
use warnings;
use Guacamole::Test;
use Test::More;

parses('a()');
parses('a::b()');

TODO: {
    local $TODO = 'We don\'t yet separate between Idents for SubCall and classes';
    parsent('a::()');
}

done_testing();
