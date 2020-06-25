use strict;
use warnings;
use Guacamole::Test;

parses('BEGIN {1}');
parses('CHECK {1}');
parses('INIT {1}');
parses('UNITCHECK {1}');
parses('END {1}');

parses('sub BEGIN {1}');
parses('sub CHECK {1}');
parses('sub INIT {1}');
parses('sub UNITCHECK {1}');
parses('sub END {1}');

done_testing();
