use strict;
use warnings;
use Test::More;
use Test::Fatal qw< exception >;
use Guacamole::Test;

foreach my $lead_sigil (qw< $ @ % & * >) {
    parses( sprintf '$foo->%s*', $lead_sigil );

    my $fail_str = sprintf '$foo->%s *', $lead_sigil;

    ok(
        exception( sub { parse_fail($fail_str) } ),
        "Failed to parse: $fail_str",
    );
}

parses('$foo->$#');

ok(
    exception( sub { parse_fail('$foo->$ #') } ),
    'Failed to parse: $foo->$ #',
);

foreach my $slice_sigil (qw< @ % >) {
    parses("\$foo->$slice_sigil\[ 0, 3 \]");
    parses("\$foo->$slice_sigil\{ 0, 3 \}");
}

ok(
    exception( sub { parse_fail('$foo->@ [ "foo", "bar" ]') } ),
    'Failed to parse: $foo->@ [ "foo", "bar" ]',
);

ok(
    exception( sub { parse_fail('$foo->% [ "foo", "bar" ]') } ),
    'Failed to parse: $foo->% [ "foo", "bar" ]',
);

ok(
    exception( sub { parse_fail('$foo->@ { "foo", "bar" }') } ),
    'Failed to parse: $foo->@ { "foo", "bar" }',
);

ok(
    exception( sub { parse_fail('$foo->% { "foo", "bar" }') } ),
    'Failed to parse: $foo->% { "foo", "bar" }',
);

done_testing();
