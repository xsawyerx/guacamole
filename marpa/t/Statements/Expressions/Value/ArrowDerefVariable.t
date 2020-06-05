use strict;
use warnings;
use Test::More;
use Test::Fatal qw< exception >;
use Guacamole::Test;

foreach my $lead_sigil (qw< $ @ % & * >) {
    parses( sprintf '$foo->%s*', $lead_sigil );

    my $fail_str = sprintf '$foo->%s *', $lead_sigil;
    parsent($fail_str);
}

parses('$foo->$#*');

TODO: {
    todo_skip 'Expose bad $# patterns' => 2;
    parsent('$foo->$#');
    parsent('$foo->$# *');
}

foreach my $slice_sigil (qw< @ % >) {
    parses("\$foo->$slice_sigil\[ 0, 3 \]");
    parses("\$foo->$slice_sigil\{ 0, 3 \}");
}

parsent('$foo->@ [ "foo", "bar" ]');
parsent('$foo->% [ "foo", "bar" ]');
parsent('$foo->@ { "foo", "bar" }');
parsent('$foo->% { "foo", "bar" }');

done_testing();
