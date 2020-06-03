use strict;
use warnings;
use Guacamole::Test;
use Test::More;

parses('<>');
parses('<$fh>');
parses('<<>>');
parses('<<$fh>>');
parsent('< $fh >');
parsent('<< $fh >>');
parsent('<@fh>');
parsent('<<@fh>>');
parsent('<fh>');
parsent('<<fh>>');
parsent('<fh()>');
parsent('<<fh()>>');
parses('while (<>) {...}');
parses('while ( my $line = <>) {...}');
parses('while (<<>>) {...}');
parses('while ( my $line = <<>>) {...}');

TODO: {
    local $TODO = 'Support both diamond operators and builtin filehandles';

    parses('while (<STDIN>) {...}');
    parses('while ( my $line = <STDIN>) {...}');

    parses('while (<<STDIN>>) {...}');
    parses('while ( my $line = <<STDIN>>) {...}');
}

done_testing();
