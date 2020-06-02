use strict;
use warnings;
use Guacamole::Test;

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

done_testing();
