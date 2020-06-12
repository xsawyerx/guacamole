use strict;
use warnings;
use Guacamole::Test;
use Test::More;

parsent('$$foo');
parsent('@$foo');
parsent('%$foo');
parsent('*$foo');
parsent('$#$foo');

parsent('$ $foo');
parsent('@ $foo');
parsent('% $foo');
parsent('* $foo');

parses('${$foo}');
parses('@{$foo}');
parses('%{$foo}');
parses('*{$foo}');
parses('$#{$foo}');

parses(q!  ${ $foo->{'bar'} } !);
parses(q!  @{ $foo->{'bar'} } !);
parses(q!  %{ $foo->{'bar'} } !);
parses(q!  *{ $foo->{'bar'} } !);
parses(q! $#{ $foo->{'bar'} } !);

parses(q!  $ { $foo->{'bar'} } !);
parses(q!  @ { $foo->{'bar'} } !);
parses(q!  % { $foo->{'bar'} } !);
parses(q!  * { $foo->{'bar'} } !);
parses(q! $# { $foo->{'bar'} } !);

parsent('$$$foo');
parsent('@$$foo');
parsent('%$$foo');
parsent('*$$foo');
parsent('$#$$foo');

done_testing();
