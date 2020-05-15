use strict;
use warnings;

use Guacamole::Test;

parses('do { 1 }');
parses('do { { 1 } }');
parses('do { ( 1 => 2 ) }');
parses('do { +{ 1 => 2 } }');

parsent('do { 1, 2 }');
parsent('do { 1 => 2 }');
parsent('do { { 1 => 2 } }');

# parses('{;}'); should it?
parses('{pop}');
parses('{pop;}');
parses('{a(); b();}');

done_testing;
