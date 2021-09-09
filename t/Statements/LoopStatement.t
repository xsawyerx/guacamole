use strict;
use warnings;

use Guacamole::Test;

# ForStatement
parses('for (@foo) {}');
parses('for (@foo) {} continue {}');
parses('for $foo (@bar) {}');
parses('for my $foo (@bar) {}');
parses('for ($i = 0; $i < 10; $ii) {}');
parses('for (my $i = 0; $i < 10; $ii) {}');
parses('for (; $i < 10; $ii) {}'); # cases with missing part or two
parses('for (my $i = 0;; $ii) {}');
parses('for (my $i = 0; $i < 10;) {}');
parses('for (;; $ii) {}');
parses('for (my $i = 0;;) {}');
parses('for (;;) {}'); # special case for infinite loop

parses('foreach (@foo) {}');
parses('foreach (@foo) {} continue {}');
parses('foreach $foo (@bar) {}');
parses('foreach my $foo (@bar) {}');
parses('foreach ($i = 0; $i < 10; $i) {}');
parses('foreach (my $i = 0; $i < 10; $i) {}');
parses('foreach (; $i < 10; $i) {}'); # cases with missing part or two
parses('foreach (my $i = 0;; $i) {}');
parses('foreach (my $i = 0; $i < 10;) {}');
parses('foreach (;; $i) {}');
parses('foreach (my $i = 0;;) {}');
parses('for (;;) {}'); # special case for infinite loop

# WhileStatement
parses('while ( $foo = shift ) {}');
parses('while ( my $foo = shift ) {}');
parses('while (1) {}');
parses('while (1) {} continue {}');
parses('while () {}'); # special case for infinite loop

# UntilStatement
parses('until ( $foo = shift ) {}');
parses('until ( my $foo = shift ) {}');
parses('until (1) {}');
parses('until (1) {} continue {}');

done_testing();
