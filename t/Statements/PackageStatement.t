use strict;
use warnings;

use Guacamole::Test;

parses('package Foo');
parses('package Foo::Bar');
parses('package Foo;');
parses('package Foo::Bar;');

# you can have package q, qq, qw, qx, qr
parses('package q;');
parses('package qq;');
parses('package qw;');
parses('package qx;');
parses('package qr;');

parses('package Foo {1}');
parses('package Foo::Bar {1}');

parses('package Foo 1.4.0;');
parses('package Foo v1.4.0;');

parses('package Foo 1.4.0 {1}');
parses('package Foo v1.4.0 {1}');

done_testing();
