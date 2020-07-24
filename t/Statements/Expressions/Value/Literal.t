use strict;
use warnings;
use Guacamole::Test;
use Test::More;

# LitNumberDec
parses('4');
parses('44');
parses('44.4');
parses("4.");
parses(".4");
parses("0");
parses("0.");
parses(".0");

parses(".0e1");
parses("0.e1");
parsent(".e1");

parses("0._4");
parses("1_.4");
parses(".4_");
parsent("._4");

parses("1._e1");
parses("1_.e1");
parses("1e_+_1");
parses("1e_+__1");
parsent("1e+_");
parsent("1e__+_1");

# LitNumberOct
parses("0755");
parses("07p1");
parses("07p-2");
parses("0_1");
parses("01_");
parses("0_7_5_5");
parsent("08");
parsent("0_");

parses("01.4p1");
parses("01.4_p1");
parses("01_.4p1");
parses("0_.4p1");
parsent("0_.4");

# LitNumberHex
parses("0xf");
parses("0XF");
parses("0xf_");
parses("0x_f");
parses("0xfP1");
parses("0xeP2_");
parses("0xfp-1");
parses("0x_ap+2");
parses("0xap+2_");
# parsent("0x_"); # FIXME: doesn't parse and doesn't error
parsent("0xap_+2");
parsent("0xap+_2");
parsent("0xap_");
parsent("0xap-_");

parses("0x1.8p1");
parses("0x1.8_p1");
parses("0x1_.8p1");
parses("0x_.8p1");
parsent("0x_.8");

# LitNumberBin
parses("0b0");
parses("0b1");
parses("0b001");
parses("0B1");
parses("0B_1");
parses("0B1_");
parsent("0b2");
parsent("0b1_2");
parsent("0b_");
parsent("0b_");

parses("0b1.1p1");
parses("0b1.1_p1");
parses("0b1_.1p1");
parses("0b_.1p1");
parsent("0b_.1");

# LitArray
parses('[]');
parses('[ 1, 2 ]');
parses('[ 1, 2, ( 10, () ) ]');

# LitHash
parses('{}');
parses('sub { {} }');
parses('sort {}, 1');
parses('$x = {}');
parses('$x = { "foo" => "bar" }');
parses('return { 1, 2, 3, 4 }');
parsent('{ 1, 2, 3, 4 }');

# LitString (SingleQuote)
parses(q{''});
parses(q{ 'hello' });
parses(q{ 'hello world \' foo " ' });

# InterpolString (DoubleQuote)
parses(q{""});
parses(q{ "hello" });
parses(q{ "hello world \" foo ' " });

done_testing();
