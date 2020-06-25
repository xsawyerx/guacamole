use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    "NAME" => "NoPit",
    "VERSION" => "0.1",
    # 5.22 is when OPpENTERSUB_NOPAREN was introduced
    "MIN_PERL_VERSION" => "5.22.0",
);
