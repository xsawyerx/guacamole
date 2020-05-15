use strict;
use warnings;
use Guacamole::Test;

parses('$cnt = chmod 0755, "foo", "bar";');
parses('chmod 0755, @executables;');
parses('$mode = "0644";');
parses('chmod $mode, "foo";');
parses('$mode = "0644";');
parses('chmod oct($mode), "foo";');
parses('$mode = 0644;');
parses('chmod $mode, "foo";');
parses('chmod($perm | 0600, $fh);');
parses('chmod S_IRWXU()|S_IRGRP()|S_IXGRP()|S_IROTH()|S_IXOTH(), @executables;');

done_testing();
