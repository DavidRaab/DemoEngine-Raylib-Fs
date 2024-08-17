#!/usr/bin/env perl
use v5.36;
use open ':std', ':encoding(UTF-8)';

# runs raylibfs program with a profiler.
# use convert to convert last nettrace file created this way to a
# speedscope file that can be examined in browser.

exec(qw{dotnet-trace collect -- build/raylibfs});
