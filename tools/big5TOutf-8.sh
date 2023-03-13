#!/bin/bash
#

for fname in ./*; do
        fname_out=${fname}_utf8
        iconv -f BIG5 -t UTF-8 <$fname >$fname_out
done;
