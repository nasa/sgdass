#!/bin/csh -f
# 
if ( $1 == "get_list" ) then
 find /imls/orig_data/mpiom06/asc/ -name "*.asc" | \
 grep "AOD1B_20" | \
 grep "X_06.asc" | \
 sed "s@/@ @g" | \
 sed "s@ AOD1B_@ @g" | \
 sed "s@_X_06@ @g" | \
 sed "s@-@ @g" | \
 awk '{printf "$MALO_DIR/script/nto_loading.py $MALO_DIR/share/astrogeo_nto_mpiom06.conf 111111111 %s%s%s 3\n", $6,$7,$8}' | grep -v  AOD1B_201 | sort -k 1r > /tmp/mpiom.com
endif 
if ( $1 == "get_load" ) then
     setenv OMP_NUM_THREADS 1
     cat /tmp/mpiom.com | parallel --jobs 16 :::
endif
