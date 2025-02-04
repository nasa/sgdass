#!/bin/csh -f
umask 0002
set num_proc = 7
setenv OMP_NUM_THREADS 6
cat /progs/spd_20201103/spd/share/used_7km_lis.txt | \
    parallel -k -j $num_proc \
    spd_3d /progs/spd_20201103/spd/share/spd_7km_vlbi.cnf \
       {} \
       /s0/7km/spd/asc/spd_vlbi_7km_ \
       3 \
       2006.01.01 \
       2007.01.01 
