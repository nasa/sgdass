#!/bin/csh -f
umask 0002
set num_proc = 1
setenv OMP_NUM_THREADS 6
cat /progs/spd_20220211/spd/share/two_days_7km_high_res_lis.txt | \
    parallel -k -j $num_proc \
    spd_3d /progs/spd_20220211/spd/share/spd_high_res_7km_vlbi.cnf \
       {} \
       /t0/7km/spd/high_res_asc/spd_vlbi_7km_ \
       3 \
       2006.01.01 \
       2007.01.01 
 
