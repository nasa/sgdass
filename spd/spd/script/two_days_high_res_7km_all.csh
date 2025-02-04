#!/bin/csh -f
umask 0002
setenv GOMP_STACKSIZE 8000000
limit stacksize       8000000
set num_proc = 3
setenv OMP_NUM_THREADS 18
cat /progs/spd_20220211/spd/share/two_days_high_res_7km_all.txt | parallel -k -j $num_proc 
