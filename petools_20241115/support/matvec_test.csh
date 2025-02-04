#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   C-shell program  matvec_test.csh performs either sanity or timing  *
# *   test of matvec library.                                            *
# *                                                                      *
# * ### 05-OCT-2002 matvec_test.csh v1.3 (c) L. Petrov  15-DEC-2006 ###  *
# *                                                                      *
# ************************************************************************
setenv LANG   C
setenv LC_ALL C
set outfile = /tmp/matvec.tst
#
if ( -f $outfile ) then
     rm -f $outfile 
endif
#
if ( $1 != "sanity" && $1 != "timer" ) then
     echo "matvec_test.csh: Wrong first argument: $1 --  sanity or timer was expected"
     exit 1
endif
set mul_mm_test = ${PETOOLS_ROOT}/bin/matvec_test
#
set DATE_LONG = `date "+%d-%h-%Y %H:%M:%S" | tr "[a-z]" "[A-Z]"`
if ( `uname` == "Linux" ) then
      echo "=== matvec test at `uname -n` on $DATE_LONG ===" | tee $outfile 
      echo " "                              | tee -a $outfile 
      cat /proc/cpuinfo | grep "model name" | sort -u | tee -a $outfile 
      cat /proc/cpuinfo | grep "cpu MHz"    | sort -u | tee -a $outfile 
      cat /proc/cpuinfo | grep "cache size" | sort -u | tee -a $outfile 
      cat /proc/cpuinfo | grep "flags"      | sort -u | tee -a $outfile 
      cat /proc/meminfo | grep "Mem:" | \
           awk '{printf "total memory    : %s bytes \n", $2}' | tee -a $outfile 
  else
     set machine = `uname -s -r -m`
     set mem_size=`sysctl -n hw.memsize`
     sysctl -n machdep.cpu.brand_string  | tee -a $outfile 
     echo "Amount of memory: $mem_size"  | tee -a $outfile 
     echo "=== matvec test at $machine on $DATE_LONG ===" | tee $outfile 
endif
echo "SOLVE_LIB_BLAS: $SOLVE_LIB_BLAS"
echo " " | tee -a $outfile 
#
$mul_mm_test iv_v  $1 | tee -a $outfile ; if ( $status != 0 ) goto failure 
$mul_mm_test tv_v  $1 | tee -a $outfile ; if ( $status != 0 ) goto failure 
$mul_mm_test sv_v  $1 | tee -a $outfile ; if ( $status != 0 ) goto failure 
#
$mul_mm_test invs  $1 | tee -a $outfile ; if ( $status != 0 ) goto failure 
#
$mul_mm_test ii_i  $1 | tee -a $outfile ; if ( $status != 0 ) goto failure 
$mul_mm_test it_i  $1 | tee -a $outfile ; if ( $status != 0 ) goto failure 
$mul_mm_test ti_i  $1 | tee -a $outfile ; if ( $status != 0 ) goto failure 
#
$mul_mm_test is_i  $1 | tee -a $outfile ; if ( $status != 0 ) goto failure 
$mul_mm_test ts_i  $1 | tee -a $outfile ; if ( $status != 0 ) goto failure 
$mul_mm_test ss_i  $1 | tee -a $outfile ; if ( $status != 0 ) goto failure 
$mul_mm_test si_i  $1 | tee -a $outfile ; if ( $status != 0 ) goto failure 
$mul_mm_test st_i  $1 | tee -a $outfile ; if ( $status != 0 ) goto failure 
#
$mul_mm_test ss_s  $1 | tee -a $outfile ; if ( $status != 0 ) goto failure 
$mul_mm_test ii_s  $1 | tee -a $outfile ; if ( $status != 0 ) goto failure 
$mul_mm_test it_s  $1 | tee -a $outfile ; if ( $status != 0 ) goto failure 
$mul_mm_test ti_s  $1 | tee -a $outfile ; if ( $status != 0 ) goto failure 
#
echo "Results are in file " $outfile
exit 0
######
failure:
echo "Test failed" | tee -a $outfile 
echo "Results are in file " $outfile
exit 1
