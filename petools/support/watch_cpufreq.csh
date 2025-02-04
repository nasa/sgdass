#!/bin/csh
# ************************************************************************
# *                                                                      *
# *   Program for wathcing the cpu frequency if the CPU management is    *
# *   disabled.                                                          *
# *   Optional argument: cpu_id (a number from 0 to max_number of cpu-1) *
# *                                                                      *
# * ## 19-NOV-2017  watch_cpufreq.csh  v1.0 (c) L. Petrov 19-NOV-2017 ## *
# *                                                                      *
# ************************************************************************
if ( `uname` != "Linux" ) then
      echo "This program runs only under Linux"
      exit 0
endif
if ( $1 == "" ) then
     set cpu_id = 0
   else
     set cpu_id = $1
endif
if ( -d /sys/devices/system/cpu/cpu${cpu_id}/cpufreq ) then
     watch -n 0 cat /sys/devices/system/cpu/cpu${cpu_id}/cpufreq/scaling_cur_freq
  else
     echo "CPU frequency management is disabled at your system"
endif
