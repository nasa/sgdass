#!/bin/csh -f
# *****************************************************************************
# *                                                                           *
# *   C-shell program checks whtehre CPU throttling is enabled.               *
# *   When called with option -v, it prints the progress and explains         *
# *   the reason why it decided whethere the CPU throttling is enabled.       *
# *                                                                           *
# * ## 27-OCT-2017 check_cpu_throttling.csh v1.1 (c) L. Petrov 20-NOV-2017 ## *
# *                                                                           *
# *****************************************************************************
setenv LANG   C
setenv LC_ALL C
if ( `uname` != "Linux" ) then
      echo "This program runs only under Linux"
      exit 0
endif
#
if ( -d /sys/devices/system/cpu/cpu0/cpufreq == 0 ) then
     echo "No CPU Throttling"
     if ( $1 == '-v' ) then
          echo "because /sys/devices/system/cpu/cpu0/cpufreq directory does not exist"
          echo "that means frequency scalinig is not supported by the kernel"
     endif 
     exit 0
endif
if ( -f /sys/devices/system/cpu/cpu0/cpufreq/cpuinfo_max_freq == 0 ) then
     echo "No CPU Throttling"
     if ( $1 == '-v' ) then
          echo "because file /sys/devices/system/cpu/cpu0/cpufreq does not exist"
          echo "that means frequency scalinig is not supported by the kernel"
     endif 
     exit 0
  else
#
# -- Gather 5 reading of CU frequency with a pause of 1 sec
#
     set gov = `cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor`
     if ( $gov == "performance" || $gov == "powersave" || $gov == "userspace" ) then
          if ( $1 == '-v' ) then
               /bin/echo -ne "Checking a change in the CPU frequcency ... "
          endif
          set freq1 = `cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_cur_freq`
          if ( $1 == '-v' ) /bin/echo -ne "1 \c"
          sleep 1
          set freq2 = `cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_cur_freq`
          if ( $1 == '-v' ) /bin/echo -ne "2 \c"
          sleep 1
          set freq3 = `cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_cur_freq`
          if ( $1 == '-v' ) /bin/echo -ne "3 \c"
          sleep 1
          set freq4 = `cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_cur_freq`
          if ( $1 == '-v' ) /bin/echo -ne "4 \c"
          sleep 1
          set freq5 = `cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_cur_freq`
          if ( $1 == '-v' ) then
               /bin/echo -ne "\r                                                   \r\c"
          endif
#
# ------- Checks whether the frequency is stable or not
#
	  if ( $freq2 == $freq1 && $freq3 == $freq1 && $freq4 == $freq1 && $freq5 == $freq1 ) then
               echo "No CPU Throttling"
               if ( $1 == '-v' ) then
                    echo "because $gov frequency scaling governer is used and"
                    echo "the CPU frequency is stable: $freq1 MHz"
               endif 
               exit 0
             else
               echo "CPU throttling is enabled"
               if ( $1 == '-v' ) then
                    echo "because $gov frequency scaling governer is used,"
                    echo "but the CPU frequency is unstable: $freq1 $freq2 $freq3 $freq4 $freq5 MHz"
               endif 
               exit 0
          endif
       else
          set freq_min = `cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_min_freq`
          set freq_max = `cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_max_freq`
	  if ( $freq_min == $freq_max ) then
               echo "No CPU Throttling"
               if ( $1 == '-v' ) then
                    echo "because the frequency scaling governer is not used and"
                    echo "the minimum and maximum CPU frequencies are"
                    echo "the same: $freq_min, $freq_max"
               endif 
               exit 0
             else
               echo "CPU throttling is enabled"
               if ( $1 == '-v' ) then
                    echo "because the frequency scaling governer is not used,"
                    echo "but the minimum and maximum CPU frequencies are"
                    echo "the different: freq_min=$freq_min, freq_max=$freq_max"
               endif 
               exit 0
          endif
     endif
endif
