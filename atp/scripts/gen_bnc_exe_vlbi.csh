#!/bin/csh -f
umask 0002
#
# ************************************************************************
# *                                                                      *
# *   Program gen_bnc_exe.csh check for log files in the system and in   *
# *   cddis without binary files and makes them				 *
# *                                                                      *
# * ### 19-MAR-2024  gen_sde_sub.csh v1.0 (c) N. Habana  19-MAR-2024 ### *
# *                                                                      *
# ************************************************************************
#
#
# -- Get PID
#
set xpid = `echo $$`
#
# -- Get all the log files that are currently on cddis but not available locally
#
python3 /progs/atp_20230928/scripts/get_logs.py >> /progs/atp_20230928/share/run_gen_bnc_exe_logs.txt;
#
# -- Write the available list of vlbi log files to file
#
python3 /progs/atp_20230928/scripts/write_log_paths_to_file.py vlbi 4.0 >> \
/progs/atp_20230928/share/run_gen_bnc_exe_logs.txt;
#
# -- Generate the binary files
#
set num_proc = 8
cat /progs/atp_20230928/share/vlbi_log_file_paths.txt | \
    parallel -k -j $num_proc python3                    \
    /progs/atp_20230928/scripts/l2b_sub_exe.py vlbi     \
                                               4.0      \
                                               {}       \
                                               $xpid;
#
# --- Generate plots that are not there yet [coming soon]
#


						
						
