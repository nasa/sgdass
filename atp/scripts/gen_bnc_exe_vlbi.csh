#!/bin/csh -f
umask 0002
#
# ************************************************************************
# *                                                                      *
# *   Program gen_bnc_exe.csh check for log files in the system and in   *
# *   cddis without binary files and makes them				 *
# *                                                                      *
# *   Copyright (c) 1975-2025 United States Government as represented by *
# *   the Administrator of the National Aeronautics and Space            *
# *   Administration. All Rights Reserved.                               *
# *   License: NASA Open Source Software Agreement (NOSA).               *
# *                                                                      *
# * ### 19-MAR-2024  gen_sde_sub.csh v1.0 (d) N. Habana  14-MAY-2025 ### *
# *                                                                      *
# ************************************************************************
#
#
# -- Get PID
#
set xpid = `echo $$`
#
# -- Identify the library holding this version of the script
#
source /opt64/bin/atp_vars
#
# -- Make sure you're working form the ATP library
#
cd /progs/atp_${ATP_VERSION}
#
# -- Get all the log files that are currently on cddis but not available locally
#
set logs_run    = "/progs/atp_${ATP_VERSION}/share/run_gen_bnc_exe_logs.txt"
python3 /opt64/bin/get_logs.py >> ${logs_run};
#
# -- Write the available list of vlbi log files to file
#
set write_logs = "/opt64/bin/write_log_paths_to_file.py"
python3 ${write_logs} vlbi 4.0 >> ${logs_run};
#
# -- Generate the binary files
#
set num_proc       = 8
set vlbi_log_paths = /progs/atp_${ATP_VERSION}/share/vlbi_log_file_paths.txt
set l2b_exe        = /opt64/bin/l2b_sub_exe.py
cat ${vlbi_log_paths} | \
    parallel -k -j $num_proc python3 ${l2b_exe} vlbi     \
                                                4.0      \
                                                {}       \
                                                $xpid;
#
# --- Generate plots that are not there yet [coming soon]
#
