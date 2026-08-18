#!/bin/bash -f
ulimit -s 2000000 > /dev/null 2>&1
# *
# *#!/bin/sh
# *
# *
# *
export PATH="$HOME/bin"
export PATH="${PATH}:/opt64/bin"
export PATH="${PATH}:${HOME}/tools"
export PATH="${PATH}:${HOME}/exec"
export PATH="${PATH}:/opt/bin"
export PATH="${PATH}:/sbin"
export PATH="${PATH}:/usr/bin"
export PATH="${PATH}:/usr/local/bin"
export PATH="${PATH}:/usr/local/sbin"
export PATH="${PATH}:/usr/X11R6/bin"
export PATH="${PATH}:/usr/sbin"
#
export LD_LIBRARY_PATH=/opt64/lib:/opt64/lib64:/usr/lib:/usr/lib64
umask 0002
#
#  See /cont/celestrak_url_list.txt
#
python3 /auto/tle_download.py >> /tle/tle_download_runtimes.txt
