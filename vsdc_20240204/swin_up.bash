#!/bin/bash
# ************************************************************************
# *                                                                      *
# *   Program swin_up.csh calls vsds for submitting Level 1a correlator  *
# *   output from the DiFX correlator.                                   *
# *                                                                      *
# *   It creates the tar file, compresses it, submits to CDDIS and       *
# *   deletes.                                                           *
# *                                                                      *
# *   Usage: swin_up.csh difx_dir vex v2d [args]                         *
# *   1st argument -- directory name with DiFX output                    *
# *   2nd argument -- modified vex file name.                            *
# *   3rd argument -- v2d file.                                          *
# *   args         -- additional agument accepted by vsdc_swin_gentar.py *                                                                   *
# *                                                                      *
# *   Supported optional arguments:                                      *
# *   -nos --nosubmit   create the output tar file, but do not submit it.*
# *   -v --verb         verbosity level:                                 *
# *                     0 -- silent mode                                 *
# *                     1 -- normal vervosity                            *
# *                     2,3,4 -- verbose debugging mode.                 *
# *                                                                      *
# *  ### 20-APR-2021  swin_up.bash  v1.8 (c)  L. Petrov  12-APR-2023 ### *
# *                                                                      *
# ************************************************************************
#
#  Local customization is put here
#
export vsdc_dir=/progs/vsdc_20240204
export vsdc_root_dir=/progs/vsdc_20240204
export vsdc_install_dir=`dirname $0`
export datacenter_cnf=/cont/vsdc/cddis.cnf 
verb=1
#
# --- End of local customization
#
tar_dir=`cat $datacenter_cnf | grep "^LARGE_TMP_DIR:" | awk '{print $2}'`
if [ "$tar_dir" == "" ]; then
     echo "Error in $datacenter_cnf control file: LARGE_TMP_DIR was not defined"
     exit 1
fi
if [ $# -lt 3 ]; then 
     echo "Usage: swin_up.bash l1a_exp_dir vex_file v2d_file [-nosubmit] [-verb verbosity]"
     exit 1
fi
gentar=${vsdc_install_dir}/vsdc_swin_gentar.py 
#
# --- print vsdc version
#
${vsdc_install_dir}/vsdc.py --version
#
export VSDC_SWIN_EXCLUDE=`${vsdc_root_dir}/vsdc_find_ffdir.py $1`
if [ "$VSDC_SWIN_EXCLUDE" == "" ]; then
     echo "No files generated with Fourfit were detected in $1"
   else 
     echo "A subdirectory with Fourfit generated files was detected in $1"
     echo "Filter VSDC_SWIN_EXCLUDE = $VSDC_SWIN_EXCLUDE will be used"
fi
#
echo "VSDC_SWIN_EXCLUDE= $VSDC_SWIN_EXCLUDE"
#
fl_submit="-s -d"
input_dir=$1; shift
vex_file=$1; shift
v2d_file=$1; shift
remaining_args=""
fl_submit="-s -d"
while [[ $# -gt 0 ]]; do
  key="$1"
  case $key in
    -v|--verbosity)
      verb="$2"
      shift # past argument
      shift # past value
      ;;
    -nos|--nosubmit)
      fl_submit=""
      shift # past argument
      ;;
    *)    # unknown option
      new_arg=("$1")
      remaining_args="$remaining_args $new_arg"
      shift # past argument
      ;;
  esac
done
#
if [ $verb>3 ]; then
     echo "Debugging output of vsdc_find_ffdir.py"
     ${vsdc_root_dir}/vsdc_find_ffdir.py $input_dir 1
     echo "End of Debugging output of vsdc_find_ffdir.py"
fi
#
$gentar -i $input_dir       \
        -o $tar_dir         \
        --vex $vex_file     \
        --v2d $v2d_file     \
        -c $datacenter_cnf  \
        --compressor lbzip2 \
        $fl_submit          \
        -v $verb            \
        $remaining_args
status=$?
if [ $status == 0 ]; then
     if [ "$fl_submit" == "" ]; then
          echo "Tar file has been generated, but it will not be submitted because option -nosubmit was invoked"
     else
          echo "Tar file has been generated and successfully submitted"
     fi
  else
     echo "Tar file has been not generated due to an error"
fi
