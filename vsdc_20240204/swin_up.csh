#!/bin/csh
# ************************************************************************
# *                                                                      *
# *   Program swin_up.csh calls vsds for submitting Level 1a correlator  *
# *   output from the DiFX correlator.                                   *
# *                                                                      *
# *   It creates the tar file, compresses it, submits to CDDIS and       *
# *   deletes.                                                           *
# *                                                                      *
# *   Usage: swin_up.csh difx_dir vex v2d [--nosubmit] [-v VERB] [args]  *
# *   1st argument -- directory name with DiFX output                    *
# *   2nd argument -- modified vex file name.                            *
# *   3rd argument -- v2d file.                                          *
# *   args         -- additional aguments accepted by vsdc_swin_gentar.py*
# *                                                                      *
# *   Supported optional arguments:                                      *
# *   -nos --nosubmit   create the output tar file, but do not submit it.*
# *   -v --verb         verbosity level:                                 *
# *                     0 -- silent mode                                 *
# *                     1 -- normal vervosity                            *
# *                     2,3,4 -- verbose debugging mode.                 *
# *                                                                      *
# *  ### 01-MAR-2021  swin_up.csh  v1.8 (c)  L. Petrov  12-APR-2023 ###  *
# *                                                                      *
# ************************************************************************
#
#  Local customization is put here
#
set vsdc_root_dir=/progs/vsdc_20240204
set vsdc_install_dir=$0:h
set datacenter_cnf=/cont/vsdc/cddis.cnf 
#
# --- End of local customization
#
#
if ( $#argv < 3 ) then 
     echo "Usage: swin_up.csh l1a_exp_dir vex_file v2d_file [--nosubmit] [--verb verbosity]"
     exit ( 1 )
endif
set tar_dir   = `cat $datacenter_cnf | grep "^LARGE_TMP_DIR:" | awk '{print $2}'`
if ( "$tar_dir" == "" ) then
     echo "Error in $datacenter_cnf control file: LARGE_TMP_DIR was not defined"
     exit 1
endif
set gentar = ${vsdc_install_dir}/vsdc_swin_gentar.py 
#
# --- print vsdc version
#
${vsdc_install_dir}/vsdc.py --version
#
# --- Check whether the directory has a subdirectory with Fourfit generated files
# 
setenv VSDC_SWIN_EXCLUDE `${vsdc_root_dir}/vsdc_find_ffdir.py $1`
if ( "$VSDC_SWIN_EXCLUDE" == "" ) then
     echo "No files generated with Fourfit were detected in $1 -- it is good"
   else 
     echo "A subdirectory with Fourfit generated files was detected in $1"
     echo "Filter VSDC_SWIN_EXCLUDE = $VSDC_SWIN_EXCLUDE will be used"
endif
set input_dir = $1 ; shift
set vex_file  = $1 ; shift
set v2d_file  = $1 ; shift
#
set fl_submit = "-s -d"
set verb      = 1
set remaining_args = ""
while ( $# > 0 )
   switch($1:q)
     case -v:
     case --verb:
         shift 
         set verb = $1
         breaksw
     case -nos:
     case --nosubmit:
         set fl_submit=""
         breaksw
     default:
         set remaining_args = "$remaining_args $1"
         breaksw
    endsw
    shift 
end
#
$gentar -i $input_dir       \
        -o $tar_dir         \
        --vex $vex_file     \
        --v2d $v2d_file     \
        -c $datacenter_cnf  \
        --compressor lbzip2 \
        -v $verb            \
        $fl_submit          \
        $remaining_args
if ( $status == 0 ) then
     if ( "$fl_submit" == "" ) then
           echo "Tar file has been generated, but it will not be submitted because oiption -nosubmit was invoked"
        else
           echo "Tar file has been generated and successfully submitted"
     endif
   else
     echo "Tar file has been not generated due to an error"
endif
