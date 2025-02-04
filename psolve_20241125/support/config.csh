#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Script  config.csh  sets numerous variables needed for             *
# *   compilation and linking SOLVE. It creates files with directives    *
# *   for FORTRAN-compilers and C-compiler. It creates include files     *
# *   using template files for includes and local files with             *
# *   preferences.                                                       *
# *                                                                      *
# *   Usage:  config.csh                                                 *
# *                                                                      *
# *  ### 17-OCT-2017 config.csh   v1.11 (c)  L. Petrov  22-DEC-2023 ###  *
# *                                                                      *
# ************************************************************************
#
setenv LC_ALL C
setenv LANG   C
set config_revision_date = "2023.12.22"
#
setenv SUPPORT_PATH `dirname $0`
set cfitsio_min_version = 3.0
#
cd     $SUPPORT_PATH
cd ../
if (    -f ${SOLVE_ROOT}/Makefile ) then
     rm -f ${SOLVE_ROOT}/Makefile
endif
set OS_name = `uname`
switch ( $OS_name )
   case "HP-UX":
     set ECHO = "/bin/echo"
     set qb = "\0044\0050"
     set qe = "\0051"
     set qt = "\0042"
     breaksw
   case "SunOS":
     set ECHO = "/bin/echo"
     set qb = "\044\050"
     set qe = "\051"
     set qt = "\042"
     breaksw
   case "Linux":
     set ECHO = "/bin/echo -e"
     set qb = "\044\050"
     set qe = "\051"
     set qt = "\042"
     breaksw
   case "Darwin":
     set ECHO = "/bin/echo"
     set qt = '"'
     breaksw
endsw
#
set    date_stamp  = `date "+%Y.%m.%d-%H:%M:%S" | tr "[a-z]" "[A-Z]"`
set    host_name   = `uname -n`
echo  "=================================="                   | tee -a $CONF_LOG
echo  "psolve config  $date_stamp  on  $host_name"           | tee -a $CONF_LOG
echo  "psolve config  revision date: " $config_revision_date >>!      $CONF_LOG
#
# --- Check petools library
#
$ECHO "config.csh: check petools library ... \c" | tee -a  $CONF_LOG
if ( -d $SOLVE_PETOOLS_DIR == 0 ) then
     echo "psolve config: petools directory $SOLVE_PETOOLS_DIR was not found"
     exit 1
endif
if ( -d $SOLVE_PETOOLS_DIR/lib == 0 ) then
     echo "psolve config: petools directory $SOLVE_PETOOLS_DIR/lib was not found"
     exit 1
endif
if ( -d $SOLVE_PETOOLS_DIR/bin == 0 ) then
     echo "psolve config: petools directory $SOLVE_PETOOLS_DIR/bin was not found"
     exit 1
endif
if ( -f $SOLVE_PETOOLS_DIR/lib/libpetools.a == 0 ) then
     echo "psolve config: petools file $SOLVE_PETOOLS_DIR/lib/libpetools.a was not found"
     exit 1
endif
source $SOLVE_PETOOLS_DIR/bin/petools_vars
echo "ok"
#
# --- Check cfitsio library
#
if ( $SOLVE_CFITSIO_DIR != "" ) then
     $ECHO "config.csh: check cfitsio library files ... \c" | tee -a  $CONF_LOG
     if ( -d $SOLVE_CFITSIO_DIR == 0 ) then
          echo "failed"
          echo "psolve config: cfitsio directory $SOLVE_CFITSIO_DIR was not found"
          exit 1
     endif
     if ( -f $SOLVE_CFITSIO_DIR/lib/libcfitsio.a == 0 ) then
          echo "failed"
          echo "psolve config: $SOLVE_CFITSIO_DIR/lib/libcfitsio.a library was not found"
          exit 1
     endif
     if ( -f $SOLVE_CFITSIO_DIR/include/fitsio.h == 0 ) then
          echo "failed"
          echo "psolve config: $SOLVE_CFITSIO_DIR/include/fitsio.h include file was not found"
          exit 1
     endif
     setenv SOLVE_OPT_NOSTRUC ""
     setenv SOLVE_CFITSIO_LIB "-L $SOLVE_CFITSIO_DIR/lib -lcfitsio"
     setenv CFITSIO_LIB       "-L $SOLVE_CFITSIO_DIR/lib -lcfitsio"
     setenv SOLVE_FITSLIB_LIB "-L $SOLVE_VTD_DIR/lib -lfitslib"
     setenv CFITSIO_INC "$SOLVE_CFITSIO_DIR/include"
     echo "ok" 
#
     $ECHO "config.csh: Check cfitsio library functionality ... \c" | tee -a  $CONF_LOG
     $SOLVE_ROOT/support/check_cfitsio_version.csh >>& $CONF_LOG
     if ( $status != 0 ) then
          echo "Failed to link against cfitsio library" | tee -a  $CONF_LOG
          exit 1
     endif
     echo "ok" 
#
     $ECHO "config.csh: Check cfitsio version >= $cfitsio_min_version ... \c" | tee -a  $CONF_LOG
     set cfitsio_version = `$SOLVE_ROOT/bin/check_cfitsio_version.e | awk '{print $3}'`
     if ( `$SOLVE_ROOT/support/version_equal_or_greater.csh $cfitsio_version $cfitsio_min_version` == 0    ) then
           echo "But found $cfitsio_version"
           echo "Please upgrade cfitsio to version $cfitsio_min_version or newer"
          exit 1
     endif
     make -f $SOLVE_ROOT/support/check_cfitsio_version.mak clean 
     $ECHO "$cfitsio_version  \c" | tee -a  $CONF_LOG
     echo "ok" 
  else 
     echo "psolve config: cfitsio directory $SOLVE_CFITSIO_DIR was not specified"
     exit 1
endif
#
# --- Check spd_client library
#
$ECHO "config.csh: check spd_client library ... \c" | tee -a  $CONF_LOG
if ( -f $SOLVE_SPD_CLIENT_DIR/lib/libspc.a == 0 ) then
     echo "psolve config: spd_client file $SOLVE_SPD_CLIENT_DIR/lib/libspc.a  was not found"
     echo "psolve config: Please install spd_client"
     exit 1
endif
if ( -f $SOLVE_SPD_CLIENT_DIR/include/spd.i == 0 ) then
     echo "psolve config: spd_client file $SOLVE_SPD_CLIENT_DIR/include/spd.i was not found"
     echo "psolve config: Please install spd_client"
     exit 1
endif
if ( -f $SOLVE_SPD_CLIENT_DIR/include/spd_local.i == 0 ) then
     echo "psolve config: spd_client file $SOLVE_SPD_CLIENT_DIR/include/spd_local.i was not found"
     echo "psolve config: Please install spd_client"
     exit 1
endif
if ( `cat $SOLVE_SPD_CLIENT_DIR/include/spd_local.i | grep "SPD__PETOOLS = " | grep yes` == "" && \
     `cat $SOLVE_SPD_CLIENT_DIR/include/spd_local.i | grep "SPC__PETOOLS = " | grep yes` == "" && ) then
     echo "psolve config: spd_client was installed without petools"
     echo "psolve config: Please re-install spd_client with petools"
     exit 1
endif
set SOLVE_SPD_CLIENT_VERSION = `cat $SOLVE_SPD_CLIENT_DIR/include/spd_local.i | grep "SPD__VERSION =" | awk '{print substr($5,2,8)}'`
if ( "$SOLVE_SPD_CLIENT_VERSION" < "$SPD_CLIENT_VERSION_MIN" ) then
     echo "Eeeeh. You have spd_client version $SOLVE_SPD_CLIENT_VERSION, but $SPD_CLIENT_VERSION_MIN is needed."
     echo "Please upgrade."
     echo "You can download the new version of spd_client from http://astrogeo.org/spd"
     exit 1
endif
echo "ok" 
#
# --- Check ners library
#
$ECHO "config.csh: check ners library ... \c" | tee -a  $CONF_LOG
if ( -d  $SOLVE_NERS_DIR == 0 ) then
     echo "psolve config: ners install directory $SOLVE_NERS_DIR was not found"
     echo "psolve config: Please install ners"
     exit 1
endif
if ( -d  $SOLVE_NERS_DIR/include == 0 ) then
     echo "psolve config: ners include directory $SOLVE_NERS_DIR/include was not found"
     echo "psolve config: Please install ners"
     exit 1
endif
#
if ( -f  $SOLVE_NERS_DIR/include/ners_local.i == 0 ) then
     echo "psolve config: ners version file $SOLVE_NERS_DIR/include/ners_local.i was not found"
     echo "psolve config: Please install ners"
     exit 1
endif
set SOLVE_NERS_VERSION = `cat $SOLVE_NERS_DIR/include/ners_local.i | grep "NERS__VERSION =" | awk '{print substr($5,2,8)}'`
if ( "$SOLVE_NERS_VERSION" < "$NERS_VERSION_MIN" ) then
     echo "Eeeeh. You have ners version $SOLVE_NERS_VERSION, but $NERS_VERSION_MIN is needed."
     echo "Please upgrade."
     echo "You can download the new version of ners from http://earthrotation.net/ners"
     exit 1
endif
set SOLVE_NERS_STANDALONE = `cat $SOLVE_NERS_DIR/include/ners_local.i | grep "NERS__NERS_STANDALONE = " | awk '{print substr($5,2,8)}' | sed 's@"@@g'`
if ( "$SOLVE_NERS_STANDALONE" == "YES" ) then
     echo "psolve config: ners package was configured without petools"
     echo "psolve config: Please re-configure and re-install ners with petools support"
     exit 1
endif
echo "ok"
#
# Check GVH library
# 
$ECHO "config.csh: check gvh library ... \c" | tee -a  $CONF_LOG
if ( -d  $SOLVE_GVH_DIR == 0 ) then
     echo "psolve config: gvh install directory $SOLVE_GVH_DIR was not found"
     echo "psolve config: Please install gvh"
     exit 1
endif
#
if ( -f  $SOLVE_GVH_DIR/include/gvh.i == 0 ) then
     echo "psolve config: gvh include directory $SOLVE_GVH_DIR/include was not found"
     echo "psolve config: Please install gvh"
     exit 1
endif
#
if (  `uname` == "Linux" && -f  $SOLVE_GVH_DIR/lib/libgvh.so == 0 ) then
     echo "psolve config: gvh library file $SOLVE_GVH_DIR/lib/libgvh.so was not found"
     echo "psolve config: Please install gvh"
     exit 1
 else if ( `uname` == "Darwin" ) then
     if ( -f  $SOLVE_GVH_DIR/lib/libgvh.1.dylib == 0 ) then
          echo "psolve config: gvh library file $SOLVE_GVH_DIR/lib/libgvh.1.dylib was not found"
          echo "psolve config: Please install gvh"
          exit 1
     endif
endif
set gvh_version = `cat $SOLVE_GVH_DIR/bin/gvh_vars | grep GVH_VERSION | awk '{print $3}'`
if ( "$gvh_version" < "$GVH_VERSION_MIN" ) then
     echo "failed"
     echo "Eeeeh. You have gvh version $gvh_version but $GVH_VERSION_MIN is needed."
     echo "Please upgrade."
     echo "You can download the new version of ners from http://astrogeo.org/gvh"
     exit 1
endif
echo "ok"
#
# --- Check VTD library
#
$ECHO "config.csh: check vtd library ... \c" | tee -a  $CONF_LOG
if ( -d  $SOLVE_VTD_DIR == 0 ) then
     echo "psolve config: ners install directory $SOLVE_VTD_DIR was not found"
     echo "psolve config: Please install ners"
     exit 1
endif
#
if ( -d  $SOLVE_VTD_DIR/include == 0 ) then
     echo "psolve config: ners include directory $SOLVE_VTD_DIR/include was not found"
     echo "psolve config: Please install ners"
     exit 1
endif
#
if ( -f  $SOLVE_VTD_DIR/include/vtd.i == 0 ) then
     echo "psolve config: version file $SOLVE_VTD_DIR/include/vtd.i was not found"
     echo "psolve config: Please install ners"
     exit 1
endif
set SOLVE_VTD_VERSION = `cat $SOLVE_VTD_DIR/include/vtd.i | grep "VTD__LABEL =" | awk '{print $6}'`
if ( "$SOLVE_VTD_VERSION" < "$VTD_VERSION_MIN" ) then
     echo "failed"
     echo "Eeeeh. You have vtd version $SOLVE_VTD_VERSION, but $VTD_VERSION_MIN is needed."
     echo "Please upgrade."
     echo "You can download the new version of vtd from http://astrogeo.org/vtd"
     exit 1
endif
echo "ok"
#
# --- Check vex_parser library
#
$ECHO "config.csh: check vex_parser library ... \c" | tee -a  $CONF_LOG
if ( -d  $SOLVE_VEX_PARSER_DIR == 0 ) then
     echo "psolve config: vex_parser install directory $SOLVE_VEX_PARSER_DIR was not found"
     echo "psolve config: Please install vex_parser"
     exit 1
endif
#
if ( -d  $SOLVE_VEX_PARSER_DIR/include == 0 ) then
     echo "psolve config: vex_parser include directory $SOLVE_VEX_PARSER_DIR/include was not found"
     echo "psolve config: Please install vex_parser"
     exit 1
endif
#
if ( -f  $SOLVE_VEX_PARSER_DIR/include/vex.i == 0 ) then
     echo "psolve config: version file $SOLVE_VEX_PARSER_DIR/include/vex.i was not found"
     echo "psolve config: Please install vex_parser"
     exit 1
endif
set SOLVE_VEX_PARSER_VERSION = `cat $SOLVE_VEX_PARSER_DIR/include/vex.i | grep "VEX_PARSER__LABEL =" | awk '{print substr($8,0,11)}' | sed "s@'@@g" | sed 's@\.@@g'`
if ( "$SOLVE_VEX_PARSER_VERSION" < "$VEX_PARSER_VERSION_MIN" ) then
     echo "failed"
     echo "Eeeeh. You have vex_parser version $SOLVE_VEX_PARSER_VERSION, but $VEX_PARSER_VERSION_MIN is needed."
     echo "Please upgrade."
     echo "You can download the new version of vtd from http://astrogeo.org/vex_parser"
     exit 1
endif
echo "ok"
#
if ( $SOLVE_SCRATCH_DATA == "undefined" ) then
     echo "psolve config: option --scratch_data was not specified"
     echo "psolve config: Please speficy Solve scratch directory with --scratch_data"
     exit 1
endif
if ( -d $SOLVE_SCRATCH_DATA == 0 ) then
     mkdir $SOLVE_SCRATCH_DATA 
     set status = $status
     if ( $status != 0 ) then
          echo "psolve config: failure to create Solve scratch directory $SOLVE_SCRATCH_DATA"
          echo "psolve config: Please check directory name specified with option --scratch_data"
          exit 1
     endif
endif
#
if ( $SOLVE_SAVE_DATA == "undefined" ) then
     echo "psolve config: option --save_data was not specified"
     echo "psolve config: Please speficy Solve save directory with --save_data"
     exit 1
endif
if ( -d $SOLVE_SAVE_DATA == 0 ) then
     mkdir $SOLVE_SAVE_DATA 
     set status = $status
     if ( $status != 0 ) then
          echo "psolve config: failure to create Solve save directory $SOLVE_SAVE_DATA"
          echo "psolve config: Please check directory name specified with option --save_data"
          exit 1
     endif
endif
#
if ( $SOLVE_GVF_DATA == "undefined" ) then
     echo "psolve config: option --gvf_data was not specified"
     echo "psolve config: Please speficy Solve scratch directory with --gvf_data"
     exit 1
endif
if ( -d $SOLVE_GVF_DATA == 0 ) then
     mkdir $SOLVE_GVF_DATA 
     set status = $status
     if ( $status != 0 ) then
          echo "psolve config: failure to create directory with VLBI databases in GVF format $SOLVE_GVF_DATA"
          echo "psolve config: Please check directory name specified with option --gvf_data"
          exit 1
     endif
endif
if ( $SOLVE_CENTER_ABR == "undentified" ) then
     echo "psolve config: option --center_abr was not specified"
     echo "psolve config: Please speficy your center 3 letter abbreviation"
     exit 1
endif
if ( "$SOLVE_CENTER_NAME" == "undentified" ) then
     echo "psolve config: option --center_name was not specified"
     echo "psolve config: Please speficy your center name"
     exit 1
endif
if ( $SOLVE_PS_VIEWER == "undentified" ) then
     echo "psolve config: option --ps_viewer was not specified"
     echo "psolve config: Please speficy the full path executable for PostScript viewer"
     exit 1
endif
#if ( -e $SOLVE_PS_VIEWER == 0 ) then
#     echo "psolve config: PostScript viewer $SOLVE_PS_VIEWER does not exist or not executable"
#     echo "psolve config: Please speficy the full path executable for PostScript viewer with --ps_viewer option"
#     exit 1
#endif
if ( $SOLVE_GIF_VIEWER == "undentified" ) then
     echo "psolve config: option --gif_viewer was not specified"
     echo "psolve config: Please speficy the full path executable for GIF viewer"
     exit 1
endif
if ( -e $SOLVE_GIF_VIEWER == 0 ) then
     echo "psolve config: GIF viewer $SOLVE_GIF_VIEWER does not exist or not executable"
     echo "psolve config: Please speficy the full path executable for gif viewer with --gif_viewer option"
     exit 1
endif
#
# --- Check whether gmake is found in your system
#
set gmake_string = `which gmake`
if ( "$gmake_string" == "" ) then 
     echo "gmake was not found in your system."
     exit 1
endif
#
set    date_stamp  = `date "+%Y.%m.%d-%H:%M:%S" | tr "[a-z]" "[A-Z]"`
set    host_name   = `uname -n`
#
set out_file = ${SOLVE_ROOT}/Makefile
source ${SOLVE_PETOOLS_DIR}/bin/petools_vars
#
if ( "$PETOOLS_VERSION" < "$PETOOLS_VERSION_MIN" ) then
     echo "Eeeeh. You have petools version $PETOOLS_VERSION, but "
     echo "$PETOOLS_VERSION_MIN is needed. Please upgrade"
     echo "You can download petools from http://astrogeo.org/petools"
     exit 1
endif
#
# ---  Remove path to include file in the installation directory
# ---  in order to prevent possible conflicts between the current
# ---  include files and include files from previos installation
#
setenv MK5_F95        `echo $MK5_F95        | sed "s@-I ../include@-I $SOLVE_ROOT/include -I $SOLVE_PETOOLS_DIR/include@g" | sed "s@-fallow-argument-mismatch@-fallow-argument-mismatch -Wno-align-commons@g"`
setenv MK5_F95_OPT    `echo $MK5_F95_OPT    | sed "s@-I ../include@-I $SOLVE_ROOT/include -I $SOLVE_PETOOLS_DIR/include@g" | sed "s@-fallow-argument-mismatch@-fallow-argument-mismatch -Wno-align-commons@g"`
setenv MK5_F95_OPTEST `echo $MK5_F95_OPTEST | sed "s@-I ../include@-I $SOLVE_ROOT/include -I $SOLVE_PETOOLS_DIR/include@g" | sed "s@-fallow-argument-mismatch@-fallow-argument-mismatch -Wno-align-commons@g"`
setenv MK5_F95_NOOPT  `echo $MK5_F95_NOOPT  | sed "s@-I ../include@-I $SOLVE_ROOT/include -I $SOLVE_PETOOLS_DIR/include@g" | sed "s@-fallow-argument-mismatch@-fallow-argument-mismatch -Wno-align-commons@g"`
#
$ECHO "config.csh: generate vcat.conf file ... \c" | tee -a  $CONF_LOG
support/solve_apr_vcat_update.csh
if ( $status == 0 ) then
     echo "ok" 
     if ( -f ${SOLVE_SAVE_DATA}/vcat.conf ) then
          echo "config.csh: upgrade vcat.conf file if needed ... " | tee -a  $CONF_LOG
          support/upgrade_vcat_conf.csh ${SOLVE_SAVE_DATA}/vcat.conf
          if ( $status == 0 ) then
               echo "vcat.conf is upgraded" 
             else
               $ECHO "Failure to upgrade $SOLVE_ROOT/support/vcat.conf"    | tee -a  $CONF_LOG
               $ECHO "You will need create it manually after installation" | tee -a  $CONF_LOG
          endif
     endif
   else
     $ECHO "No functional vcat.conf file has been generated"     | tee -a  $CONF_LOG
     $ECHO "You will need create it manually after installation" | tee -a  $CONF_LOG
endif
#
# --- Preprocess 4 include files
#
$ECHO "config.csh: generate include files ... \c" | tee -a  $CONF_LOG
set DATE_ISO   = `date "+%Y.%m.%d_%H:%M:%S"`
set solve_version_len     = `echo $SOLVE_VERSION                  | awk '{print length($1)}'`
set psolve_root_len       = `echo $SOLVE_ROOT                     | awk '{print length($1)}'`
set psolve_dir_len        = `echo $SOLVE_PREFIX                   | awk '{print length($1)}'`
set solve_spool_dir_len   = `echo $SOLVE_SCRATCH_DATA/spool_dir   | awk '{print length($1)}'`
set solve_work_dir_len    = `echo $SOLVE_SCRATCH_DATA/work_dir    | awk '{print length($1)}'`
set solve_cgm_dir_len     = `echo $SOLVE_SCRATCH_DATA/cgm_dir     | awk '{print length($1)}'`
set solve_prog_dir_len    = `echo $SOLVE_PREFIX/psolve/bin/       | awk '{print length($1)}'`
set solve_save_dir_len    = `echo $SOLVE_SAVE_DATA                | awk '{print length($1)}'`
set solve_help_dir_len    = `echo $SOLVE_PREFIX/psolve/doc        | awk '{print length($1)}'`
set solve_pima_dir_len    = `echo $SOLVE_SCRATCH_PIMA             | awk '{print length($1)}'`
set solve_center_abr_len  = `echo $SOLVE_CENTER_ABR               | awk '{print length($1)}'`
set solve_center_name_len = `echo $SOLVE_CENTER_NAME              | awk '{print length($0)}'`
set solve_ps_viewer_len   = `echo $SOLVE_PS_VIEWER                | awk '{print length($1)}'`
set solve_gif_viewer_len  = `echo $SOLVE_GIF_VIEWER               | awk '{print length($1)}'`
set solve_stp_dir_len     = `echo $SOLVE_STP_DIR                  | awk '{print length($1)}'`
set solve_gvf_dir_len      = `echo $SOLVE_GVF_DATA                | awk '{print length($1)}'`
cat $SOLVE_ROOT/include/solve_i.templ | \
    sed "s@%%@Automatically generated on $DATE_ISO from psolve_templ.i@g" | \
    sed "s@%SOLVE_VERSION_LEN%@$solve_version_len@g"                      | \
    sed "s@%SOLVE_VERSION_VAL%@$SOLVE_VERSION@g"                          | \
    sed "s@%PSOLVE_DIR_LEN%@$psolve_dir_len@g"                            | \
    sed "s@%PSOLVE_DIR_VAL%@$SOLVE_PREFIX@g"                              | \
    sed "s@%PSOLVE_ROOT_LEN%@$psolve_root_len@g"                          | \
    sed "s@%PSOLVE_ROOT_VAL%@$SOLVE_ROOT@g"                               | \
    sed "s@%SPOOL_DIR_LEN%@$solve_spool_dir_len@g"                        | \
    sed "s@%SPOOL_DIR_VAL%@$SOLVE_SCRATCH_DATA/spool_dir@g"               | \
    sed "s@%CGM_DIR_LEN%@$solve_cgm_dir_len@g"                            | \
    sed "s@%CGM_DIR_VAL%@$SOLVE_SCRATCH_DATA/cgm_dir@g"                   | \
    sed "s@%SOLVE_PROG_DIR_LEN%@$solve_prog_dir_len@g"                    | \
    sed "s@%SOLVE_PROG_DIR_VAL%@$SOLVE_PREFIX/psolve/bin/@g"              | \
    sed "s@%SOLVE_WORK_DIR_LEN%@$solve_work_dir_len@g"                    | \
    sed "s@%SOLVE_WORK_DIR_VAL%@$SOLVE_SCRATCH_DATA/work_dir@g"           | \
    sed "s@%SOLVE_SAVE_DIR_LEN%@$solve_save_dir_len@g"                    | \
    sed "s@%SOLVE_SAVE_DIR_VAL%@$SOLVE_SAVE_DATA@g"                       | \
    sed "s@%SOLVE_HELP_DIR_LEN%@$solve_help_dir_len@g"                    | \
    sed "s@%SOLVE_HELP_DIR_VAL%@$SOLVE_PREFIX/psolve/doc@g"               | \
    sed "s@%SOLVE_PIMA_DIR_LEN%@$solve_pima_dir_len@g"                    | \
    sed "s@%SOLVE_PIMA_DIR_VAL%@$SOLVE_SCRATCH_PIMA@g"                    | \
    sed "s@%CENTER_ABR_LEN%@$solve_center_abr_len@g"                      | \
    sed "s@%CENTER_ABR_VAL%@$SOLVE_CENTER_ABR@g"                          | \
    sed "s@%CENTER_FULL_NAME_LEN%@$solve_center_name_len@g"               | \
    sed "s@%CENTER_FULL_NAME_VAL%@$SOLVE_CENTER_NAME@g"                   | \
    sed "s@%SOLVE_PS_VIEWER_LEN%@$solve_ps_viewer_len@g"                  | \
    sed "s@%SOLVE_PS_VIEWER_VAL%@$SOLVE_PS_VIEWER@g"                      | \
    sed "s@%SOLVE_GIF_VIEWER_LEN%@$solve_gif_viewer_len@g"                | \
    sed "s@%SOLVE_GIF_VIEWER_VAL%@$SOLVE_GIF_VIEWER@g"                    | \
    sed "s@%SOLVE_STP_DIR_LEN%@$solve_stp_dir_len@g"                      | \
    sed "s@%SOLVE_STP_DIR_VAL%@$SOLVE_STP_DIR@g"                          | \
    sed "s@%SOLVE_GVF_DIR_LEN%@$solve_gvf_dir_len@g"                      | \
    sed "s@%SOLVE_GVF_DIR_VAL%@$SOLVE_GVF_DATA@g"                         | \
    sed "s@ADDRESS__TYPE@INTEGER*8@g"                                       \
    > $SOLVE_ROOT/include/solve.i 
#
cat $SOLVE_ROOT/include/glbp_i.templ                                      | \
    sed "s@%%@Automatically generated on $DATE_ISO from glbi_i.templ@g"   | \
    sed "s@ADDRESS__TYPE@INTEGER*8@g"                                       \
    > $SOLVE_ROOT/include/glbp.i
#
cat $SOLVE_ROOT/include/glbc4_i.templ                                     | \
    sed "s@%%@Automatically generated on $DATE_ISO from glbc4_i.templ@g"  | \
    sed "s@ADDRESS__TYPE@INTEGER*8@g"                                       \
    > $SOLVE_ROOT/include/glbc4.i
#
cat $SOLVE_ROOT/include/equmem_i.templ                                    | \
    sed "s@%%@Automatically generated on $DATE_ISO from equmem_i.templ@g" | \
    sed "s@ADDRESS__TYPE@INTEGER*8@g"                                       \
    > $SOLVE_ROOT/include/equmem.i
#
cat $SOLVE_ROOT/include/fast_i.templ                                      | \
    sed "s@%%@Automatically generated on $DATE_ISO from fast_i.templ@g"   | \
    sed "s@ADDRESS__TYPE@INTEGER*8@g"                                       \
    > $SOLVE_ROOT/include/fast.i
#
cat $SOLVE_ROOT/support/psolve_templ | \
    sed "s@%%@Automatically generated on $DATE_ISO from support/psolve_templ@g" | \
    sed "s@%SOLVE_PROG_DIR_VAL%@$SOLVE_PREFIX/psolve/bin@g"                     | \
    sed "s@%SOLVE_VERSION%@$SOLVE_VERSION@g"                                      \
    > $SOLVE_ROOT/temp/psolve
chmod u+rwx,g+rwx,o+rx  $SOLVE_ROOT/temp/psolve         
#
echo "ok" 
#
if ( `uname` == "Linux" ) then
     set num_threads     = `cat /proc/cpuinfo | grep "core id" | wc -l`
     set num_thr_per_cpu = `lscpu | grep '^Thread(s) per core:' | awk '{print $4}'`
     set num_cores = `expr $num_threads / $num_thr_per_cpu`
else
     set num_cores = `sysctl -n  machdep.cpu.core_count`
endif
cat   ${SOLVE_ROOT}/Makefile.in1 > $out_file 
$ECHO "PSOLVE_ROOT        =      $SOLVE_ROOT"               >> $out_file
$ECHO "PETOOLS_ROOT       =      $PETOOLS_ROOT"             >> $out_file
$ECHO "PETOOLS_PREFIX     =      $PETOOLS_PREFIX"           >> $out_file
$ECHO "SPD_CLIENT_PREFIX  =      $SOLVE_SPD_CLIENT_DIR"     >> $out_file
$ECHO "SPD_CLIENT_LIB     =      $SOLVE_SPD_CLIENT_DIR/lib" >> $out_file
$ECHO "NERS_LIB           =      $SOLVE_NERS_DIR/lib"       >> $out_file
$ECHO "NERS_INC           =      $SOLVE_NERS_DIR/include"   >> $out_file
$ECHO "GVH_LIB            =      $SOLVE_GVH_DIR/lib"        >> $out_file
$ECHO "GVH_INC            =      $SOLVE_GVH_DIR/include"    >> $out_file
$ECHO "VTD_LIB            =      $SOLVE_VTD_DIR/lib"        >> $out_file
$ECHO "VTD_INC            =      $SOLVE_VTD_DIR/include"    >> $out_file
$ECHO "VEX_LIB            =      $SOLVE_VEX_PARSER_DIR/lib"     >> $out_file
$ECHO "VEX_INC            =      $SOLVE_VEX_PARSER_DIR/include" >> $out_file
$ECHO "SOLVE_LIB_GVH      = ${qt}-L$SOLVE_GVH_DIR/lib -lgvh -lvcat${qt}"  >> $out_file
$ECHO "SOLVE_LIB_VTD      = ${qt}-L$SOLVE_VTD_DIR/lib -lvtd -L$SOLVE_SPD_CLIENT_DIR/lib -lspc -L$SOLVE_NERS_DIR/lib -lners${qt}"  >> $out_file
$ECHO "SOLVE_LIB_PETOOLS  = ${qt}$PETOOLS_LIB${qt}"         >> $out_file
$ECHO "SOLVE_LIB_PGPLOT   = ${qt}$SOLVE_LIB_PGPLOT${qt}"    >> $out_file
$ECHO "SOLVE_LIB_X11      = ${qt}$SOLVE_LIB_X11${qt}"       >> $out_file
$ECHO "SOLVE_LIB_XT       = ${qt}$SOLVE_LIB_XT${qt}"        >> $out_file
$ECHO "SOLVE_LIB_X11      = ${qt}$SOLVE_LIB_X11${qt}"       >> $out_file
$ECHO "SOLVE_LIB_XHP11    = ${qt}$SOLVE_LIB_XHP11${qt}"     >> $out_file
$ECHO "SOLVE_EXTRA_LIB    = ${qt}$SOLVE_EXTRA_LIB${qt}"     >> $out_file
$ECHO "SOLVE_LIB_VEC      = ${qt}$SOLVE_LIB_VEC${qt}"       >> $out_file
$ECHO "SOLVE_LIB_BLAS     = ${qt}$SOLVE_LIB_BLAS${qt}"      >> $out_file
$ECHO "MK5_C              = ${qt}$MK5_C${qt}"               >> $out_file
$ECHO "MK5_X11_INCLUDE    = ${qt}$MK5_X11_INCLUDE${qt}"     >> $out_file
if ( $SOLVE_NOOPT == "YES" ) then
     $ECHO "MK5_F95         = ${qt}$MK5_F95_NOOPT${qt} -DDEBUG"  >> $out_file
     $ECHO "MK5_F95_OPT     = ${qt}$MK5_F95_NOOPT${qt} -DDEBUG"  >> $out_file
     $ECHO "MK5_F95_OPTEST  = ${qt}$MK5_F95_NOOPT${qt} -DDEBUG"  >> $out_file
     $ECHO "MK5_F95_NOOPT   = ${qt}$MK5_F95_NOOPT${qt} -DDEBUG"  >> $out_file
  else
     $ECHO "MK5_F95            = ${qt}$MK5_F95${qt}"        >> $out_file
     $ECHO "MK5_F95_OPT        = ${qt}$MK5_F95_OPT${qt}"    >> $out_file
     $ECHO "MK5_F95_OPTEST     = ${qt}$MK5_F95_OPTEST${qt}" >> $out_file
     $ECHO "MK5_F95_NOOPT      = ${qt}$MK5_F95_NOOPT${qt}"  >> $out_file
endif
$ECHO "MK5_LINK           = ${qt}$MK5_LINK${qt}"            >> $out_file
$ECHO "MK5_C_LINK         = ${qt}$MK5_C_LINK${qt}"          >> $out_file
$ECHO "SOLVE_ROOT         =      $SOLVE_ROOT"               >> $out_file
$ECHO "SOLVE_PREFIX       =      $SOLVE_PREFIX"             >> $out_file
$ECHO "SOLVE_BIN          =      $SOLVE_PREFIX/bin"         >> $out_file
$ECHO "SOLVE_INC          =      $SOLVE_PREFIX/include"     >> $out_file
$ECHO "SOLVE_LIB          =      $SOLVE_PREFIX/lib"         >> $out_file
$ECHO "SOLVE_SCRATCH_DATA =      $SOLVE_SCRATCH_DATA"       >> $out_file
$ECHO "SOLVE_SAVE_DATA    =      $SOLVE_SAVE_DATA"          >> $out_file
$ECHO "SOLVE_GVF_DATA     =      $SOLVE_GVF_DATA"           >> $out_file
$ECHO "SOLVE_SCRATCH_PIMA =      $SOLVE_SCRATCH_PIMA"       >> $out_file
$ECHO "SOLVE_CENTER_ABR   =      $SOLVE_CENTER_ABR"         >> $out_file
$ECHO "SOLVE_CENTER_NAME  = ${qt}$SOLVE_CENTER_NAME${qt}"   >> $out_file
$ECHO "SOLVE_PS_VIEWER    =      $SOLVE_PS_VIEWER"          >> $out_file
$ECHO "SOLVE_GIF_VIEWER   =      $SOLVE_GIF_VIEWER"         >> $out_file
$ECHO "SOLVE_CFITSIO_LIB  = ${qt}$SOLVE_CFITSIO_LIB${qt}"   >> $out_file
$ECHO "SOLVE_FITSLIB_LIB  = ${qt}$SOLVE_FITSLIB_LIB${qt}"   >> $out_file
$ECHO "SOLVE_OPT_NOSTRUC  = ${qt}$SOLVE_OPT_NOSTRUC${qt}"   >> $out_file
$ECHO "SOLVE_OS           = $SOLVE_OS"                      >> $out_file
$ECHO "SOLVE_VERSION      = $SOLVE_VERSION"                 >> $out_file
$ECHO "SOLVE_VERS         = $SOLVE_VERS"                    >> $out_file
$ECHO "NUM_PROC           = $num_cores"                     >> $out_file
$ECHO "CONF_LOG           = $CONF_LOG"                      >> $out_file
$ECHO "BUILD_LOG          = $BUILD_LOG"                     >> $out_file
cat   ${SOLVE_ROOT}/Makefile.in2                            >> $out_file
#
set out_vars = ${SOLVE_ROOT}/support/solve_vars
if ( -f $out_vars ) rm -f $out_vars
$ECHO "#\!/bin/csh"                                               > $out_vars
$ECHO "setenv PETOOLS_ROOT            $PETOOLS_ROOT"             >> $out_vars
$ECHO "setenv PETOOLS_PREFIX          $PETOOLS_PREFIX"           >> $out_vars
$ECHO "setenv SPD_CLIENT_PREFIX       $SOLVE_SPD_CLIENT_DIR"     >> $out_vars
$ECHO "setenv SPD_CLIENT_LIB          $SOLVE_SPD_CLIENT_DIR/lib" >> $out_vars
$ECHO "setenv NERS_LIB                $SOLVE_NERS_DIR/lib"       >> $out_vars
$ECHO "setenv NERS_INC                $SOLVE_NERS_DIR/include"   >> $out_vars
$ECHO "setenv VTD_LIB                 $SOLVE_VTD_DIR/lib"        >> $out_vars
$ECHO "setenv VTD_INC                 $SOLVE_VTD_DIR/include"    >> $out_vars
$ECHO "setenv GVH_LIB                 $SOLVE_GVH_DIR/lib"        >> $out_vars
$ECHO "setenv GVH_INC                 $SOLVE_GVH_DIR/include"    >> $out_vars
$ECHO "setenv VEX_LIB                 $SOLVE_VEX_PARSER_DIR/lib"     >> $out_vars
$ECHO "setenv VEX_INC                 $SOLVE_VEX_PARSER_DIR/include" >> $out_vars
$ECHO "setenv SOLVE_LIB_GVH      ${qt}-L$SOLVE_GVH_DIR/lib -lgvh -lvcat${qt}"      >> $out_vars
$ECHO "setenv SOLVE_LIB_VTD      ${qt}-L$SOLVE_VTD_DIR/lib -lvtd -L$SOLVE_SPD_CLIENT_DIR/lib -lspc -L$SOLVE_NERS_DIR/lib -lners${qt}"  >> $out_vars
$ECHO "setenv SOLVE_LIB_PETOOLS  ${qt}$PETOOLS_LIB${qt}"         >> $out_vars
$ECHO "setenv SOLVE_LIB_PGPLOT   ${qt}$SOLVE_LIB_PGPLOT${qt}"    >> $out_vars
$ECHO "setenv SOLVE_LIB_X11      ${qt}$SOLVE_LIB_X11${qt}"       >> $out_vars
$ECHO "setenv SOLVE_LIB_XT       ${qt}$SOLVE_LIB_XT${qt}"        >> $out_vars
$ECHO "setenv SOLVE_LIB_X11      ${qt}$SOLVE_LIB_X11${qt}"       >> $out_vars
$ECHO "setenv SOLVE_LIB_XHP11    ${qt}$SOLVE_LIB_XHP11${qt}"     >> $out_vars
$ECHO "setenv SOLVE_EXTRA_LIB    ${qt}$SOLVE_EXTRA_LIB${qt}"     >> $out_vars
$ECHO "setenv SOLVE_LIB_VEC      ${qt}$SOLVE_LIB_VEC${qt}"       >> $out_vars
$ECHO "setenv SOLVE_LIB_BLAS     ${qt}$SOLVE_LIB_BLAS${qt}"      >> $out_vars
$ECHO "setenv MK5_C              ${qt}$MK5_C${qt}"               >> $out_vars
$ECHO "setenv MK5_X11_INCLUDE    ${qt}$MK5_X11_INCLUDE${qt}"     >> $out_vars
if ( $SOLVE_NOOPT == "YES" ) then 
     $ECHO "setenv MK5_F95         ${qt}$MK5_F95_NOOPT -DDEBUG${qt}"  >> $out_vars
     $ECHO "setenv MK5_F95_OPT     ${qt}$MK5_F95_NOOPT -DDEBUG${qt}"  >> $out_vars
     $ECHO "setenv MK5_F95_OPTEST  ${qt}$MK5_F95_NOOPT -DDEBUG${qt}"  >> $out_vars
     $ECHO "setenv MK5_F95_NOOPT   ${qt}$MK5_F95_NOOPT -DDEBUG${qt}"  >> $out_vars
  else
     $ECHO "setenv MK5_F95            ${qt}$MK5_F95${qt}"        >> $out_vars
     $ECHO "setenv MK5_F95_OPT        ${qt}$MK5_F95_OPT${qt}"    >> $out_vars
     $ECHO "setenv MK5_F95_OPTEST     ${qt}$MK5_F95_OPTEST${qt}" >> $out_vars
     $ECHO "setenv MK5_F95_NOOPT      ${qt}$MK5_F95_NOOPT${qt}"  >> $out_vars
endif
$ECHO "setenv MK5_LINK           ${qt}$MK5_LINK${qt}"            >> $out_vars
$ECHO "setenv MK5_C_LINK         ${qt}$MK5_C_LINK${qt}"          >> $out_vars
$ECHO "setenv SOLVE_ROOT              $SOLVE_ROOT"               >> $out_vars
$ECHO "setenv SOLVE_PREFIX            $SOLVE_PREFIX"             >> $out_vars
$ECHO "setenv SOLVE_BIN               $SOLVE_PREFIX/bin"         >> $out_vars
$ECHO "setenv SOLVE_INC               $SOLVE_PREFIX/include"     >> $out_vars
$ECHO "setenv SOLVE_LIB               $SOLVE_PREFIX/lib"         >> $out_vars
$ECHO "setenv SOLVE_SCRATCH_DATA      $SOLVE_SCRATCH_DATA"       >> $out_vars
$ECHO "setenv SOLVE_SAVE_DATA         $SOLVE_SAVE_DATA"          >> $out_vars
$ECHO "setenv SOLVE_GVF_DATA          $SOLVE_GVF_DATA"           >> $out_vars
$ECHO "setenv SOLVE_SCRATCH_PIMA      $SOLVE_SCRATCH_PIMA"       >> $out_vars
$ECHO "setenv SOLVE_CENTER_ABR        $SOLVE_CENTER_ABR"         >> $out_vars
$ECHO "setenv SOLVE_CENTER_NAM   ${qt}$SOLVE_CENTER_NAME${qt}"   >> $out_vars
$ECHO "setenv SOLVE_PS_VIEWER         $SOLVE_PS_VIEWER"          >> $out_vars
$ECHO "setenv SOLVE_GIF_VIEWER        $SOLVE_GIF_VIEWER"         >> $out_vars
$ECHO "setenv SOLVE_CFITSIO_LIB  ${qt}$SOLVE_CFITSIO_LIB${qt}"   >> $out_vars
$ECHO "setenv SOLVE_FITSLIB_LIB  ${qt}$SOLVE_FITSLIB_LIB${qt}"   >> $out_vars
$ECHO "setenv SOLVE_OPT_NOSTRUC  ${qt}$SOLVE_OPT_NOSTRUC${qt}"   >> $out_vars
$ECHO "setenv SOLVE_OS           $SOLVE_OS"                      >> $out_vars
$ECHO "setenv SOLVE_VERSION      $SOLVE_VERSION"                 >> $out_vars
$ECHO "umask 0022"                                               >> $out_vars
#
source $out_vars
#
# --- Create a file with export environoment variables
#
set export_vars = ${SOLVE_ROOT}/support/export_solve_vars
if ( -f $export_vars  ) rm -f $export_vars
$ECHO "#\!/bin/csh"                                           > $export_vars
$ECHO "setenv SOLVE_PREFIX     $SOLVE_PREFIX"                 >> $export_vars
$ECHO "setenv SOLVE_INCLUDE    $SOLVE_PREFIX/include"         >> $export_vars
$ECHO "setenv SPOOL_DIR        $SOLVE_SCRATCH_DATA/spool_dir" >> $export_vars
$ECHO "setenv WORK_DIR         $SOLVE_SCRATCH_DATA/work_dir"  >> $export_vars
$ECHO "setenv CGM_DIR          $SOLVE_SCRATCH_DATA/cgm_dir"   >> $export_vars
$ECHO "setenv GVF_DIR          $SOLVE_GVF_DATA"               >> $export_vars
$ECHO "setenv PIMA_SCR         $SOLVE_SCRATCH_PIMA"           >> $export_vars
$ECHO "setenv SOLVE_VERSION    $SOLVE_VERSION"                >> $export_vars
$ECHO "setenv SOLVE_STP_DIR    $SOLVE_STP_DIR"                >> $export_vars
$ECHO "setenv SOLVE_PS_VIEWER  $SOLVE_PS_VIEWER"              >> $export_vars
$ECHO "setenv SOLVE_GIF_VIEWER $SOLVE_GIF_VIEWER"             >> $export_vars
$ECHO "setenv SOLVE_GVF_DIR    $SOLVE_GVF_DATA"               >> $export_vars
#
# --- Transform csh defintion file into bash defintion file
#
cat ${SOLVE_ROOT}/support/solve_vars | \
    awk '{ printf "export %s=", $2; for (i = 3; i <= NF; i++) printf ("%s ",$i); printf ("\n") }' | \
    sed  "s@export =@#\!/bin/bash@" > \
    ${SOLVE_ROOT}/support/solve_vars.sh
#
$ECHO "Remove stale include files, if present... \c" | tee -a  $CONF_LOG
make uninstall_include >>& $CONF_LOG
chmod g+rw,o+r ${SOLVE_ROOT}/Makefile
chmod g+rw,o+r ${SOLVE_ROOT}/support/solve_vars
chmod g+rw,o+r ${SOLVE_ROOT}/support/export_solve_vars
echo "ok" | tee -a  $CONF_LOG
chmod g+rw          $CONF_LOG >& /dev/null
#
echo "config.csh is done"
