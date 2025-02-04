#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Script  config.csh  sets numerous variables needed for             *
# *   compilation and linking pima. It creates files with directives     *
# *   for FORTRAN-compilers and C-compiler. It creates include files     *
# *   using template files for includes and local files with             *
# *   preferences.                                                       *
# *                                                                      *
# *   Usage:  config.csh                                                 *
# *                                                                      *
# *  ###  15-JUN-2009  config.csh  v1.20 (c)  L. Petrov  07-JAN-2025 ### *
# *                                                                      *
# ************************************************************************
#
set config_revision_date = "2025.01.07"
#
setenv SUPPORT_PATH `dirname $0`
#
set cfitsio_min_version = 3.0
set python_min_vers     = 3.2
#
cd     $SUPPORT_PATH
cd ../
if (    -f ${PIMA_ROOT}/Makefile ) then
     rm -f ${PIMA_ROOT}/Makefile
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
     set qb = "#\!"
     breaksw
endsw
#
# --- Check petools
#
echo  "pima config  revision date: " $config_revision_date | tee -a $CONF_LOG
if ( -d $PETOOLS_PREFIX == 0 ) then
     echo "pima config: petools directory $PETOOLS_PREFIX was not found"
     exit 1
endif
if ( -d $PETOOLS_PREFIX/lib == 0 ) then
     echo "pima config: petools directory $PETOOLS_PREFIX/lib was not found"
     exit 1
endif
if ( -d $PETOOLS_PREFIX/bin == 0 ) then
     echo "pima config: petools directory $PETOOLS_PREFIX/bin was not found"
     exit 1
endif
if ( -f $PETOOLS_PREFIX/bin/petools_vars ) then
      source $PETOOLS_PREFIX/bin/petools_vars 
      if ( $PETOOLS_VERSION < $PETOOLS_VERSION_MIN ) then
           echo "pima config: you have too old petools version $PETOOLS_VERSION"
           echo "pima config: while $PETOOLS_VERSION_MIN version is needed. Please upgrade"
           exit 1
      endif
endif
if ( -f $PETOOLS_PREFIX/lib/libpetools.a == 0 ) then
     echo "pima config: petools file $PETOOLS_PREFIX/lib/libpetools.a was not found"
     exit 1
endif
#
# --- Check vtd
#
if ( -d $VTD_PREFIX == 0 ) then
     echo "pima config: vtd directory $VTD_PREFIX was not found"
     exit 1
endif
if ( -d $VTD_PREFIX/lib == 0 ) then
     echo "pima config: vtd directory $VTD_PREFIX/lib was not found"
     exit 1
endif
if ( -d $VTD_PREFIX/bin == 0 ) then
     echo "pima config: vtd directory $VTD_PREFIX/bin was not found"
     exit 1
endif
if ( -f $VTD_PREFIX/bin/vtd_vars ) then
      source $VTD_PREFIX/bin/vtd_vars 
      if ( $VTD_VERSION < $VTD_VERSION_MIN ) then
           echo "pima config: you have too old vtd version $VTD_VERSION"
           echo "pima config: while $VTD_VERSION_MIN version is needed. Please upgrade"
           exit 1
      endif
endif
#
# --- Check gvh
#
if ( -d $GVH_PREFIX == 0 ) then
     echo "pima config: gvh directory $GVH_PREFIX was not found"
     exit 1
endif
if ( -d $GVH_PREFIX/lib == 0 ) then
     echo "pima config: gvh directory $GVH_PREFIX/lib was not found"
     exit 1
endif
if ( -d $GVH_PREFIX/bin == 0 ) then
     echo "pima config: gvh directory $GVH_PREFIX/bin was not found"
     exit 1
endif
if ( -f $GVH_PREFIX/bin/gvh_vars ) then
      source $GVH_PREFIX/bin/gvh_vars 
      if ( $GVH_VERSION < $GVH_VERSION_MIN ) then
           echo "pima config: you have too old gvh version $GVH_VERSION"
           echo "pima config: while $GVH_VERSION_MIN version is needed. Please upgrade"
           exit 1
      endif
endif
#
if ( -d  $NERS_PREFIX == 0 ) then
     echo "vtd config: ners install directory $NERS_PREFIX was not found"
     echo "vtd config: Please install ners"
     exit 1
endif
#
if ( -d  $NERS_PREFIX/include == 0 ) then
     echo "vtd config: ners include directory $NERS_PREFIX/include was not found"
     echo "vtd config: Please install ners"
     exit 1
endif
#
if ( -f  $NERS_PREFIX/include/ners_local.i == 0 ) then
     echo "vtd config: ners version file $NERS_PREFIX/include/ners_local.i was not found"
     echo "vtd config: Please install ners"
     exit 1
endif
set NERS_VERSION = `cat $NERS_PREFIX/include/ners_local.i | grep "NERS__VERSION =" | awk '{print substr($5,2,8)}'`
if ( "$NERS_VERSION" < "$NERS_VERSION_MIN" ) then
     echo "Eeeeh. You have ners version $NERS_VERSION, but $NERS_VERSION_MIN is needed."
     echo "Please upgrade."
     echo "You can download the new version of ners from http://earthrotation.net/ners"
     exit 1
endif
set NERS_STANDALONE = `cat $NERS_PREFIX/include/ners_local.i | grep "NERS__NERS_STANDALONE = " | awk '{print substr($5,2,8)}' | sed 's@"@@g'`
if ( "$NERS_STANDALONE" == "YES" ) then
     echo "vtd config: ners package was configured without petools"
     echo "vtd config: Please re-configure and re-install ners with petools support"
     exit 1
endif
#
# --- Check spc-client
#
if ( -d $SPC_PREFIX == 0 ) then
     echo "pima config: spd client directory $SPC_PREFIX was not found"
     exit 1
endif
if ( -d $SPC_PREFIX/lib == 0 ) then
     echo "pima config: spd client directory $SPC_PREFIX.lib was not found"
     exit 1
endif
if ( -d $SPC_PREFIX/include == 0 ) then
     echo "pima config: spd client directory $SPC_PREFIX/include was not found"
     exit 1
endif
if ( -f $SPC_PREFIX/lib/libspc.a == 0 ) then
     echo "pima config: spd client library $SPC_PREFIX/lib/libspc.a was not found"
     exit 1
endif
#
# --- Check fourpack
#
if ( -d $FOURPACK_PREFIX == 0 ) then
     echo "pima config: fourpack directory $FOURPACK_PREFIX was not found"
     exit 1
endif
if ( -d $FOURPACK_PREFIX/lib == 0 ) then
     echo "pima config: fourpack directory $FOURPACK_PREFIX/lib was not found"
     exit 1
endif
if ( -d $FOURPACK_PREFIX/bin == 0 ) then
     echo "pima config: fourpack directory $FOURPACK_PREFIX/bin was not found"
     exit 1
endif
if ( -f $FOURPACK_PREFIX/bin/fourpack_vars ) then
      source $FOURPACK_PREFIX/bin/fourpack_vars 
      if ( $FOURPACK_VERSION < $FOURPACK_VERSION_MIN ) then
           echo "pima config: you have too old fourpack version $FOURPACK_VERSION"
           echo "pima config: while $FOURPACK_VERSION_MIN version is needed. Please upgrade"
           exit 1
      endif
   else 
      echo "pima config: did not find file $FOURPACK_PREFIX/bin/fourpack_vars"
      exit 1
endif
if ( -f $FFTW_INC/fftw3.f == 0 ) then
     echo "pima config: include files for fftw were not found"
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
if ( -d $PIMA_SCRATCH_DIR == 0 ) then
     install -d $PIMA_SCRATCH_DIR
     if ( $status != 0 ) then
          echo "Failure in an attempt to create directory $PIMA_SCRATCH_DIR"
	  exit 1
     endif
endif
if ( -d $PIMA_FITS_DIR == 0 ) then
     install -d $PIMA_FITS_DIR
     if ( $status != 0 ) then
          echo "Failure in an attempt to create directory $PIMA_FITS_DIR"
	  exit 1
     endif
endif
if ( -d $PIMA_EXP_DIR == 0 ) then
     install -d $PIMA_EXP_DIR
     if ( $status != 0 ) then
          echo "Failure in an attempt to create directory $PIMA_EXP_DIR"
	  exit 1
     endif
endif
if ( -d $PIMA_SHARE_DIR == 0 ) then
     echo "Pima shared directory $PIMA_SHARE_DIR does not exist. The directory"
     echo "should exist. NB: PIMA does not write in that directory."
     exit 1
endif
if ( $PIMA_WITHOUT_SOLVE == "NO") then
     $ECHO "config.csh: Check for p-Solve files ... \c" | tee -a  $CONF_LOG
     if ( -d $PIMA_PSOLVE_DIR == 0 ) then
          echo "no" | tee -a $CONF_LOG
          echo "Did not find VTD/Post-Solve directory $PIMA_PSOLVE_DIR"               | tee -a $CONF_LOG
          echo "Have you installed Solve? If not, you can installed it (recommended)" | tee -a $CONF_LOG
          echo "or configure pima with --without-solve"
	  exit 1
     endif
     if ( -d $PIMA_PSOLVE_DIR/psolve/bin == 0 ) then
          echo "no" | tee -a $CONF_LOG
          echo "Did not find VTD/Post-Solve directory $PIMA_PSOLVE_DIR/psolve/bin"    | tee -a $CONF_LOG
          echo "Have you installed Solve? If not, you can installed it (recommended)" | tee -a $CONF_LOG
          echo "or configure pima with --without-solve"
	  exit 1
     endif
     if ( -f $PIMA_PSOLVE_DIR/psolve/bin/samb == 0 ) then
          echo "no" | tee -a $CONF_LOG
          echo "Did not find SAMB program in $PIMA_PSOLVE_DIR/bin"                    | tee -a $CONF_LOG
          echo "Have you installed Solve? If not, you can installed it (recommended)" | tee -a $CONF_LOG
          echo "or configure pima with --without-solve"
	  exit 1
     endif
     if ( -f $GVH_PREFIX/bin/gvf_supr_promote == 0 ) then
          echo "no" | tee -a $CONF_LOG                                     | tee -a $CONF_LOG
          echo "Did not find gvf_supr_promote program in $GVH_PREFIX/bin " | tee -a $CONF_LOG
	  exit 1
     endif
     if ( -f $GVH_PREFIX/bin/gvf_db == 0 ) then
          echo "Did not find gvf_db program in $GVH_PREFIX/bin " | tee -a $CONF_LOG
	  exit 1
     endif
     echo "ok" 
     $ECHO "config.csh: PIMA is configured with p-Solve support " | tee -a  $CONF_LOG
endif
#
$ECHO "config.csh: Check spd_client library... \c" | tee -a  $CONF_LOG
if ( -f $PIMA_ROOT/support/spc_test_01.o ) rm $PIMA_ROOT/support/spc_test_01.o
if ( -f $PIMA_ROOT/support/spc_test_01.e ) rm $PIMA_ROOT/support/spc_test_01.e
$MK5_F95 -c -o $PIMA_ROOT/support/spc_test_01.o $PIMA_ROOT/support/spc_test_01.f                          >>& $CONF_LOG 
$MK5_LINK   -o $PIMA_ROOT/support/spc_test_01.e $PIMA_ROOT/support/spc_test_01.o -L$SPC_PREFIX/lib -lspc  $SOLVE_LIB_BLAS $SOLVE_EXTRA_LIB >>& $CONF_LOG
if ( $status != 0 ) then
     echo "no" | tee -a $CONF_LOG
     echo "Failed to link against spd_client library" | tee -a  $CONF_LOG
     exit 1
endif
set spc_version = `$PIMA_ROOT/support/spc_test_01.e`
if ( $status != 0 ) then
     echo "no" | tee -a $CONF_LOG
     echo "Failed to execute a test program linked against spd_client library" | tee -a  $CONF_LOG
     exit 1
endif
if ( $spc_version < $SPC_VERSION_MIN ) then
     echo "no" | tee -a $CONF_LOG
     echo "Too old SPD_CLIENT  library: version $spc_version, while $SPC_VERSION_MIN is required" | tee -a  $CONF_LOG
     exit 1
endif
echo "ok" 
if ( -f $PIMA_ROOT/support/spc_test_01.o ) rm $PIMA_ROOT/support/spc_test_01.o
if ( -f $PIMA_ROOT/support/spc_test_01.e ) rm $PIMA_ROOT/support/spc_test_01.e
#
$ECHO "config.csh: Check spd_client library and petools together ... \c" | tee -a  $CONF_LOG
if ( -f $PIMA_ROOT/support/spc_test_02.o ) rm $PIMA_ROOT/support/spc_test_02.o
if ( -f $PIMA_ROOT/support/spc_test_02.e ) rm $PIMA_ROOT/support/spc_test_02.e
$MK5_F95 -c -I$SPC_PREFIX/include -o $PIMA_ROOT/support/spc_test_02.o \
            $PIMA_ROOT/support/spc_test_02.f >>& $CONF_LOG   
$MK5_LINK   -o $PIMA_ROOT/support/spc_test_02.e $PIMA_ROOT/support/spc_test_02.o \
            -L$SPC_PREFIX/lib -lspc $PETOOLS_LIB $SOLVE_LIB_BLAS $SOLVE_EXTRA_LIB | tee -a $CONF_LOG
if ( $status != 0 ) then
     echo "no" | tee -a $CONF_LOG
     echo "Failed to link against spd_client and petools libraries" | tee -a  $CONF_LOG
     exit 1
endif
$PIMA_ROOT/support/spc_test_02.e
if ( $status != 0 ) then
     echo "no" | tee -a $CONF_LOG
     echo "Failed to execute a test program linked against spd_client library" | tee -a  $CONF_LOG
     exit 1
endif
echo "ok" 
if ( -f $PIMA_ROOT/support/spc_test_02.o ) rm $PIMA_ROOT/support/spc_test_02.o
if ( -f $PIMA_ROOT/support/spc_test_02.e ) rm $PIMA_ROOT/support/spc_test_02.e
#
$ECHO "config.csh: Check for conflicts of spd_client and petools together ... \c" | tee -a  $CONF_LOG
if ( -f $PIMA_ROOT/support/spc_test_03.o ) rm $PIMA_ROOT/support/spc_test_03.o
if ( -f $PIMA_ROOT/support/spc_test_03.e ) rm $PIMA_ROOT/support/spc_test_03.e
$MK5_F95 -c -o $PIMA_ROOT/support/spc_test_03.o $PIMA_ROOT/support/spc_test_03.f \
               >>& $CONF_LOG   
$MK5_LINK   -o $PIMA_ROOT/support/spc_test_03.e $PIMA_ROOT/support/spc_test_03.o \
            -L$SPC_PREFIX/lib -lspc -L$PETOOLS_LIB $SOLVE_LIB_BLAS $SOLVE_EXTRA_LIB >>& $CONF_LOG
if ( $status != 0 ) then
     echo "no" | tee -a $CONF_LOG
     echo "There is a conflict between spd_client and petools"    | tee -a  $CONF_LOG
     echo "Please re-build spd_client with --with-petools switch" | tee -a  $CONF_LOG
     exit 1
endif
echo "ok" 
if ( -f $PIMA_ROOT/support/spc_test_03.o ) rm $PIMA_ROOT/support/spc_test_03.o
if ( -f $PIMA_ROOT/support/spc_test_03.e ) rm $PIMA_ROOT/support/spc_test_03.e
#
$ECHO "config.csh: Check for conflicts of spd_client and lapack togeather ... \c" | tee -a  $CONF_LOG
if ( -f $PIMA_ROOT/support/spc_test_04.o ) rm $PIMA_ROOT/support/spc_test_04.o
if ( -f $PIMA_ROOT/support/spc_test_04.e ) rm $PIMA_ROOT/support/spc_test_04.e
$MK5_F95 -c -o $PIMA_ROOT/support/spc_test_04.o $PIMA_ROOT/support/spc_test_04.f \
               >>& $CONF_LOG   
$MK5_LINK   -o $PIMA_ROOT/support/spc_test_04.e $PIMA_ROOT/support/spc_test_04.o \
              -L$SPC_PREFIX/lib -lspc -L$PETOOLS_PREFIX/lib -lpetools $SOLVE_LIB_BLAS >>& $CONF_LOG
if ( $status != 0 ) then
     echo "no" | tee -a $CONF_LOG
     echo "There is a conflict between spd_client and lapack"    | tee -a  $CONF_LOG
     echo "Please re-build spd_client with --with-lapack switch" | tee -a  $CONF_LOG
     exit 1
endif
echo "ok" 
if ( -f $PIMA_ROOT/support/spc_test_04.o ) rm $PIMA_ROOT/support/spc_test_04.o
if ( -f $PIMA_ROOT/support/spc_test_04.e ) rm $PIMA_ROOT/support/spc_test_04.e
#
set    date_stamp  = `date "+%Y.%m.%d-%H:%M:%S" | tr "[a-z]" "[A-Z]"`
set    host_name   = `uname -n`
#
if ( "$PIMA_CURL_DIR" == "" ) then
     setenv CURL_LIB ""
     setenv CURL_INC ""
  else
     if ( "$PIMA_CURL_DIR" == "-lcurl" ) then
          setenv CURL_LIB "-lcurl" 
          setenv CURL_INC ""
       else if ( -f $PIMA_CURL_DIR/lib/libcurl.so ) then
          setenv CURL_LIB "-L $PIMA_CURL_DIR/lib -lcurl"
          setenv CURL_INC "-I $PIMA_CURL_DIR/include"
       else if ( -f $PIMA_CURL_DIR/lib64/libcurl.so ) then
          setenv CURL_LIB "-L $PIMA_CURL_DIR/lib64 -lcurl"
          setenv CURL_INC "-I $PIMA_CURL_DIR/include"
       else
         echo "Cannot fund curl library in $PIMA_CURL_DIR"    | tee -a  $CONF_LOG
         exit 1
     endif
endif
#
setenv CFITSIO_LIB "-L $CFITSIO_PREFIX/lib -lcfitsio"
setenv CFITSIO_INC "-I $CFITSIO_PREFIX/include"
$ECHO "config.csh: Check cfitsio library... \c" | tee -a  $CONF_LOG
$PIMA_ROOT/support/check_cfitsio_version.csh >>&  $CONF_LOG
if ( $status != 0 ) then
     echo "Failed to link against cfitsio library" | tee -a  $CONF_LOG
     exit 1
endif
echo "ok" 
#
$ECHO "config.csh: Check cfitsio version >= $cfitsio_min_version ... \c" | tee -a  $CONF_LOG
set cfitsio_version = `$PIMA_ROOT/bin/check_cfitsio_version.e | awk '{print $3}'`
if ( `$PIMA_ROOT/support/version_equal_or_greater.csh $cfitsio_version $cfitsio_min_version` == 0    ) then
      echo "But found $cfitsio_version"
      echo "Please upgrade cfitsio to version $cfitsio_min_version or newer"
      exit 1
endif
echo $cfitsio_version 
make -f $PIMA_ROOT/support/check_cfitsio_version.mak clean 
#
echo "pima config  $date_stamp  on  $host_name" >>! $CONF_LOG
##
set out_file = ${PIMA_ROOT}/Makefile
#
if ( "$PETOOLS_VERSION" < "$PETOOLS_VERSION_MIN" ) then
     echo "Eeeeh. You have petools version $PETOOLS_VERSION, but "
     echo "$PETOOLS_VERSION_MIN is needed. Please upgrade"
     exit 1
endif
#
set DATE_ISO  = `date "+%Y.%m.%d_%H:%M:%S"`
set pima_version_len     = `echo $PIMA_VERSION       | awk '{print length($1)}'`
set pima_prefix_len      = `echo $PIMA_PREFIX        | awk '{print length}'`
set pima_root_len        = `echo $PIMA_ROOT          | awk '{print length($1)}'`
set pima_exp_dir_len     = `echo $PIMA_EXP_DIR       | awk '{print length($1)}'`
set pima_fits_dir_len    = `echo $PIMA_FITS_DIR      | awk '{print length($1)}'`
set pima_scratch_dir_len = `echo $PIMA_SCRATCH_DIR   | awk '{print length($1)}'`
set pima_share_dir_len   = `echo $PIMA_SHARE_DIR     | awk '{print length($1)}'`
set spd_url = "https://atmospheric-propagation.smce.nasa.gov/spd/asc/geosit"
#
cat include/pima_local.templ                                | \
    sed "s@PIMA__%%DATE@$DATE_ISO@g"                        | \
    sed "s@PIMA__VERSION%%LEN@$pima_version_len@g"          | \
    sed "s@PIMA__VERSION%%STR@$PIMA_VERSION@g"              | \
    sed "s@PIMA__PREFIX%%LEN@$pima_prefix_len@g"            | \
    sed "s@PIMA__PREFIX%%STR@$PIMA_PREFIX@g"                | \
    sed "s@PIMA__ROOT%%LEN@$pima_root_len@g"                | \
    sed "s@PIMA__ROOT%%STR@$PIMA_ROOT@g"                    | \
    sed "s@PIMA__EXP_DIR%%LEN@$pima_exp_dir_len@g"          | \
    sed "s@PIMA__EXP_DIR%%STR@$PIMA_EXP_DIR@g"              | \
    sed "s@PIMA__FITS_DIR%%LEN@$pima_fits_dir_len@g"        | \
    sed "s@PIMA__FITS_DIR%%STR@$PIMA_FITS_DIR@g"            | \
    sed "s@PIMA__SCRATCH_DIR%%LEN@$pima_scratch_dir_len@g"  | \
    sed "s@PIMA__SCRATCH_DIR%%STR@$PIMA_SCRATCH_DIR@g"      | \
    sed "s@PIMA__SHARE_DIR%%LEN@$pima_share_dir_len@g"      | \
    sed "s@PIMA__SHARE_DIR%%STR@$PIMA_SHARE_DIR@g"          > \
    include/pima_local.i
#
if ( $PIMA_WITHOUT_PYTHON == "NO" ) then
     set python_vers = `$PIMA_ROOT/support/python_test.py`
     if ( $status != 0 ) then
          echo "Eeeeh. You do not have Python3. You can either upgrade python (recommended) or"
          echo "run configure with a switch --without-python"
	  exit 1
     endif
     if ( `$PIMA_ROOT/support/version_equal_or_greater.csh $python_vers $python_min_vers` == "0" ) then
           echo "You have version of Python $python_vers"
           echo "but version $python_min_vers or higher is required"
           echo "You can either upgrade python (recommended) or"
           echo "run configure with a switch --without-python"
	   exit 1
     endif
     cat scripts/pima_local_templ.py                                         | \
         sed "s@%%date%%@$date_stamp@g"                                      | \
         sed "s@%%host%%@$host_name@g"                                       | \
         sed "s@%%pf_dir%%@$PIMA_EXP_DIR@g"                                  | \
         sed "s@%%pima_dynamic_exec%%@$PIMA_PREFIX/bin/pima@g"               | \
         sed "s@%%pima_static_exec%%@$PIMA_ROOT/bin/pima_static@g"           | \
         sed "s@%%pima_path%%@$PIMA_PREFIX@g"                                | \
         sed "s@%%pima_share_dir%%@$PIMA_SHARE_DIR@g"                        | \
         sed "s@%%doc_path%%@$PIMA_PREFIX/doc@g"                             | \
         sed "s@%%fitsh%%@$PIMA_PREFIX/bin/fitsh@g"                          | \
         sed "s@%%fits_to_map_exec%%@$PIMA_PREFIX/bin/fits_to_map@g"         | \
         sed "s@%%fits_to_cfd_exec%%@$PIMA_PREFIX/bin/fits_to_cfd@g"         | \
         sed "s@%%fits_to_radplot_exec%%@$PIMA_PREFIX/bin/fits_to_radplot@g" | \
         sed "s@%%log_to_antab_exec%%@$PIMA_PREFIX/bin/log_to_antab@g"       | \
         sed "s@%%gvf_promote_exec%%@$GVH_PREFIX/bin/gvf_supr_promote@g"     | \
         sed "s@%%gvf_db_exec%%@$GVH_PREFIX/bin/gvf_db@g"                    | \
         sed "s@%%samb_exec%%@$PIMA_PSOLVE_DIR/psolve/bin/samb@g"            | \
         sed "s@%%difmap_exec%%@$PIMA_DIFMAP_DIR/bin/difmap@g"               | \
         sed "s@%%automap_exec%%@$PIMA_PREFIX/bin/automap.py@g"              | \
         sed "s@%%ners_prefix%%@$NERS_PREFIX@g"                              | \
         sed "s@%%spc_prefix%%@$SPC_PREFIX@g"                                | \
         sed "s@%%vtd_prefix%%@$VTD_PREFIX@g"                                | \
         sed "s@%%spd_url%%@$spd_url@g"                                      > \
         scripts/pima_local.py 
#
     cat scripts/dimap_templ.csh                                             | \
         sed "s@%%date%%@$date_stamp@g"                                      | \
         sed "s@%%host%%@$host_name@g"                                       | \
         sed "s@%%pf_dir%%@$PIMA_EXP_DIR@g"                                  | \
         sed "s@%%pima_dynamic_exec%%@$PIMA_PREFIX/bin/pima@g"               | \
         sed "s@%%pima_static_exec%%@$PIMA_ROOT/bin/pima_static@g"           | \
         sed "s@%%pima_path%%@$PIMA_PREFIX@g"                                | \
         sed "s@%%pima_share_dir%%@$PIMA_SHARE_DIR@g"                        | \
         sed "s@%%doc_path%%@$PIMA_PREFIX/doc@g"                             | \
         sed "s@%%fitsh%%@$PIMA_PREFIX/bin/fitsh@g"                          | \
         sed "s@%%fits_to_map_exec%%@$PIMA_PREFIX/bin/fits_to_map@g"         | \
         sed "s@%%fits_to_cfd_exec%%@$PIMA_PREFIX/bin/fits_to_cfd@g"         | \
         sed "s@%%fits_to_radplot_exec%%@$PIMA_PREFIX/bin/fits_to_radplot@g" | \
         sed "s@%%log_to_antab_exec%%@$PIMA_PREFIX/bin/log_to_antab@g"       | \
         sed "s@%%gvf_promote_exec%%@$GVH_PREFIX/bin/gvf_supr_promote@g"     | \
         sed "s@%%gvf_db_exec%%@$GVH_PREFIX/bin/gvf_db@g"                    | \
         sed "s@%%samb_exec%%@$PIMA_PSOLVE_DIR/psolve/bin/samb@g"            | \
         sed "s@%%difmap_exec%%@$PIMA_DIFMAP_DIR/bin/difmap@g"               | \
         sed "s@%%automap_exec%%@$PIMA_PREFIX/bin/automap.py@g"              | \
         sed "s@%%ners_prefix%%@$NERS_PREFIX@g"                              | \
         sed "s@%%spc_prefix%%@$SPC_PREFIX@g"                                | \
         sed "s@%%vtd_prefix%%@$VTD_PREFIX@g"                                > \
         scripts/dimap
     chmod o+x,u+x,g+x scripts/dimap
endif
if ( `uname` == "Linux" ) then
     set num_threads     = `cat /proc/cpuinfo | grep "core id" | wc -l`
     set num_thr_per_cpu = `lscpu | grep '^Thread(s) per core:' | awk '{print $4}'`
     set num_cores = `expr $num_threads / $num_thr_per_cpu`
else
     set num_cores = `sysctl -n  machdep.cpu.core_count`
endif
#
cat   ${PIMA_ROOT}/Makefile.in1 > $out_file 
$ECHO "PIMA_ROOT    = $PIMA_ROOT"                        >> $out_file
$ECHO "PETOOLS_PREFIX     = $PETOOLS_PREFIX"             >> $out_file
$ECHO "PETOOLS_LIB        = $PETOOLS_LIB"                >> $out_file
$ECHO "PETOOLS_LIB_A      = $PETOOLS_LIB_A"              >> $out_file
$ECHO "PETOOLS_OS         = $PETOOLS_OS"                 >> $out_file
$ECHO "SOLVE_LIB_PGPLOT   = $SOLVE_LIB_PGPLOT"           >> $out_file
$ECHO "SOLVE_LIB_PGPLOT_A = $SOLVE_LIB_PGPLOT"           >> $out_file
$ECHO "SOLVE_LIB_X11      = $SOLVE_LIB_X11"              >> $out_file
$ECHO "SOLVE_EXTRA_LIB    = $SOLVE_EXTRA_LIB"            >> $out_file
$ECHO "SOLVE_LIB_VEC      = $SOLVE_LIB_VEC"              >> $out_file
$ECHO "SOLVE_LIB_BLAS     = $SOLVE_LIB_BLAS"             >> $out_file
$ECHO "MK5_C              = $MK5_C"                      >> $out_file
if ( $PIMA_NOOPT == "YES" ) then 
     $ECHO "MK5_F95         = ${qt}$MK5_F95_NOOPT${qt}"  >> $out_file
     $ECHO "MK5_F95_OPT     = ${qt}$MK5_F95_NOOPT${qt}"  >> $out_file
     $ECHO "MK5_F95_OPTEST  = ${qt}$MK5_F95_NOOPT${qt}"  >> $out_file
     $ECHO "MK5_F95_NOOPT   = ${qt}$MK5_F95_NOOPT${qt}"  >> $out_file
  else
     $ECHO "MK5_F95            = ${qt}$MK5_F95${qt}"        >> $out_file
     $ECHO "MK5_F95_OPT        = ${qt}$MK5_F95_OPT${qt}"    >> $out_file
     $ECHO "MK5_F95_OPTEST     = ${qt}$MK5_F95_OPTEST${qt}" >> $out_file
     $ECHO "MK5_F95_NOOPT      = ${qt}$MK5_F95_NOOPT${qt}"  >> $out_file
endif
$ECHO "MK5_C_LINK       = $MK5_C_LINK"                   >> $out_file
$ECHO "MK5_LINK         = $MK5_LINK"                     >> $out_file
$ECHO "PIMA_ROOT        = $PIMA_ROOT"                    >> $out_file
$ECHO "PIMA_PREFIX      = $PIMA_PREFIX"                  >> $out_file
$ECHO "PIMA_BIN         = $PIMA_PREFIX/bin"              >> $out_file
$ECHO "PIMA_LIB         = $PIMA_PREFIX/lib"              >> $out_file
$ECHO "PIMA_INC         = $PIMA_PREFIX/include"          >> $out_file
$ECHO "GVH_LIB          = $GVH_LIB"                      >> $out_file
$ECHO "GVH_PREFIX       = $GVH_PREFIX"                   >> $out_file
$ECHO "GVH_INC          = $GVH_PREFIX/include"           >> $out_file
$ECHO "CURL_LIB         = ${qt}$CURL_LIB${qt}"           >> $out_file
$ECHO "CURL_INC         = ${qt}$CURL_INC${qt}"           >> $out_file
$ECHO "VTD_PREFIX       = $VTD_PREFIX"                   >> $out_file
$ECHO "VTD_LIB          = $VTD_LIB"                      >> $out_file
$ECHO "VTD_INC          = $VTD_PREFIX/include"           >> $out_file
$ECHO "NERS_PREFIX      = $NERS_PREFIX"                  >> $out_file
$ECHO "NERS_LIB         = -L $NERS_PREFIX/lib -lners"    >> $out_file
$ECHO "NERS_INC         = $NERS_PREFIX/include"          >> $out_file
$ECHO "FOURPACK_PREFIX  = $FOURPACK_PREFIX"              >> $out_file
$ECHO "FOURPACK_LIB     = $FOURPACK_LIB"                 >> $out_file
$ECHO "FOURPACK_LIB_STATIC     = $FOURPACK_LIB_STATIC"   >> $out_file
$ECHO "FOURPACK_INC     = $FOURPACK_PREFIX/include"      >> $out_file
$ECHO "FFTW_LIB         = $FFTW_LIB"                     >> $out_file
$ECHO "FFTW_INC         = $FFTW_INC"                     >> $out_file
$ECHO "SPC_PREFIX       = $SPC_PREFIX"                   >> $out_file
$ECHO "SPC_LIB          = -L $SPC_PREFIX/lib -lspc"      >> $out_file
$ECHO "SPC_INC          = -L $SPC_PREFIX/include"        >> $out_file
$ECHO "CFITSIO_PREFIX   = $CFITSIO_PREFIX"               >> $out_file
$ECHO "CFITSIO_LIB      = -L $CFITSIO_PREFIX/lib -lcfitsio" >> $out_file
$ECHO "CFITSIO_INC      = -I $CFITSIO_PREFIX/include"       >> $out_file
$ECHO "PIMA_VERSION     = $PIMA_VERSION"                 >> $out_file
$ECHO "NUM_PROC         = $num_cores"                    >> $out_file
$ECHO "PIMA_PSOLVE_DIR     = $PIMA_PSOLVE_DIR"           >> $out_file
$ECHO "PIMA_EXP_DIR        = $PIMA_EXP_DIR"              >> $out_file
$ECHO "PIMA_DIFMAP_DIR     = $PIMA_DIFMAP_DIR"           >> $out_file
$ECHO "PIMA_CURL_DIR       = $PIMA_CURL_DIR"             >> $out_file
$ECHO "PIMA_SHARE_DIR      = $PIMA_SHARE_DIR"            >> $out_file
$ECHO "PIMA_WITHOUT_PYTHON = $PIMA_WITHOUT_PYTHON"       >> $out_file
$ECHO "PIMA_WITHOUT_SOLVE  = $PIMA_WITHOUT_SOLVE"        >> $out_file
$ECHO "PIMA_WITHOUT_DIFMAP = $PIMA_WITHOUT_DIFMAP"       >> $out_file
$ECHO "PIMA_NOOPT          = $PIMA_NOOPT"                >> $out_file
$ECHO "CONF_LOG          = $CONF_LOG"                    >> $out_file
$ECHO "BUILD_LOG         = $BUILD_LOG"                   >> $out_file
cat   ${PIMA_ROOT}/Makefile.in2                          >> $out_file
#
set out_vars = ${PIMA_ROOT}/support/pima_vars
if ( -f $out_vars ) rm -f $out_vars
$ECHO "#\!/bin/csh"                                               > $out_vars
$ECHO "setenv PIMA_ROOT          $PIMA_ROOT"                     >> $out_vars
$ECHO "setenv PETOOLS_PREFIX     $PETOOLS_PREFIX"                >> $out_vars
$ECHO "setenv PETOOLS_LIB        ${qt}$PETOOLS_LIB${qt}"         >> $out_vars
$ECHO "setenv PETOOLS_LIB_A      ${qt}$PETOOLS_LIB_A${qt}"       >> $out_vars
$ECHO "setenv PETOOLS_OS         ${qt}$PETOOLS_OS${qt}"          >> $out_vars
$ECHO "setenv SOLVE_LIB_PGPLOT   ${qt}$SOLVE_LIB_PGPLOT${qt}"    >> $out_vars
$ECHO "setenv SOLVE_LIB_PGPLOT_A ${qt}$SOLVE_LIB_PGPLOT_A${qt}"  >> $out_vars
$ECHO "setenv SOLVE_LIB_X11      ${qt}$SOLVE_LIB_X11${qt}"       >> $out_vars
$ECHO "setenv SOLVE_EXTRA_LIB    ${qt}$SOLVE_EXTRA_LIB${qt}"     >> $out_vars
$ECHO "setenv SOLVE_LIB_VEC      ${qt}$SOLVE_LIB_VEC${qt}"       >> $out_vars
$ECHO "setenv SOLVE_LIB_BLAS     ${qt}$SOLVE_LIB_BLAS${qt}"      >> $out_vars
$ECHO "setenv MK5_C              ${qt}$MK5_C${qt}"               >> $out_vars
if ( $PIMA_NOOPT == "YES" ) then 
     $ECHO "setenv MK5_F95         ${qt}$MK5_F95_NOOPT${qt}"     >> $out_vars
     $ECHO "setenv MK5_F95_OPT     ${qt}$MK5_F95_NOOPT${qt}"     >> $out_vars
     $ECHO "setenv MK5_F95_OPTEST  ${qt}$MK5_F95_NOOPT${qt}"     >> $out_vars
     $ECHO "setenv MK5_F95_NOOPT   ${qt}$MK5_F95_NOOPT${qt}"     >> $out_vars
  else
     $ECHO "setenv MK5_F95            ${qt}$MK5_F95${qt}"        >> $out_vars
     $ECHO "setenv MK5_F95_OPT        ${qt}$MK5_F95_OPT${qt}"    >> $out_vars
     $ECHO "setenv MK5_F95_OPTEST     ${qt}$MK5_F95_OPTEST${qt}" >> $out_vars
     $ECHO "setenv MK5_F95_NOOPT      ${qt}$MK5_F95_NOOPT${qt}"  >> $out_vars
endif
$ECHO "setenv MK5_LINK         ${qt}$MK5_LINK${qt}"         >> $out_vars
$ECHO "setenv MK5_C_LINK       ${qt}$MK5_C_LINK${qt}"       >> $out_vars
$ECHO "setenv PIMA_ROOT        $PIMA_ROOT"                  >> $out_vars
$ECHO "setenv PIMA_PREFIX      $PIMA_PREFIX"                >> $out_vars
$ECHO "setenv PIMA_BIN         $PIMA_PREFIX/bin"            >> $out_vars
$ECHO "setenv PIMA_LIB         $PIMA_PREFIX/lib"            >> $out_vars
$ECHO "setenv PIMA_INC         $PIMA_PREFIX/include"        >> $out_vars
$ECHO "setenv GVH_PREFIX       $GVH_PREFIX"                 >> $out_vars
$ECHO "setenv VTD_PREFIX       $VTD_PREFIX"                 >> $out_vars
$ECHO "setenv NERS_PREFIX      $NERS_PREFIX"                >> $out_vars
$ECHO "setenv FOURPACK_PREFIX  $FOURPACK_PREFIX"            >> $out_vars
$ECHO "setenv SPC_PREFIX       $SPC_PREFIX"                 >> $out_vars
$ECHO "setenv CFITSIO_PREFIX   $CFITSIO_PREFIX"             >> $out_vars
$ECHO "setenv CURL_LIB         ${qt}$CURL_LIB${qt}"         >> $out_vars
$ECHO "setenv CURL_INC         ${qt}$CURL_INC${qt}"         >> $out_vars
$ECHO "setenv GVH_LIB          ${qt}$GVH_LIB${qt}"          >> $out_vars
$ECHO "setenv GVH_INC          $GVH_PREFIX/include"         >> $out_vars
$ECHO "setenv VTD_LIB          ${qt}$VTD_LIB${qt}"          >> $out_vars
$ECHO "setenv VTD_INC          $VTD_PREFIX/include"         >> $out_vars
$ECHO "setenv NERS_LIB         ${qt}-L $NERS_PREFIX/lib -lners${qt}" >> $out_vars
$ECHO "setenv NERS_INC         $NERS_PREFIX/include"        >> $out_vars
$ECHO "setenv FOURPACK_LIB     ${qt}$FOURPACK_LIB${qt}"     >> $out_vars
$ECHO "setenv FOURPACK_LIB_STATIC     ${qt}$FOURPACK_LIB_STATIC${qt}"     >> $out_vars
$ECHO "setenv FOURPACK_INC     $FOURPACK_PREFIX/include"    >> $out_vars
$ECHO "setenv FFTW_LIB         ${qt}$FFTW_LIB${qt}"         >> $out_vars
$ECHO "setenv FFTW_INC         $FFTW_INC"                   >> $out_vars
$ECHO "setenv SPC_LIB          ${qt}-L $SPC_PREFIX/lib -lspc${qt}"         >> $out_vars
$ECHO "setenv SPC_INC          $SPC_PREFIX/include"         >> $out_vars
$ECHO "setenv CFITSIO_LIB      ${qt}-L $CFITSIO_PREFIX/lib -lcfitsio${qt}" >> $out_vars
$ECHO "setenv CFITSIO_INC      $CFITSIO_PREFIX/include"     >> $out_vars
$ECHO "setenv PIMA_WITHOUT_PYTHON  ${qt}$PIMA_WITHOUT_PYTHON${qt}" >> $out_vars
$ECHO "setenv PIMA_WITHOUT_SOLVE   ${qt}$PIMA_WITHOUT_SOLVE${qt}"  >> $out_vars
$ECHO "setenv PIMA_WITHOUT_DIFMAP  ${qt}$PIMA_WITHOUT_SOLVE${qt}"  >> $out_vars
$ECHO "setenv PIMA_PSOLVE_DIR      $PIMA_PSOLVE_DIR"               >> $out_vars
$ECHO "setenv PIMA_DIFMAP_DIR      $PIMA_DIFMAP_DIR"               >> $out_vars
$ECHO "setenv PIMA_CURL_DIR        $PIMA_CURL_DIR"                 >> $out_vars
$ECHO "setenv PIMA_SHARE_DIR       $PIMA_SHARE_DIR"                >> $out_vars
$ECHO "setenv PIMA_NOOPT           $PIMA_NOOPT"                    >> $out_vars
$ECHO "setenv PIMA_EXP_DIR         $PIMA_EXP_DIR"                  >> $out_vars
$ECHO "setenv PIMA_VERSION         $PIMA_VERSION"                  >> $out_vars
$ECHO "umask 0022"                                                 >> $out_vars
#
source ${PIMA_ROOT}/support/pima_vars
#
cat ${PIMA_ROOT}/share/pima/r1447_x_pima.templ | \
    sed "s@__PIMA-FITS__@$PIMA_FITS_DIR@g"  | \
    sed "s@__VTD_DATA__@$VTD_DATA@g"  | \
    sed "s@__PIMA_SHARE__@${PIMA_PREFIX}/share/pima@g" | \
    sed "s@__PIMA_SCRATCH__@$PIMA_SCRATCH_DIR@g"  \
    > ${PIMA_ROOT}/examples/r1447_x_pima.cnt
#
cat ${PIMA_ROOT}/share/pima/r1447_s_pima.templ | \
    sed "s@__PIMA-FITS__@$PIMA_FITS_DIR@g"  | \
    sed "s@__VTD_DATA__@$VTD_DATA@g"  | \
    sed "s@__PIMA_SHARE__@${PIMA_PREFIX}/share/pima@g" | \
    sed "s@__PIMA_SCRATCH__@$PIMA_SCRATCH_DIR@g"  \
    > ${PIMA_ROOT}/examples/r1447_s_pima.cnt
#
source $VTD_PREFIX/bin/vtd_vars
cat ${VTD_DATA}/vtd_test_01.cnf > \
    ${PIMA_ROOT}/examples/pima_basic.vtd
#
cat ${PIMA_ROOT}/share/pima/vcat_conf.templ | \
    sed "s@__PIMA_SHARE__@${PIMA_PREFIX}/share/pima@g"  \
    > ${PIMA_ROOT}/examples/vcat.conf
#
chmod g+rw,o+r ${PIMA_ROOT}/Makefile
chmod g+rw,o+r ${PIMA_ROOT}/support/pima_vars
#
echo "config.csh is done"
