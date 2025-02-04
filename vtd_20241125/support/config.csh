#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Script  config.csh  sets numerous variables needed for             *
# *   compilation and linking vtd. It creates files with directives      *
# *   for FORTRAN-compilers and C-compiler. It creates include files     *
# *   using template files for includes and local files with             *
# *   preferences.                                                       *
# *                                                                      *
# *   Usage:  config.csh                                                 *
# *                                                                      *
# *  ###  18-MAY-2004  config.csh  v1.18 (c)  L. Petrov  24-MAY-2019 ### *
# *                                                                      *
# ************************************************************************
#
set config_revision_date = "2019.05.24"
#
setenv SUPPORT_PATH `dirname $0`
set cfitsio_min_version = 3.0
#
cd     $SUPPORT_PATH
cd ../
if (    -f ${VTD_ROOT}/Makefile ) then
     rm -f ${VTD_ROOT}/Makefile
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
umask 0022
set    date_stamp  = `date "+%Y.%m.%d-%H:%M:%S" | tr "[a-z]" "[A-Z]"`
set    host_name   = `uname -n`
echo "vtd config  $date_stamp  on  $host_name"           >>! $CONF_LOG
echo "vtd config  revision date: " $config_revision_date >>! $CONF_LOG
#
if ( -d $PETOOLS_DIR == 0 ) then
     echo "vtd config: petools directory $PETOOLS_DIR was not found"
     exit 1
endif
if ( -d $PETOOLS_DIR/lib == 0 ) then
     echo "vtd config: petools directory $PETOOLS_DIR/lib was not found"
     exit 1
endif
if ( -d $PETOOLS_DIR/bin == 0 ) then
     echo "vtd config: petools directory $PETOOLS_DIR/bin was not found"
     exit 1
endif
if ( -f $PETOOLS_DIR/lib/libpetools.a == 0 ) then
     echo "vtd config: petools file $PETOOLS_DIR/lib/libpetools.a was not found"
     exit 1
endif
#
source $PETOOLS_DIR/bin/petools_vars
if ( $VTD_CFITSIO_DIR != "" ) then
     if ( "$VTD_CURL_DIR" == "" ) then
          setenv CURL_LIB ""
          setenv CURL_INC ""
       else
          if ( "$VTD_CURL_DIR" == "/usr/include" ) then
               setenv CURL_LIB "-lcurl" 
               setenv CURL_INC "/usr/include/curl"
            else if ( -f $VTD_CURL_DIR/lib/libcurl.so ) then
               setenv CURL_LIB "-L$VTD_CURL_DIR/lib -lcurl"
               setenv CURL_INC "$VTD_CURL_DIR/include/curl"
            else if ( -f $VTD_CURL_DIR/lib/libcurl.dylib ) then
               setenv CURL_LIB "-L$VTD_CURL_DIR/lib -lcurl"
               setenv CURL_INC "$VTD_CURL_DIR/include/curl"
            else if ( -f $VTD_CURL_DIR/lib64/libcurl.so ) then
               setenv CURL_LIB "-L$VTD_CURL_DIR/lib64 -lcurl"
               setenv CURL_INC "$VTD_CURL_DIR/include/curl"
            else if ( -f $VTD_CURL_DIR/lib64/libcurl.dylib ) then
               setenv CURL_LIB "-L$VTD_CURL_DIR/lib64 -lcurl"
               setenv CURL_INC "$VTD_CURL_DIR/include/curl"
            else
              echo "Cannot fund curl library in $VTD_CURL_DIR"    | tee -a  $CONF_LOG
              exit 1
          endif
	  if ( -f $CURL_INC/curl.h == 0 ) then
               echo "vtd config: $CURL_INC/curl.h was not found"
               echo "vtd config: You need either build curl from sources or isntgal curl developement package"
               exit 1
          end if
     endif
#
     if ( -d $VTD_CFITSIO_DIR == 0 ) then
          echo "vtd config: cfitsio directory $VTD_CFITSIO_DIR was not found"
          exit 1
     endif
     if ( -f $VTD_CFITSIO_DIR/lib/libcfitsio.a == 0 ) then
          echo "vtd config: $VTD_CFITSIO_DIR/lib/libcfitsio.a library was not found"
          exit 1
     endif
     if ( -f $VTD_CFITSIO_DIR/include/fitsio.h == 0 ) then
          echo "vtd config: $VTD_CFITSIO_DIR/include/fitsio.h include file was not found"
          exit 1
     endif
     setenv VTD_OPT_NOSTRUC ""
     setenv VTD_CFITSIO_LIB $VTD_CFITSIO_DIR/lib/libcfitsio.a 
     setenv VTD_FITSLIB_LIB $VTD_ROOT/fitslib/libfitslib.a
     setenv CFITSIO_LIB "-L $VTD_CFITSIO_DIR/lib -lcfitsio"
     setenv CFITSIO_INC "$VTD_CFITSIO_DIR/include"
#
     $ECHO "config.csh: Check cfitsio library... \c" | tee -a  $CONF_LOG
     $VTD_ROOT/support/check_cfitsio_version.csh >>& $CONF_LOG
     if ( $status != 0 ) then
          echo "Failed to link against cfitsio library" | tee -a  $CONF_LOG
          echo "Please look at temp/conf.log for hints" | tee -a  $CONF_LOG
          echo "CFITSIO_LIB = $CFITSIO_LIB"             | tee -a  $CONF_LOG
          echo "CFITSIO_INC = $CFITSIO_INC"             | tee -a  $CONF_LOG
          echo "VTD_CFITSIO_LIB = $VTD_CFITSIO_LIB"     | tee -a  $CONF_LOG
          echo "Check file temp.conf for detail"        | tee -a  $CONF_LOG
          exit 1
     endif
     echo "ok" 
#
     $ECHO "config.csh: Check cfitsio version >= $cfitsio_min_version ... \c" | tee -a  $CONF_LOG
     set cfitsio_version = `$VTD_ROOT/bin/check_cfitsio_version.e | awk '{print $3}'`
     if ( `$VTD_ROOT/support/version_equal_or_greater.csh $cfitsio_version $cfitsio_min_version` == 0    ) then
           echo "But found $cfitsio_version"
           echo "Please upgrade cfitsio to version $cfitsio_min_version or newer"
          exit 1
     endif
     echo $cfitsio_version 
  else 
     setenv VTD_OPT_NOSTRUC "-D VTD__NO_STRUC"
     setenv VTD_CFITSIO_LIB ""
     setenv VTD_FITSLIB_LIB ""
endif
if ( -f $VTD_SPD_CLIENT_DIR/lib/libspc.a == 0 ) then
     echo "vtd config: spd_client file $VTD_SPD_CLIENT_DIR/lib/libspc.a  was not found"
     echo "vtd config: Please install spd_client"
     exit 1
endif
if ( -f $VTD_SPD_CLIENT_DIR/include/spd.i == 0 ) then
     echo "vtd config: spd_client file $VTD_SPD_CLIENT_DIR/include/spd.i was not found"
     echo "vtd config: Please install spd_client"
     exit 1
endif
if ( -f $VTD_SPD_CLIENT_DIR/include/spd_local.i == 0 ) then
     echo "vtd config: spd_client file $VTD_SPD_CLIENT_DIR/include/spd_local.i was not found"
     echo "vtd config: Please install spd_client"
     exit 1
endif
if ( `cat $VTD_SPD_CLIENT_DIR/include/spd_local.i | grep "SPD__PETOOLS = " | grep yes` == "" && \
     `cat $VTD_SPD_CLIENT_DIR/include/spd_local.i | grep "SPC__PETOOLS = " | grep yes` == "" && ) then
     echo "vtd config: spd_client was installed without petools"
     echo "vtd config: Please re-install spd_client with petools"
     exit 1
endif
set VTD_SPD_CLIENT_VERSION = `cat $VTD_SPD_CLIENT_DIR/include/spd_local.i | grep "SPD__VERSION =" | awk '{print substr($5,2,8)}'`
if ( "$VTD_SPD_CLIENT_VERSION" < "$SPD_CLIENT_VERSION_MIN" ) then
     echo "Eeeeh. You have spd_client version $VTD_SPD_CLIENT_VERSION, but $SPD_CLIENT_VERSION_MIN is needed."
     echo "Please upgrade."
     echo "You can download the new version of spd_client from http://astrogeo.org/spd"
     exit 1
endif
if ( -d  $VTD_NERS_DIR == 0 ) then
     echo "vtd config: ners install directory $VTD_NERS_DIR was not found"
     echo "vtd config: Please install ners"
     exit 1
endif
#
if ( -d  $VTD_NERS_DIR/include == 0 ) then
     echo "vtd config: ners include directory $VTD_NERS_DIR/include was not found"
     echo "vtd config: Please install ners"
     exit 1
endif
#
if ( -f  $VTD_NERS_DIR/include/ners_local.i == 0 ) then
     echo "vtd config: ners version file $VTD_NERS_DIR/include/ners_local.i was not found"
     echo "vtd config: Please install ners"
     exit 1
endif
set VTD_NERS_VERSION = `cat $VTD_NERS_DIR/include/ners_local.i | grep "NERS__VERSION =" | awk '{print substr($5,2,8)}'`
if ( "$VTD_NERS_VERSION" < "$NERS_VERSION_MIN" ) then
     echo "Eeeeh. You have ners version $VTD_NERS_VERSION, but $NERS_VERSION_MIN is needed."
     echo "Please upgrade."
     echo "You can download the new version of ners from http://earthrotation.net/ners"
     exit 1
endif
set VTD_NERS_STANDALONE = `cat $VTD_NERS_DIR/include/ners_local.i | grep "NERS__NERS_STANDALONE = " | awk '{print substr($5,2,8)}' | sed 's@"@@g'`
if ( "$VTD_NERS_STANDALONE" == "YES" ) then
     echo "vtd config: ners package was configured without petools"
     echo "vtd config: Please re-configure and re-install ners with petools support"
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
echo "vtd config  $date_stamp  on  $host_name" 
#
# --- Creation of ./include/vtd.i
#
if ( $PETOOLS_BITS == 64 ) then
     cat $VTD_ROOT/include/vtd_i.templ | \
         sed 's@ADDRESS__TYPE@INTEGER*8@g' > \
         $VTD_ROOT/include/vtd.i
  else 
     cat $VTD_ROOT/include/vtd_i.templ | \
         sed 's@ADDRESS__TYPE@INTEGER*4@g' > \
         $VTD_ROOT/include/vtd.i
endif
#
# --- Create vtd_example_01.f, vtd_example_02.f, vtd_example_03.f, 
# --- vtd_example_04.f, vtd_example_06.f, and vtd_example_05.c from templates
#
sed "s|__VTD_SHARE__|$VTD_DATA|g" \
     $VTD_ROOT/example/vtd_example_01.template > \
     $VTD_ROOT/example/vtd_example_01.f
sed "s|__VTD_SHARE__|$VTD_DATA|g" \
     $VTD_ROOT/example/vtd_example_02.template > \
     $VTD_ROOT/example/vtd_example_02.f
sed "s|__VTD_SHARE__|$VTD_DATA|g" \
     $VTD_ROOT/example/vtd_example_03.template > \
     $VTD_ROOT/example/vtd_example_03.f
sed "s|__VTD_SHARE__|$VTD_DATA|g" \
     $VTD_ROOT/example/vtd_example_04.template > \
     $VTD_ROOT/example/vtd_example_04.f
sed "s|__VTD_SHARE__|$VTD_DATA|g" \
     $VTD_ROOT/example/vtd_example_05.template > \
     $VTD_ROOT/example/vtd_example_05.c
sed "s|__VTD_SHARE__|$VTD_DATA|g" \
     $VTD_ROOT/example/vtd_example_06.template > \
     $VTD_ROOT/example/vtd_example_06.f
#
# --- Create vtd_test_01.cnf, vtd_test_02.cnf, vtd_test_03.cnf from templates
#
sed "s|__VTD_SHARE__|$VTD_DATA|g" \
     $VTD_ROOT/share/vtd_test_01_template.cnf > \
     $VTD_ROOT/temp/vtd_test_01.cnf
#
sed "s|__VTD_SHARE__|$VTD_DATA|g" \
     $VTD_ROOT/share/vtd_test_02_template.cnf > \
     $VTD_ROOT/temp/vtd_test_02.cnf
#
sed "s|__VTD_SHARE__|$VTD_DATA|g" \
     $VTD_ROOT/share/vtd_test_03_template.cnf > \
     $VTD_ROOT/temp/vtd_test_03.cnf
#
sed "s|__VTD_SHARE__|$VTD_DATA|g" \
     $VTD_ROOT/share/vtd_example_04.com > \
     $VTD_ROOT/temp/vtd_example_04.com
#
set out_file = ${VTD_ROOT}/Makefile
source ${PETOOLS_DIR}/bin/petools_vars
#
if ( "$PETOOLS_VERSION" < "$PETOOLS_VERSION_MIN" ) then
     echo "Eeeeh. You have petools version $PETOOLS_VERSION, but "
     echo "$PETOOLS_VERSION_MIN is needed. Please upgrade"
     echo "You can download petools from http://astrogeo.org/petools"
     exit 1
endif
cat scripts/vtd_local_templ.py | \
    sed "s@%%date%%@$date_stamp@g" | \
    sed "s@%%host%%@$host_name@g"  | \
    sed "s@%%vtd_prefix%%@$VTD_PREFIX@g" | \
    sed "s@%%vtd_root%%@$VTD_ROOT@g" | \
    sed "s@%%vtd_data%%@$VTD_DATA@g"     | \
    sed "s@%%doc_path%%@$VTD_ROOT/doc@g" | \
    sed "s@%%ners_prefix%%@$VTD_NERS_DIR@g" > \
    scripts/vtd_local.py 
#
set DATE_ISO  = `date "+%Y.%m.%d_%H:%M:%S"`
set vtd_version_len     = `echo $VTD_VERSION    | awk '{print length($1)}'`
set vtd_prefix_len      = `echo $VTD_PREFIX     | awk '{print length($1)}'`
set vtd_root_len        = `echo $VTD_ROOT       | awk '{print length($1)}'`
set vtd_data_len        = `echo $VTD_DATA       | awk '{print length($1)}'`
set vtd_doc_path_len    = `echo $VTD_ROOT/doc   | awk '{print length($1)}'`
cat include/vtd_local.templ | \
    sed "s@%%DATE@$DATE_ISO@g"                     | \
    sed "s@%%vtd_version_len%%@$vtd_version_len@g" | \
    sed "s@%%vtd_prefix_len%%@$vtd_prefix_len@g"   | \
    sed "s@%%vtd_root_len%%@$vtd_root_len@g"       | \
    sed "s@%%vtd_data_len%%@$vtd_data_len@g"       | \
    sed "s@%%vtd_doc_len%%@$vtd_doc_path_len@g"    | \
    sed "s@%%vtd_version_val%%@$VTD_VERSION@g"     | \
    sed "s@%%vtd_prefix_val%%@$VTD_PREFIX@g"       | \
    sed "s@%%vtd_root_val%%@$VTD_ROOT@g"           | \
    sed "s@%%vtd_data_val%%@$VTD_DATA@g"           | \
    sed "s@%%vtd_doc_val%%@$VTD_ROOT/doc@g" >        \
    include/vtd_local.i
#
# ---  Remove path to include file in the installation directory
# ---  in order to prevent possible conflicts between the current
# ---  include files and include files from previos installation
#
setenv MK5_F95     `echo $MK5_F95 | sed "s@-I $PETOOLS_ROOT/include@@g"`" -I $VTD_ROOT/include"
setenv MK5_F95_OPT `echo $MK5_F95_OPT | sed "s@-I $PETOOLS_ROOT/include@@g"`" -I $VTD_ROOT/include"
#
if ( `uname` == "Linux" ) then
     set num_threads     = `cat /proc/cpuinfo | grep "core id" | wc -l`
     set num_thr_per_cpu = `lscpu | grep '^Thread(s) per core:' | awk '{print $4}'`
     set num_cores = `expr $num_threads / $num_thr_per_cpu`
else
     set num_cores = `sysctl -n  machdep.cpu.core_count`
endif
cat   ${VTD_ROOT}/Makefile.in1 > $out_file 
$ECHO "PETOOLS_ROOT      =      $PETOOLS_ROOT"            >> $out_file
$ECHO "PETOOLS_PREFIX    =      $PETOOLS_PREFIX"          >> $out_file
$ECHO "SPD_CLIENT_PREFIX =      $VTD_SPD_CLIENT_DIR"      >> $out_file
$ECHO "PETOOLS_LIB       = ${qt}$PETOOLS_LIB${qt}"        >> $out_file
$ECHO "SPD_CLIENT_LIB    =      $VTD_SPD_CLIENT_DIR/lib"  >> $out_file
$ECHO "NERS_LIB          =      $VTD_NERS_DIR/lib"        >> $out_file
$ECHO "NERS_INC          =      $VTD_NERS_DIR/include"    >> $out_file
$ECHO "SOLVE_LIB_PGPLOT  = ${qt}$SOLVE_LIB_PGPLOT${qt}"   >> $out_file
$ECHO "SOLVE_LIB_X11     = ${qt}$SOLVE_LIB_X11${qt}"      >> $out_file
$ECHO "SOLVE_LIB_XT      = ${qt}$SOLVE_LIB_XT${qt}"       >> $out_file
$ECHO "SOLVE_LIB_XHP11   = ${qt}$SOLVE_LIB_XHP11${qt}"    >> $out_file
$ECHO "SOLVE_EXTRA_LIB   = ${qt}$SOLVE_EXTRA_LIB${qt}"    >> $out_file
$ECHO "SOLVE_LIB_VEC     = ${qt}$SOLVE_LIB_VEC${qt}"      >> $out_file
$ECHO "SOLVE_LIB_BLAS    = ${qt}$SOLVE_LIB_BLAS${qt}"     >> $out_file
$ECHO "MK5_C             = ${qt}$MK5_C${qt}"              >> $out_file
if ( $VTD_NOOPT == "NO" ) then 
     $ECHO "MK5_F95_OPT       = ${qt}$MK5_F95_OPT${qt}"        >> $out_file
     $ECHO "MK5_F95           = ${qt}$MK5_F95${qt}"            >> $out_file
  else
     $ECHO "MK5_F95_OPT       = ${qt}$MK5_F95_NOOPT${qt}"      >> $out_file
     $ECHO "MK5_F95           = ${qt}$MK5_F95_NOOPT${qt}"      >> $out_file
endif
$ECHO "MK5_LINK          = ${qt}$MK5_LINK${qt}"           >> $out_file
$ECHO "MK5_C_LINK        = ${qt}$MK5_C_LINK${qt}"         >> $out_file
$ECHO "VTD_ROOT          =      $VTD_ROOT"                >> $out_file
$ECHO "VTD_PREFIX        =      $VTD_PREFIX"              >> $out_file
$ECHO "VTD_BIN           =      $VTD_PREFIX/bin"          >> $out_file
$ECHO "VTD_INC           =      $VTD_PREFIX/include"      >> $out_file
$ECHO "VTD_LIB           =      $VTD_PREFIX/lib"          >> $out_file
$ECHO "VTD_DATA          =      $VTD_DATA"                >> $out_file
$ECHO "VTD_CFITSIO_LIB   = ${qt}$VTD_CFITSIO_LIB${qt}"    >> $out_file
$ECHO "VTD_FITSLIB_LIB   = ${qt}$VTD_FITSLIB_LIB${qt}"    >> $out_file
$ECHO "VTD_CURL_DIR      = $VTD_CURL_DIR"                 >> $out_file
$ECHO "CURL_LIB          = ${qt}$CURL_LIB${qt}"           >> $out_file
$ECHO "CURL_INC          = ${qt}$CURL_INC${qt}"           >> $out_file
$ECHO "VTD_OPT_NOSTRUC   = ${qt}$VTD_OPT_NOSTRUC${qt}"    >> $out_file
$ECHO "VTD_OS            = $VTD_OS"                       >> $out_file
$ECHO "VTD_VERSION       = $VTD_VERSION"                  >> $out_file
$ECHO "NUM_PROC          = $num_cores"                    >> $out_file
if ( $VTD_CFITSIO_DIR == "" ) then
     $ECHO "VTD_LIB_EXAMPLE   = ${qt}$VTD_ROOT/src/libvtd.a $VTD_ROOT/fitslib/libfitslib.a ${qt}" >> $out_file
     $ECHO "VTD_CFITSIO_DIR   = ${qt}${qt}"               >> $out_file
  else
     $ECHO "VTD_LIB_EXAMPLE   = ${qt}$VTD_ROOT/src/libvtd.a $VTD_ROOT/fitslib/libfitslib.a -L$VTD_CFITSIO_DIR/lib -lcfitsio $CURL_LIB${qt}" >> $out_file
     $ECHO "VTD_CFITSIO_DIR   =      $VTD_CFITSIO_DIR"         >> $out_file
endif
$ECHO "CONF_LOG          = $CONF_LOG"                     >> $out_file
$ECHO "BUILD_LOG         = $BUILD_LOG"                    >> $out_file
cat   ${VTD_ROOT}/Makefile.in2                            >> $out_file
#
set out_vars = ${VTD_ROOT}/support/vtd_vars
if ( -f $out_vars ) rm -f $out_vars
$ECHO "#\!/bin/csh"                                            > $out_vars
$ECHO "setenv PETOOLS_ROOT          $PETOOLS_ROOT"            >> $out_vars
$ECHO "setenv PETOOLS_PREFIX        $PETOOLS_PREFIX"          >> $out_vars
$ECHO "setenv SPD_CLIENT_PREFIX     $VTD_SPD_CLIENT_DIR"      >> $out_vars
$ECHO "setenv PETOOLS_LIB      ${qt}$PETOOLS_LIB${qt}"        >> $out_vars
$ECHO "setenv SPD_CLIENT_LIB        $VTD_SPD_CLIENT_DIR/lib"  >> $out_vars
$ECHO "setenv NERS_LIB              $VTD_NERS_DIR/lib"        >> $out_vars
$ECHO "setenv NERS_INC              $VTD_NERS_DIR/include"    >> $out_vars
$ECHO "setenv SOLVE_LIB_PGPLOT ${qt}$SOLVE_LIB_PGPLOT${qt}"   >> $out_vars
$ECHO "setenv SOLVE_LIB_X11    ${qt}$SOLVE_LIB_X11${qt}"      >> $out_vars
$ECHO "setenv SOLVE_LIB_XT     ${qt}$SOLVE_LIB_XT${qt}"       >> $out_vars
$ECHO "setenv SOLVE_LIB_XHP11  ${qt}$SOLVE_LIB_XHP11${qt}"    >> $out_vars
$ECHO "setenv SOLVE_EXTRA_LIB  ${qt}$SOLVE_EXTRA_LIB${qt}"    >> $out_vars
$ECHO "setenv SOLVE_LIB_VEC    ${qt}$SOLVE_LIB_VEC${qt}"      >> $out_vars
$ECHO "setenv SOLVE_LIB_BLAS   ${qt}$SOLVE_LIB_BLAS${qt}"     >> $out_vars
$ECHO "setenv MK5_C            ${qt}$MK5_C${qt}"              >> $out_vars
$ECHO "setenv MK5_LINK         ${qt}$MK5_LINK${qt}"           >> $out_vars
$ECHO "setenv MK5_C_LINK       ${qt}$MK5_C_LINK${qt}"         >> $out_vars
if ( $VTD_NOOPT == "NO" ) then 
     $ECHO "setenv MK5_F95     ${qt}$MK5_F95${qt}"            >> $out_vars
     $ECHO "setenv MK5_F95_OPT ${qt}$MK5_F95_OPT${qt}"        >> $out_vars
  else
     $ECHO "setenv MK5_F95     ${qt}$MK5_F95_NOOPT${qt}"      >> $out_vars
     $ECHO "setenv MK5_F95_OPT ${qt}$MK5_F95_NOOPT${qt}"      >> $out_vars
endif
$ECHO "setenv VTD_ROOT              $VTD_ROOT"                >> $out_vars
$ECHO "setenv VTD_PREFIX            $VTD_PREFIX"              >> $out_vars
$ECHO "setenv VTD_BIN               $VTD_PREFIX/bin"          >> $out_vars
$ECHO "setenv VTD_INC               $VTD_PREFIX/include"      >> $out_vars
$ECHO "setenv VTD_LIB               $VTD_PREFIX/lib"          >> $out_vars
$ECHO "setenv VTD_DATA              $VTD_DATA"                >> $out_vars
$ECHO "setenv VTD_CFITSIO_LIB  ${qt}$VTD_CFITSIO_LIB${qt}"    >> $out_vars
$ECHO "setenv VTD_FITSLIB_LIB  ${qt}$VTD_FITSLIB_LIB${qt}"    >> $out_vars
$ECHO "setenv CURL_LIB         ${qt}$CURL_LIB${qt}"           >> $out_vars
$ECHO "setenv CURL_INC         ${qt}$CURL_INC${qt}"           >> $out_vars
$ECHO "setenv VTD_OPT_NOSTRUC  ${qt}$VTD_OPT_NOSTRUC${qt}"    >> $out_vars
$ECHO "setenv VTD_OS           $VTD_OS"                       >> $out_vars
$ECHO "setenv VTD_VERSION      $VTD_VERSION"                  >> $out_vars
if ( $VTD_CFITSIO_DIR == "" ) then
     $ECHO "setenv VTD_LIB_EXAMPLE ${qt}$VTD_ROOT/src/libvtd.a${qt}" >> $out_vars
     $ECHO "setenv VTD_CFITSIO_DIR ${qt}${qt}"                >> $out_vars
  else
     $ECHO "setenv VTD_LIB_EXAMPLE ${qt}$VTD_ROOT/src/libvtd.a $VTD_ROOT/fitslib/libfitslib.a -L$VTD_CFITSIO_DIR/lib -lcfitsio${qt}" >> $out_vars
     $ECHO "setenv VTD_CFITSIO_DIR       $VTD_CFITSIO_DIR"    >> $out_vars
endif
$ECHO "umask 0022"                                            >> $out_vars
#
source ${VTD_ROOT}/support/vtd_vars
#
# --- Create a file with export environoment variables
#
set export_vars = ${VTD_ROOT}/support/export_vtd_vars
if ( -f $export_vars  ) rm -f $export_vars 
$ECHO "#\!/bin/csh"                               > $export_vars
$ECHO "setenv VTD_PREFIX    $VTD_PREFIX"         >> $export_vars
if ( $VTD_CFITSIO_DIR == "" ) then
     $ECHO "setenv VTD_CFITSIO_DIR ${qt}${qt}"                     >> $export_vars
     $ECHO "setenv VTD_LIB      ${qt}-L$VTD_PREFIX/lib -lvtd${qt}" >> $export_vars
   else
     $ECHO "setenv VTD_CFITSIO_PREFIX $VTD_CFITSIO_DIR"               >> $export_vars
     $ECHO "setenv VTD_LIB      ${qt}-L$VTD_PREFIX/lib -lvtd -lfitslib -L$VTD_CFITSIO_DIR/lib -lcfitsio${qt}" >> $export_vars
endif
$ECHO "setenv VTD_INCLUDE   $VTD_PREFIX/include" >> $export_vars
$ECHO "setenv VTD_DATA      $VTD_DATA"           >> $export_vars
$ECHO "setenv VTD_VERSION   $VTD_VERSION"        >> $export_vars
#
# --- Transform csh defintion file into bash defintion file
#
cat ${VTD_ROOT}/support/vtd_vars | \
    awk '{ printf "export %s=", $2; for (i = 3; i <= NF; i++) printf ("%s ",$i); printf ("\n") }' | \
    sed  "s@export =@#\!/bin/bash@" > \
    ${VTD_ROOT}/support/vtd_vars.sh
cat ${VTD_ROOT}/support/export_vtd_vars | \
    awk '{ printf "export %s=", $2; for (i = 3; i <= NF; i++) printf ("%s ",$i); printf ("\n") }' | \
    sed  "s@export =@#\!/bin/bash@" > \
    ${VTD_ROOT}/support/export_vtd_vars.sh
#
if ( `uname` == "SunOS" ) then
      set pwd_save = `pwd`
      cd  $SUPPORT_PATH 
      make -f sun_preproc.mak clean
      make -f sun_preproc.mak 
      set status_make = $status
      cd $pwd_save
      if ( $status_make != 0 ) then
           echo "Error in compiling/linking sun_preproc"
	   exit 1
      endif
endif
$ECHO "Remove stale include files, if present... \c" | tee -a  $CONF_LOG
make uninstall_include >>& $CONF_LOG
chmod g+rw,o+r ${VTD_ROOT}/Makefile
chmod g+rw,o+r ${VTD_ROOT}/support/vtd_vars
chmod g+rw,o+r ${VTD_ROOT}/support/export_vtd_vars
chmod g+rw,o+r ${VTD_ROOT}/support/vtd_vars.sh
chmod g+rw,o+r ${VTD_ROOT}/support/export_vtd_vars.sh
echo "ok" | tee -a  $CONF_LOG
#
echo "config.csh is done"
