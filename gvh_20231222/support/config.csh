#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Script  config.csh  sets numerous variables needed for             *
# *   compilation and linking gvh. It creates files with directives      *
# *   for FORTRAN-compilers and C-compiler. It creates include files     *
# *   using template files for includes and local files with             *
# *   preferences.                                                       *
# *                                                                      *
# *   Usage:  config.csh                                                 *
# *                                                                      *
# *  ###  15-JAN-2011  config.csh  v1.6  (c)  L. Petrov  15-MAY-2019 ### *
# *                                                                      *
# ************************************************************************
#
setenv SUPPORT_PATH `dirname $0`
cd     $SUPPORT_PATH
cd ../
if (    -f ${GVH_ROOT}/Makefile ) then
     rm -f ${GVH_ROOT}/Makefile
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
     set qe = "\051"
     breaksw
endsw
#
if ( -d $PETOOLS_DIR == 0 ) then
     echo "GVH config: petools directory $PETOOLS_DIR was not found"
     exit 1
endif
if ( -d $PETOOLS_DIR/lib == 0 ) then
     echo "GVH config: petools directory $PETOOLS_DIR/lib was not found"
     exit 1
endif
if ( -d $PETOOLS_DIR/bin == 0 ) then
     echo "GVH config: petools directory $PETOOLS_DIR/bin was not found"
     exit 1
endif
if ( -f $PETOOLS_DIR/lib/libpetools.a == 0 ) then
     echo "GVH config: petools file $PETOOLS_DIR/lib/libpetools.a  was not found"
     exit 1
endif
source $PETOOLS_DIR/bin/petools_vars
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
# --- Creation of ./include/gvh.i
#
if ( $PETOOLS_BITS == 64 ) then
     cat $GVH_ROOT/include/gvh_i.templ | \
         sed 's@%L1@120@g' | \
         sed 's@%L2@136@g' | \
         sed 's@ADDRESS__TYPE@INTEGER*8    @g' > \
         $GVH_ROOT/include/gvh.i
  else 
     cat $GVH_ROOT/include/gvh_i.templ | \
         sed 's@%L1@116@g' | \
         sed 's@%L2@128@g' | \
         sed 's@ADDRESS__TYPE@INTEGER*4    @g' > \
         $GVH_ROOT/include/gvh.i
endif
#
set out_file = ${GVH_ROOT}/Makefile
source ${PETOOLS_DIR}/bin/petools_vars
#
# ---  Remove path to include file in the installation directory
# ---  in order to prevent possible conflicts between the current
# ---  include files and include files from previos installation
#
setenv MK5_F95     `echo $MK5_F95 | sed "s@-I $PETOOLS_ROOT/include@@g"`
setenv MK5_F95_OPT `echo $MK5_F95_OPT | sed "s@-I $PETOOLS_ROOT/include@@g"`
#
if ( "$PETOOLS_VERSION" < "$PETOOLS_VERSION_MIN" ) then
     echo "Eeeeh. You have petools version $PETOOLS_VERSION, but "
     echo "$PETOOLS_VERSION_MIN is needed. Please upgrade"
     echo "You can download petools from http://astrogeo.org/petools"
     exit 1
endif
#
set DATE_LONG = `date "+%d-%h-%Y %H:%M:%S" | tr "[a-z]" "[A-Z]"`
set GVH__ROOT_LEN    = `echo $GVH_ROOT    | awk '{print length($1)}'`
set GVH__VERSION_LEN = `echo $GVH_VERSION | awk '{print length($1)}'`
cat <<EOF > ${GVH_ROOT}/include/gvh_local.i
!
! --- Local customization of GVH
! --- This file was created automaticly on $DATE_LONG
! 
      CHARACTER    GVH__ROOT*$GVH__ROOT_LEN, GVH__VERSION*$GVH__VERSION_LEN
      PARAMETER  ( GVH__ROOT    = '$GVH_ROOT' )
      PARAMETER  ( GVH__VERSION = '$GVH_VERSION' )
EOF
cat $GVH_ROOT/gvh/gvh_version_template.f | \
    sed "s/@GVH_VERSION_STR@/$GVH_VERSION/g" > \
    $GVH_ROOT/gvh/gvh_version.f
#
if ( `uname` == "Linux" ) then
     set num_threads     = `cat /proc/cpuinfo | grep "core id" | wc -l`
     set num_thr_per_cpu = `lscpu | grep '^Thread(s) per core:' | awk '{print $4}'`
     set num_cores = `expr $num_threads / $num_thr_per_cpu`
else
     set num_cores = `sysctl -n  machdep.cpu.core_count`
endif
cat   ${GVH_ROOT}/Makefile.in1 > $out_file 
$ECHO "PETOOLS_ROOT     = ${qt}$PETOOLS_ROOT${qt}"       >> $out_file
$ECHO "PETOOLS_PREFIX   = ${qt}$PETOOLS_PREFIX${qt}"     >> $out_file
$ECHO "PETOOLS_LIB      = ${qt}$PETOOLS_LIB${qt}"        >> $out_file
$ECHO "VTD_LIB          = ${qt}$VTD_LIB${qt}"            >> $out_file
$ECHO "NERS_PREFIX      = $NERS_PREFIX"                  >> $out_file
$ECHO "NERS_LIB         = -L $NERS_PREFIX/lib -lners"    >> $out_file
$ECHO "NERS_INC         = $NERS_PREFIX/include"          >> $out_file
$ECHO "SOLVE_EXTRA_LIB  = ${qt}$SOLVE_EXTRA_LIB${qt}"    >> $out_file
$ECHO "SOLVE_LIB_VEC    = ${qt}$SOLVE_LIB_VEC${qt}"      >> $out_file
$ECHO "SOLVE_LIB_BLAS   = ${qt}$SOLVE_LIB_BLAS${qt}"     >> $out_file
$ECHO "MK5_C            = ${qt}$MK5_C${qt}"              >> $out_file
$ECHO "MK5_C_LINK       = ${qt}$MK5_C_LINK${qt}"         >> $out_file
if ( $GVH_NOOPT == "NO" ) then 
     $ECHO "MK5_F95_OPT       = ${qt}$MK5_F95_OPT${qt}"        >> $out_file
     $ECHO "MK5_F95           = ${qt}$MK5_F95${qt}"            >> $out_file
  else
     $ECHO "MK5_F95_OPT       = ${qt}$MK5_F95_NOOPT${qt}"      >> $out_file
     $ECHO "MK5_F95           = ${qt}$MK5_F95_NOOPT${qt}"      >> $out_file
endif
$ECHO "MK5_LINK         = ${qt}$MK5_LINK${qt}"           >> $out_file
$ECHO "GVH_ROOT         = $GVH_ROOT"                     >> $out_file
$ECHO "GVH_PREFIX       = $GVH_PREFIX"                   >> $out_file
$ECHO "GVH_BIN          = $GVH_PREFIX/bin"               >> $out_file
$ECHO "GVH_INC          = $GVH_PREFIX/include"           >> $out_file
$ECHO "GVH_LIB          = $GVH_PREFIX/lib"               >> $out_file
$ECHO "GVH_OS           = $GVH_OS"                       >> $out_file
$ECHO "GVH_VERSION      = $GVH_VERSION"                  >> $out_file
$ECHO "NUM_PROC         = $num_cores"                    >> $out_file
$ECHO "CONF_LOG         = $CONF_LOG"                     >> $out_file
$ECHO "BUILD_LOG        = $BUILD_LOG"                    >> $out_file
cat   ${GVH_ROOT}/Makefile.in2                           >> $out_file
#
set out_vars = ${GVH_ROOT}/support/gvh_vars
if ( -f $out_vars ) rm -f $out_vars
$ECHO "#\!/bin/csh"                                            > $out_vars
$ECHO "setenv PETOOLS_ROOT     ${qt}$PETOOLS_ROOT${qt}"       >> $out_vars
$ECHO "setenv PETOOLS_PREFIX   ${qt}$PETOOLS_PREFIX${qt}"     >> $out_vars
$ECHO "setenv PETOOLS_LIB      ${qt}$PETOOLS_LIB${qt}"        >> $out_vars
$ECHO "setenv VTD_LIB          ${qt}$VTD_LIB${qt}"            >> $out_vars
$ECHO "setenv NERS_LIB         ${qt}-L $NERS_PREFIX/lib -lners${qt}" >> $out_vars
$ECHO "setenv NERS_INC         $NERS_PREFIX/include"          >> $out_vars
$ECHO "setenv SOLVE_EXTRA_LIB  ${qt}$SOLVE_EXTRA_LIB${qt}"    >> $out_vars
$ECHO "setenv SOLVE_LIB_VEC    ${qt}$SOLVE_LIB_VEC${qt}"      >> $out_vars
$ECHO "setenv SOLVE_LIB_BLAS   ${qt}$SOLVE_LIB_BLAS${qt}"     >> $out_vars
$ECHO "setenv MK5_C            ${qt}$MK5_C${qt}"              >> $out_vars
$ECHO "setenv MK5_C_LINK       ${qt}$MK5_C_LINK${qt}"         >> $out_vars
if ( $GVH_NOOPT == "NO" ) then 
     $ECHO "setenv MK5_F95_OPT      ${qt}$MK5_F95_OPT${qt}"        >> $out_vars
     $ECHO "setenv MK5_F95          ${qt}$MK5_F95${qt}"            >> $out_vars
  else
     $ECHO "setenv MK5_F95_OPT      ${qt}$MK5_F95_NOOPT${qt}"      >> $out_vars
     $ECHO "setenv MK5_F95          ${qt}$MK5_F95_NOOPT${qt}"      >> $out_vars
endif
$ECHO "setenv MK5_LINK         ${qt}$MK5_LINK${qt}"           >> $out_vars
$ECHO "setenv GVH_ROOT         $GVH_ROOT"                     >> $out_vars
$ECHO "setenv GVH_PREFIX       $GVH_PREFIX"                   >> $out_vars
$ECHO "setenv GVH_BIN          $GVH_PREFIX/bin"               >> $out_vars
$ECHO "setenv GVH_INC          $GVH_PREFIX/include"           >> $out_vars
$ECHO "setenv GVH_LIB          $GVH_PREFIX/lib"               >> $out_vars
$ECHO "setenv GVH_OS           $GVH_OS"                       >> $out_vars
$ECHO "setenv GVH_VERSION      $GVH_VERSION"                  >> $out_vars
#
source ${GVH_ROOT}/support/gvh_vars
#
set export_vars = ${GVH_ROOT}/support/export_gvh_vars
if ( -f $export_vars  ) rm -f $export_vars 
$ECHO "#\!/bin/csh"                               > $export_vars
$ECHO "setenv GVH_PREFIX    $GVH_PREFIX"         >> $export_vars
$ECHO "setenv GVH_LIB      ${qt}-L$GVH_PREFIX/lib -lgvh -lvcat${qt}" >> $export_vars
$ECHO "setenv GVH_INCLUDE   $GVH_PREFIX/include" >> $export_vars
$ECHO "setenv GVH_VERSION   $GVH_VERSION"        >> $export_vars
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
chmod g+rw,o+r ${GVH_ROOT}/Makefile
chmod g+rw,o+r ${GVH_ROOT}/support/gvh_vars
chmod g+rw,o+r ${GVH_ROOT}/support/export_gvh_vars
#
echo "config.csh is done"
