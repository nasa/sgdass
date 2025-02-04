#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Script  config.csh  sets numerous variables needed for             *
# *   compilation and linking spd. It creates files with directives      *
# *   for FORTRAN-compilers and C-compiler. It creates include files     *
# *   using template files for includes and local files with             *
# *   preferences.                                                       *
# *                                                                      *
# *   Usage:  config.csh                                                 *
# *                                                                      *
# *  ###  18-JAN-2008  config.csh  v2.3  (c)  L. Petrov  17-NOV-2016 ### *
# *                                                                      *
# ************************************************************************
#
setenv SUPPORT_PATH `dirname $0`
cd     $SUPPORT_PATH
cd ../
setenv CONF_LOG   ${SPD_ROOT}/temp/conf.log
if (    -f ${SPD_ROOT}/Makefile ) then
     rm -f ${SPD_ROOT}/Makefile
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
     setenv SPD_SOLIB  libspd.so.1
     setenv MALO_SOLIB libmalo.so.1
     breaksw
   case "Darwin":
     set ECHO = "/bin/echo"
     set qt = '"'
     setenv SPD_SOLIB libspd.1.dylib
     setenv MALO_SOLIB libmalo.1.dylib
     breaksw
endsw
#
if ( -d $SPD_PETOOLS == 0 ) then
     echo "spd config: petools directory $SPD_PETOOLS was not found"
     exit 1
endif
if ( -d $SPD_PETOOLS/lib == 0 ) then
     echo "spd config: petools directory $SPD_PETOOLS/lib was not found"
     exit 1
endif
if ( -d $SPD_PETOOLS/bin == 0 ) then
     echo "spd config: petools directory $SPD_PETOOLS/bin was not found"
     exit 1
endif
if ( -f $SPD_PETOOLS/lib/libpetools.a  == 0 ) then
     echo "spd config: petools file $SPD_PETOOLS/lib/libpetools.a  was not found"
     exit 1
endif
#
# --- Check fourpack
#
if ( -d $SPD_FOURPACK == 0 ) then
     echo "spd config: fourpack directory $SPD_FOURPACK was not found"
     exit 1
endif
if ( -d $SPD_FOURPACK/lib == 0 ) then
     echo "spd config: fourpack directory $SPD_FOURPACK/lib was not found"
     exit 1
endif
if ( -d $SPD_FOURPACK/bin == 0 ) then
     echo "spd config: fourpack directory $SPD_FOURPACK/bin was not found"
     exit 1
endif
if ( -f $SPD_FOURPACK/bin/fourpack_vars ) then
      source $SPD_FOURPACK/bin/fourpack_vars 
      if ( $FOURPACK_VERSION < $FOURPACK_VERSION_MIN ) then
           echo "spd config: you have too old library fourpack version $FOURPACK_VERSION"
           echo "spd config: while $FOURPACK_VERSION_MIN version is needed. Please upgrade"
           exit 1
      endif
endif
#
# --- Check vtd
#
if ( -d $SPD_VTD == 0 ) then
     echo "spd config: vtd directory $SPD_VTD was not found"
     exit 1
endif
if ( -d $SPD_VTD/lib == 0 ) then
     echo "spd config: vtd directory $SPD_VTD/lib was not found"
     exit 1
endif
if ( -d $SPD_VTD/bin == 0 ) then
     echo "spd config: vtd directory $SPD_VTD/bin was not found"
     exit 1
endif
if ( -f $SPD_VTD/bin/vtd_vars ) then
      source $SPD_VTD/bin/vtd_vars 
      if ( $VTD_VERSION < $VTD_VERSION_MIN ) then
           echo "spd config: you have too old vtd version $VTD_VERSION"
           echo "spd config: while $VTD_VERSION_MIN version is needed. Please upgrade"
           exit 1
      endif
  else
     echo "spd config: vtd file $SPD_VTD/bin/vtd_vars was not found"
     exit 1
endif
if ( -d  $SPD_NERS == 0 ) then
     echo "spd config: ners install directory $SPD_NERS was not found"
     echo "spd config: Please install ners"
     exit 1
endif
#
if ( -d  $SPD_NERS/include == 0 ) then
     echo "spd config: ners include directory $SPD_NERS/include was not found"
     echo "spd config: Please install ners"
     exit 1
endif
#
if ( -f  $SPD_NERS/include/ners_local.i == 0 ) then
     echo "spd config: ners version file $SPD_NERS/include/ners_local.i was not found"
     echo "spd config: Please install ners"
     exit 1
endif
set SPD_NERS_VERSION = `cat $SPD_NERS/include/ners_local.i | grep "NERS__VERSION =" | awk '{print substr($5,2,8)}'`
if ( "$SPD_NERS_VERSION" < "$NERS_VERSION_MIN" ) then
     echo "Eeeeh. You have ners version $SPD_NERS_VERSION, but $NERS_VERSION_MIN is needed."
     echo "Please upgrade."
     echo "You can download the new version of ners from http://earthrotation.net/ners"
     exit 1
endif
set SPD_NERS_STANDALONE = `cat $SPD_NERS/include/ners_local.i | grep "NERS__NERS_STANDALONE = " | awk '{print substr($5,2,8)}' | sed 's@"@@g'`
if ( "$SPD_NERS_STANDALONE" == "YES" ) then
     echo "spd config: ners package was configured without petools"
     echo "spd config: Please re-configure and re-install ners with petools support"
     exit 1
endif
#
# --- Check cfitsio
#
if ( $OS_name == "Linux" && ! -f $SPD_CFITSIO/lib/libcfitsio.so ) then
     echo "spd spd config: cfitsio library $SPD_CFITSIO/lib/libcfitsio.so was not found"
     exit 1
  else if ( $OS_name == "Linux" && ! -f $SPD_CFITSIO/lib/libcfitsio.so ) then
     echo "spd spd config: cfitsio library $SPD_CFITSIO/lib/libcfitsio.dylib was not found"
     exit 1
endif
#
# --- Check MALO
#
if ( -d $SPD_MALO == 0 ) then
     echo "spd config: malo directory $MALO_NETCDF was not found"
     exit 1
endif
#
if ( -f $SPD_MALO/include/heb.i == 0 ) then
     echo "spd config: malo was not properly installed"
     exit 1
endif
#
if ( -f $SPD_MALO/lib/$MALO_SOLIB == 0 ) then
     echo "spd config: malo was not properly installed"
     echo "$SPD_MALO/lib/$MALO_SOLIB "
     exit 1
endif
#
set malo_version = `cat $SPD_MALO/bin/malo_vars | grep MALO_VERSION | awk '{print $3}'`
if ( $malo_version < $MALO_VERSION_MIN ) then
     echo "spd config: you have too old malo library version $MALO_VERSION"
     echo "spd config: while $MALO_VERSION_MIN version is required. Please upgrade."
     exit 1
endif
#
# --- Check necdf
#
if ( -d $SPD_NETCDF == 0 ) then
     echo "spd config: netcdf directory $SPD_NETCDF was not found"
     exit 1
endif
if ( -f ${SPD_NETCDF}/include/netcdf.h == 0 ) then
     echo "spd config: netcdf include file $SPD_NETCDF/include/netcdf.h was not found"
     exit 1
endif
if ( -f ${SPD_NETCDF}/lib/libnetcdf.a ) then
     setenv SPD_NETCDF_LIB ${SPD_NETCDF}/lib
   else if ( -f ${SPD_NETCDF}/lib64/libnetcdf.a ) then
     setenv SPD_NETCDF_LIB ${SPD_NETCDF}/lib64
   else 
     echo "spd config: neither netcdf library file $SPD_NETCDF/lib/netcdf.a nor $SPD_NETCDF/lib74/netcdf.a was not found"
     exit 1
endif
#
if ( -d $SPD_HDF4 == 0 ) then
     echo "spd config: hdf4 directory $SPD_HDF4 was not found"
     exit 1
endif
if ( -d $SPD_HDF5 == 0 ) then
     echo "spd config: hdf5 directory $SPD_HDF4 was not found"
     exit 1
endif
if ( -f ${SPD_HDF4}/include/hdf.inc     == 0 && \
     -f ${SPD_HDF4}/include/hdf/hdf.inc == 0    ) then
     echo "spd config: neither hdf4 include file ${SPD_NETCDF}/include/hdf.inc nor ${SPD_NETCDF}/include/hdf/hdf.inc were not found"
     exit 1
endif
if ( -f ${SPD_HDF4}/lib/libmfhdf.a ) then
     setenv SPD_HDF4_LIB ${SPD_HDF4}/lib
   else if ( -f ${SPD_HDF4}/lib64/libmfhdf.a ) then
     setenv SPD_HDF4_LIB ${SPD_HDF4}/lib64
   else 
     echo "spd config: hdf4 library files ${SPD_HDF4}/lib/libmfhdf.a or ${SPD_HDF4}/lib64/libmfhdf.a  were not found"
     exit 1
endif
if ( -f ${SPD_HDF5}/lib/libhdf5.a ) then
     setenv SPD_HDF5_LIB ${SPD_HDF5}/lib
   else if ( -f ${SPD_HDF5}/lib64/libhdf5.a ) then
     setenv SPD_HDF5_LIB ${SPD_HDF5}/lib64
   else 
     echo "spd config: hdf5 library files ${SPD_HDF5}/lib/libhdf5.a or ${SPD_HDF5}/lib64/libhdf5_hl.a  were not found"
     exit 1
endif
#
# --- Check whether make is found in your system
#
set gmake_string = `which make`
if ( "$gmake_string" == "" ) then 
     echo "make was not found in your system."
     exit 1
endif
#
set DATE_LONG = `date "+%d-%h-%Y %H:%M:%S" | tr "[a-z]" "[A-Z]"`
set SPD_ROOT_LEN    = `echo $SPD_ROOT              | awk '{print length($1)}'`
set SPD_PREFIX_LEN  = `echo $SPD_PREFIX            | awk '{print length($1)}'`
set SPD_DOC_LEN     = `echo $SPD_PREFIX/spd/doc    | awk '{print length($1)}'`
set SPD_SHARE_LEN   = `echo $SPD_PREFIX/spd/share  | awk '{print length($1)}'`
set SPD_SCRIPT_LEN  = `echo $SPD_PREFIX/spd/script | awk '{print length($1)}'`
set SPD_VERSION_LEN = `echo $SPD_VERSION           | awk '{print length($1)}'`
#
# --- Create an include file with local customizations
#
cat <<EOF > ${SPD_ROOT}/include/spd_local.i
!
! --- Local customization of SPD
! --- This file was created automaticly on $DATE_LONG
! 
      CHARACTER  SPD__ROOT*$SPD_ROOT_LEN, SPD__PREFIX*$SPD_PREFIX_LEN, SPD__SHARE*$SPD_SHARE_LEN, SPD__DOC*$SPD_DOC_LEN, SPD__SCRIPT*$SPD_SCRIPT_LEN, SPD__VERSION*$SPD_VERSION_LEN
      PARAMETER  ( SPD__ROOT    = '$SPD_ROOT' )
      PARAMETER  ( SPD__PREFIX  = '$SPD_PREFIX' )
      PARAMETER  ( SPD__DOC     = '$SPD_PREFIX/spd/doc'    )
      PARAMETER  ( SPD__SHARE   = '$SPD_PREFIX/spd/share'  )
      PARAMETER  ( SPD__SCRIPT  = '$SPD_PREFIX/spd/script' )
      PARAMETER  ( SPD__VERSION = '$SPD_VERSION' )
! 
! --- End of SPD local customization include block
EOF
#
set    date_stamp  = `date "+%Y.%m.%d-%H:%M:%S" | tr "[a-z]" "[A-Z]"`
set    host_name   = `uname -n`
#
set out_file = ${SPD_ROOT}/Makefile
source ${SPD_PETOOLS}/bin/petools_vars
#
if ( "$PETOOLS_VERSION" < "$PETOOLS_VERSION_MIN" ) then
     echo "Eeeeh. You have petools version $PETOOLS_VERSION, but "
     echo "$PETOOLS_VERSION_MIN is needed. Please upgrade"
     exit 1
endif
#
cat   ${SPD_ROOT}/Makefile.in1 > $out_file 
if ( `uname` == "Linux" ) then
     set num_proc = `cat /proc/cpuinfo | grep "cpu core" | wc -l`
     set num_proc = `expr $num_proc + 1`
  else 
     set num_proc = 1
endif
#
if ( `echo $MK5_F95 | grep GNU` != "" ) then
      setenv SPD_OPENMP "-fopenmp"
   else
      setenv SPD_OPENMP "-openmp"
endif
echo "" >> $out_file
echo "PETOOLS_NUM_PROC = $num_proc" >> $out_file
echo "" >> $out_file
if ( $SPD_USE_DIAGI == "NO" ) then
     setenv SPD_DIAGI "-DNO_DIAGI"
     setenv SPD_LIB_DIAGI " "
   else 
     setenv SPD_DIAGI " "
     setenv SPD_LIB_DIAGI $PETOOLS_ROOT/lib/diagi.a 		
endif 
#
$ECHO "SPD_MALO         = $SPD_MALO"                    >> $out_file
$ECHO "SPD_VTD          = $SPD_VTD"                     >> $out_file
$ECHO "SPD_NERSD        = $SPD_NERS"                    >> $out_file
$ECHO "SPD_CFITSIO      = $SPD_CFITSIO"                 >> $out_file
$ECHO "SPD_NETCDF       = SPD_NETCDF"                   >> $out_file
$ECHO "SPD_NETCDF_LIB   = $SPD_NETCDF_LIB"              >> $out_file
$ECHO "SPD_HDF4         = $SPD_HDF4"                    >> $out_file
$ECHO "SPD_HDF4_LIB     = $SPD_HDF4_LIB"                >> $out_file
$ECHO "SPD_HDF5         = $SPD_HDF5"                    >> $out_file
$ECHO "SPD_HDF5_LIB     = $SPD_HDF5_LIB"                >> $out_file
$ECHO "SPD_JPEG         = $SPD_JPEG"                    >> $out_file
$ECHO "SPD_Z            = $SPD_Z"                       >> $out_file
$ECHO "PETOOLS_PREFIX   = $PETOOLS_PREFIX"              >> $out_file
$ECHO "PETOOLS_LIB      = $PETOOLS_LIB"                 >> $out_file
$ECHO "SOLVE_EXTRA_LIB  = $SOLVE_EXTRA_LIB"             >> $out_file
$ECHO "SOLVE_LIB_VEC    = $SOLVE_LIB_VEC"               >> $out_file
$ECHO "SOLVE_LIB_BLAS   = $SOLVE_LIB_BLAS"              >> $out_file
$ECHO "SPD_FOURPACK     = $SPD_FOURPACK"                >> $out_file
$ECHO "MK5_C            = $MK5_C"                       >> $out_file
$ECHO "MK5_F95_NOOPT    = $MK5_F95_NOOPT"               >> $out_file
if ( $SPD_NOOPT == "NO" ) then
      $ECHO "MK5_F95_OPT      = $MK5_F95_OPT"           >> $out_file
      $ECHO "MK5_F95          = $MK5_F95"               >> $out_file
else
      $ECHO "MK5_F95_OPT      = $MK5_F95_NOOPT"         >> $out_file
      $ECHO "MK5_F95          = $MK5_F95_NOOPT"         >> $out_file
endif
$ECHO "MK5_LINK         = $MK5_LINK"                    >> $out_file
$ECHO "SPD_PREFIX       = $SPD_PREFIX"                  >> $out_file
$ECHO "SPD_ROOT         = $SPD_ROOT"                    >> $out_file
$ECHO "SPD_BIN          = $SPD_BIN"                     >> $out_file
$ECHO "SPD_OS           = $SPD_OS"                      >> $out_file
$ECHO "SPD_OPENMP       = $SPD_OPENMP"                  >> $out_file
$ECHO "SPD_USE_DIAGI    = $SPD_USE_DIAGI"               >> $out_file
$ECHO "SPD_DIAGI        = $SPD_DIAGI"                   >> $out_file
$ECHO "SPD_LIB_DIAGI    = $SPD_LIB_DIAGI"               >> $out_file
$ECHO "SPD_VERSION      = $SPD_VERSION"                 >> $out_file
cat ${SPD_ROOT}/Makefile.in2                            >> $out_file
#
set out_vars = ${SPD_ROOT}/support/spd_vars
if ( -f $out_vars ) rm -f $out_vars
$ECHO "#\!/bin/csh"                                            > $out_vars
$ECHO "source $PETOOLS_PREFIX/bin/petools_vars"               >> $out_vars
$ECHO "source $SPD_FOURPACK/bin/fourpack_vars"                >> $out_vars
$ECHO "setenv SPD_MALO         $SPD_MALO"                     >> $out_vars
$ECHO "setenv SPD_VTD          $SPD_VTD"                      >> $out_vars
$ECHO "setenv SPD_NERS         $SPD_NERS"                     >> $out_vars
$ECHO "setenv SPD_CFITSIO      $SPD_CFITSIO"                  >> $out_vars
$ECHO "setenv SPD_NETCDF       $SPD_NETCDF"                   >> $out_vars
$ECHO "setenv SPD_NETCDF_LIB   $SPD_NETCDF_LIB"               >> $out_vars
$ECHO "setenv SPD_HDF4         $SPD_HDF4"                     >> $out_vars
$ECHO "setenv SPD_HDF4_LIB     $SPD_HDF4_LIB"                 >> $out_vars
$ECHO "setenv SPD_HDF5         $SPD_HDF5"                     >> $out_vars
$ECHO "setenv SPD_HDF5_LIB     $SPD_HDF5_LIB"                 >> $out_vars
$ECHO "setenv SPD_JPEG         $SPD_JPEG"                     >> $out_vars
$ECHO "setenv SPD_Z            $SPD_Z"                        >> $out_vars
$ECHO "setenv PETOOLS_PREFIX   $PETOOLS_PREFIX"               >> $out_vars
$ECHO "setenv PETOOLS_LIB      ${qt}$PETOOLS_LIB${qt}"        >> $out_vars
$ECHO "setenv SOLVE_EXTRA_LIB  ${qt}$SOLVE_EXTRA_LIB${qt}"    >> $out_vars
$ECHO "setenv SOLVE_LIB_VEC    ${qt}$SOLVE_LIB_VEC${qt}"      >> $out_vars
$ECHO "setenv SOLVE_LIB_BLAS   ${qt}$SOLVE_LIB_BLAS${qt}"     >> $out_vars
$ECHO "setenv SPD_FOURPACK     $SPD_FOURPACK"                 >> $out_vars
$ECHO "setenv MK5_C            ${qt}$MK5_C${qt}"              >> $out_vars
$ECHO "setenv MK5_F95_NOOPT    ${qt}$MK5_F95_NOOPT${qt}"      >> $out_vars
if ( $SPD_NOOPT == "NO" ) then
     $ECHO "setenv MK5_F95_OPT      ${qt}$MK5_F95_OPT${qt}"   >> $out_vars
     $ECHO "setenv MK5_F95          ${qt}$MK5_F95${qt}"       >> $out_vars
else
     $ECHO "setenv MK5_F95_OPT      ${qt}$MK5_F95_NOOPT${qt}" >> $out_vars
     $ECHO "setenv MK5_F95          ${qt}$MK5_F95_NOOPT${qt}" >> $out_vars
endif
$ECHO "setenv MK5_LINK         ${qt}$MK5_LINK${qt}"           >> $out_vars
$ECHO "setenv SPD_PREFIX       $SPD_PREFIX"                   >> $out_vars
$ECHO "setenv SPD_ROOT         $SPD_ROOT"                     >> $out_vars
$ECHO "setenv SPD_BIN          $SPD_BIN"                      >> $out_vars
$ECHO "setenv SPD_OS           $SPD_OS"                       >> $out_vars
$ECHO "setenv SPD_OPENMP       $SPD_OPENMP"                   >> $out_vars
$ECHO "setenv SPD_USE_DIAGI    $SPD_USE_DIAGI"                >> $out_vars
$ECHO "setenv SPD_DIAGI        $SPD_DIAGI"                    >> $out_vars
$ECHO "setenv SPD_LIB_DIAGI    $SPD_LIB_DIAGI"                >> $out_vars
$ECHO "setenv SPD_SOLIB        $SPD_SOLIB"                    >> $out_vars
$ECHO "setenv SPD_VERSION      $SPD_VERSION"                  >> $out_vars
#
source ${SPD_ROOT}/support/spd_vars
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
chmod g+rw,o+r ${SPD_ROOT}/Makefile
chmod g+rw,o+r ${SPD_ROOT}/support/spd_vars
echo "ok" | tee -a  $CONF_LOG
#
echo "config.csh is done"
