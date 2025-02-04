#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Script  config.csh  sets numerous variables needed for             *
# *   compilation and linking of petools. It creates files with          *
# *   directives for FORTRAN-compilers and C-compiler.                   *
# *   It creates include files using template files for includes and     *
# *   local files with preferences.                                      *
# *                                                                      *
# *   Usage:  config.csh                                                 *
# *                                                                      *
# * ### 20-JUN-1998  config.csh  v8.19  (c) L. Petrov   12-OCT-2024  ### *
# *                                                                      *
# ************************************************************************
#
setenv LANG   C
setenv LC_ALL C
set config_revision_date = "2024.10.12"
#
set atlas_min_version = 3.10.0
set openblas_min_version = 0.2.0
#
unalias *
setenv SUPPORT_PATH `dirname $0`
cd     $SUPPORT_PATH
cd ../
setenv PETOOLS_ROOT  `pwd`
if (    -f ${PETOOLS_ROOT}/support/petools_vars ) then
     rm -f ${PETOOLS_ROOT}/support/petools_vars
endif
if (    -f ${PETOOLS_ROOT}/Makefile ) then
     rm -f ${PETOOLS_ROOT}/Makefile
endif
set OS_name = `uname`
switch ( $OS_name )
   case "HP-UX":
     set ECHO = "/bin/echo"
     set qt = "\0042"
     breaksw
   case "SunOS":
     set ECHO = "/bin/echo"
     set qt = "\042"
     breaksw
   case "Linux":
     set ECHO = "/bin/echo -e"
     set qt = "\042"
     breaksw
   case "Darwin":
     set ECHO = "/bin/echo"
     set qt = '"'
     breaksw
endsw
#
if ( `uname` == "Linux" ) then
     set num_threads     = `cat /proc/cpuinfo | grep "core id" | wc -l`
     set num_thr_per_cpu = `lscpu | grep '^Thread(s) per core:' | awk '{print $4}'`
     set num_cores = `expr $num_threads / $num_thr_per_cpu`
else
     set num_cores = `sysctl -n  machdep.cpu.core_count`
endif
#
set    date_stamp  = `date "+%Y.%m.%d-%H:%M:%S" | tr "[a-z]" "[A-Z]"`
set    host_name   = `uname -n`
echo "petools config  $date_stamp  on  $host_name" >>! $CONF_LOG
echo "petools config  revision date: " $config_revision_date >>! $CONF_LOG
#
switch ( $OS_name )
   case "Linux":
     cat /proc/cpuinfo  | sort -u >> $CONF_LOG
     cat /proc/meminfo  >> $CONF_LOG
     cat /proc/swaps    >> $CONF_LOG
     df                 >> $CONF_LOG
     setenv             >> $CONF_LOG
     breaksw
   case "Darwin":
     sysctl -a | grep cpu | sort -u >> $CONF_LOG
     hostinfo           >> $CONF_LOG
     df                 >> $CONF_LOG
     setenv             >> $CONF_LOG
     breaksw
endsw
if ( $PETOOLS_SHORT_TEST == 1 ) then
     echo "CONF_LOG = $CONF_LOG"  > Makefile
     echo ' '                    >> Makefile
     echo '.PHONY: short-test'   >> Makefile
     echo 'short-test:' >> Makefile
     echo '	@echo "Failure in configuration. Please examine file $(CONF_LOG)"' >> Makefile
     echo '	@exit 1' >> Makefile
endif
#
if ( $?PETOOLS_LOCAL != 0 ) then
     switch ( $OS_name )
       case "HP-UX":
          setenv PETOOLS_LOCAL ${PETOOLS_ROOT}/support/petools_hp.lcl
          breaksw
       case "Linux":
          if ( "`cat /proc/cpuinfo | grep sse4_2 | sort -u`" != "" ) then
                setenv PETOOLS_LOCAL ${PETOOLS_ROOT}/support/petools_intel_core_i7.lcl
	    else 
                setenv PETOOLS_LOCAL ${PETOOLS_ROOT}/support/petools_intel_p4_sse2.lcl
          endif
          setenv SOLVE_HP_VERSION 0
          breaksw
       case "Darwin":
          if ( "`sysctl -n machdep.cpu.features | grep SSE4`" != "" ) then
                setenv PETOOLS_LOCAL ${PETOOLS_ROOT}/support/petools_intel_core_i7.lcl
	    else 
                setenv PETOOLS_LOCAL ${PETOOLS_ROOT}/support/petools_intel_p4_sse2.lcl
          endif
          setenv SOLVE_HP_VERSION 0
          breaksw
       case "SunOS":
          setenv PETOOLS_LOCAL ${PETOOLS_ROOT}/support/petools_sparc.lcl
          setenv SOLVE_HP_VERSION 0
          breaksw
       default:
          echo "Operating system $OS_name is not yet supported :-("
          exit 1
     endsw
endif
if ( -f $PETOOLS_LOCAL ) then
   else
     echo "config.csh: Cannot find file $PETOOLS_LOCAL"
     exit 1
endif
#
# --- Check X11 files
#
if ( $PETOOLS_PGPLOT == "YES" ) then
     if ( $PETOOLS_SHORT_TEST == 0 ) then
          if ( -d pgplot == 0 ) then
               if ( -f $PETOOLS_PGPLOT_TARBALL == 0 ) then
                    echo "config.csh Did not find pgplot tarball $PETOOLS_PGPLOT_TARBALL specified with --with-pgplot_tarball petools." | tee -a $CONF_LOG
                    exit 1
               endif
               if ( `echo $PETOOLS_PGPLOT_TARBALL | grep tar.gz` != "" ) then
	            tar -zxf $PETOOLS_PGPLOT_TARBALL  | tee -a $CONF_LOG
                 else if ( `echo $PETOOLS_PGPLOT_TARBALL | grep tar.bz2` != "" ) then
	            tar -jxf $PETOOLS_PGPLOT_TARBALL  | tee -a $CONF_LOG
                 else
	            tar -xf $PETOOLS_PGPLOT_TARBALL  | tee -a $CONF_LOG
               endif
               if ( $status != 0 ) then
                    echo "Error in untarring pgplot tarball $PETOOLS_PGPLOT_TARBALL" | tee -a $CONF_LOG
                    exit 1
               endif
          endif 
          if ( -f pgplot/pgplot_mods.txt == 0 ) then
               patch -Np0 -i pgplot-01.patch | tee -a $CONF_LOG
               if ( $status != 0 ) then
                    echo "Error in patching pgplot" | tee -a $CONF_LOG
               endif
               cp pgplot_mods.txt pgplot/ 
          endif
     endif
#
     $ECHO "config.csh: Check X11 files ... \c" | tee -a $CONF_LOG
     source ${PETOOLS_ROOT}/support/search_X11.csh
     if ( $status != 0 ) then
          echo "config.csh: Look at file $CONF_LOG for more detail"
          exit 1
     endif
     echo "ok"
endif
#
$ECHO "config.csh: Check Fortran compiler ... \c" | tee -a $CONF_LOG
if ( $?MK5_FC == 0 ) then
     setenv MK5_FC `${PETOOLS_ROOT}/support/fortran_compiler_guess.csh`
     if ( $status != 0 ) then
          echo "config.csh: FAILURE" | tee -a $CONF_LOG
          echo "config.csh: No Fortran compiler was found " | tee -a  $CONF_LOG
          echo "config.csh: Refer to installation instruction"
          echo "config.csh: Look at file " $CONF_LOG
          exit 1
     endif
endif
echo "ok"
#
unsetenv PETOOLS_COMPVAR_DEF
unsetenv NO_ENDIAN_CHECK
#
# --- Set standard variables
#
source ${PETOOLS_ROOT}/support/petools_standard_vars
if ( -f ${PETOOLS_ROOT}/local/petools_vars ) then
#
# -- Overwrite standard variables with local prefernces
#
     source ${PETOOLS_ROOT}/local/petools_vars
endif
if ( "$MK5_LDFLAGS" != "" ) then 
     setenv SOLVE_EXTRA_LIB "$SOLVE_EXTRA_LIB $MK5_LDFLAGS"
endif
#
# --- Check file $PETOOLS_LOCAL
#
#
# Check: do you have a C compiler?
#
$ECHO "config.csh: Check C compiler ... \c" | tee -a $CONF_LOG
${PETOOLS_ROOT}/support/check_file.csh $MK5_CC command;
if ( $status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: C compiler has not been found " | tee -a  $CONF_LOG
     echo "config.csh: Refer to installation instruction"
     echo "config.csh: Look at file " $CONF_LOG
     exit 1
endif
#
# --- Remove stale executables
#
if (    -f ${PETOOLS_ROOT}/bin/check_endian ) then
     rm -f ${PETOOLS_ROOT}/bin/check_endian
endif
#
if (    -f ${PETOOLS_ROOT}/bin/learn_fortran_true ) then
     rm -f ${PETOOLS_ROOT}/bin/learn_fortran_true
endif
#
if (    -f ${PETOOLS_ROOT}/bin/learn_fortran_false) then
     rm -f ${PETOOLS_ROOT}/bin/learn_fortran_false
endif
#
# --- First set envirnonment variables for compilers in no_endian_check and
# --- no true/false check mode
#
$ECHO "config.csh: Set environment variables for compilers... \c" | tee -a  $CONF_LOG
setenv NO_ENDIAN_CHECK     "YES"
setenv NO_TRUE_FALSE_CHECK "YES"
source ${PETOOLS_ROOT}/support/compile_var.csh
if ( $status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: Error in setting environment variables for compilation" | tee -a  $CONF_LOG
     echo "config.csh: See file " $CONF_LOG
     exit 1
endif
if ( "`echo $PETOOLS_BLAS | grep -i intel`" != "" ) then
      setenv MK5_PREP_DIRS $MK5_PREP_DIRECTIVES_NOUNDSC
   else
      setenv MK5_PREP_DIRS $MK5_PREP_DIRECTIVES_UNDSC
endif
echo "ok"
#
# --- Compile check_endian program
#
$ECHO "config.csh: Check endian... \c" | tee -a  $CONF_LOG
if ( -f $PETOOLS_ROOT/bin/check_endian ) rm -f $PETOOLS_ROOT/bin/check_endian
$MK5_C -c -o /tmp/check_endian.o $PETOOLS_ROOT/support/check_endian.c >>& $CONF_LOG
if ( $status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: $MK5_C -c -o /tmp/check_endian.o $PETOOLS_ROOT/support/check_endian.c "
     echo "config.csh: Error in compilation check_endian program " | tee -a  $CONF_LOG
     echo "config.csh: See file " $CONF_LOG
     exit 1
endif
#
# --- ... and link it
#
$MK5_C -o $PETOOLS_ROOT/bin/check_endian /tmp/check_endian.o >>& $CONF_LOG
if ( $status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: Error in linking check_endian program " | tee -a  $CONF_LOG
     echo "config.csh: See file " $CONF_LOG
     exit 1
endif
rm -f /tmp/check_endian.o
echo "  Your architecture is `$PETOOLS_ROOT/bin/check_endian`" | tee -a  $CONF_LOG
#
# --- Compile learn_fortran_true program
#
$ECHO "config.csh: Check fortran_true...  \c" | tee -a  $CONF_LOG
if ( -f $PETOOLS_ROOT/bin/learn_fortran_true ) rm -f $PETOOLS_ROOT/bin/learn_fortran_true
$MK5_F95 -c -o /tmp/learn_fortran_true.o $PETOOLS_ROOT/support/learn_fortran_true.f >>& $CONF_LOG
if ( $status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: Error in compilation learn_fortran_true program " | tee -a  $CONF_LOG
     echo "config.csh: MK5_F95=$MK5_F95"
     echo "config.csh: See file " $CONF_LOG
     exit 1
endif
#
# --- ... and link it
#
$MK5_LINK -o $PETOOLS_ROOT/bin/learn_fortran_true /tmp/learn_fortran_true.o >>& $CONF_LOG
if ( $status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: Error in linking learn_fortran_true program " | tee -a  $CONF_LOG
     echo "config.csh: MK5_LINK=$MK5_LINK"
     echo "config.csh: See file " $CONF_LOG
     exit 1
endif
rm -f /tmp/learn_fortran_true.o
echo "  Fortran .TRUE. is `$PETOOLS_ROOT/bin/learn_fortran_true`" | tee -a  $CONF_LOG
#
# --- Compile learn_fortran_false program
#
$ECHO "config.csh: Check fortran_false... \c" | tee -a  $CONF_LOG
if ( -f $PETOOLS_ROOT/bin/learn_fortran_false ) rm -f $PETOOLS_ROOT/bin/learn_fortran_false
$MK5_F95 -c -o /tmp/learn_fortran_false.o $PETOOLS_ROOT/support/learn_fortran_false.f >>& $CONF_LOG
if ( $status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: Error in compilation learn_fortran_false program " | tee -a  $CONF_LOG
     echo "config.csh: See file " $CONF_LOG
     exit 1
endif
#
# --- ... and link it
#
$MK5_LINK -o $PETOOLS_ROOT/bin/learn_fortran_false /tmp/learn_fortran_false.o >>& $CONF_LOG
if ( $status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: Error in linking learn_fortran_false program " | tee -a  $CONF_LOG
     echo "config.csh: See file " $CONF_LOG
     exit 1
endif
echo "  Fortran .FALSE. is `$PETOOLS_ROOT/bin/learn_fortran_false`" | tee -a  $CONF_LOG
rm -f /tmp/learn_fortran_false.o
#
# --- Setting all environment variables needed for compilation and linking
#
$ECHO "config.csh: Set options for compilers ... \c" | tee -a  $CONF_LOG
source $PETOOLS_ROOT/support/compile_var.csh
if ( $?PETOOLS_COMPVAR_DEF == 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: No suitable Fortran compiler was found." | tee -a $CONF_LOG
     echo "config.csh: Refer to installation instruction"
     echo "config.csh: Look at file " $CONF_LOG
     exit 1
endif
echo "ok"
${PETOOLS_ROOT}/support/f95_version.csh | tee -a $CONF_LOG
${PETOOLS_ROOT}/support/cc_version.csh  | tee -a $CONF_LOG
#
# --- Copy directivies for the preprocessor of the Fortran95 compiler which
# --- will be actually used
#
if ( "`echo $PETOOLS_BLAS | grep -i intel`" != "" ) then
     cp $MK5_PREP_DIRECTIVES_UNDSC    ${PETOOLS_ROOT}/include/mk5_preprocessor_directives.inc
  else
     cp $MK5_PREP_DIRECTIVES_NOUNDSC  ${PETOOLS_ROOT}/include/mk5_preprocessor_directives.inc
endif
#
if ( `uname` == "Linux" ) then
#
# -- Put the wrapper for fpp (Fortran preprocessor) which will be used
# -- instead of the vendor supplied preprocessor
#
     cp $PETOOLS_ROOT/support/intel_fpp $PETOOLS_ROOT/support/fpp
#
# -- Put the wrapper for ld (linker) which will sometimes be used to
# -- alter default Fortran compiler options passed to the linker
#
     cp $PETOOLS_ROOT/support/intel_ld  $PETOOLS_ROOT/support/ld
endif
#
echo "Commands for calling compilers: " >> $CONF_LOG
echo "which cc  " >> $CONF_LOG
which cc >> $CONF_LOG
echo "which $MK5_FC  " >> $CONF_LOG
which $MK5_FC >> $CONF_LOG
#
$ECHO "config.csh: Check whether openMP works... \c"
support/check_openmp.csh >>& $CONF_LOG
if ( $status != 0 ) then
     echo "Test of openMP has failed. Please check gcc/gfortran installation" | tee -a  $CONF_LOG
     echo "config.csh: Look at file $CONF_LOG for more detail"
     exit 1
endif
echo "ok"
#
if ( -d $PETOOLS_PREFIX == 0 ) then
      mkdir     $PETOOLS_PREFIX 
      if ( $status != 0 ) then
           $ECHO "config.csh: FAILURE: cannot create $PETOOLS_PREFIX" 
           echo  "config.csh: FAILURE: cannot create $PETOOLS_PREFIX" >> $CONF_LOG
           exit 1
      endif   
      chmod 775 $PETOOLS_PREFIX 
endif
if ( -d $PETOOLS_PREFIX/share == 0 ) then
      mkdir     $PETOOLS_PREFIX/share
      if ( $status != 0 ) then
           $ECHO "config.csh: FAILURE: cannot create $PETOOLS_PREFIX/share" 
           echo  "config.csh: FAILURE: cannot create $PETOOLS_PREFIX/share" >> $CONF_LOG
           exit 1
      endif   
      chmod 775 $PETOOLS_PREFIX/share
endif
if ( -d $PETOOLS_PREFIX/bin == 0 ) then
      mkdir     $PETOOLS_PREFIX/bin
      if ( $status != 0 ) then
           $ECHO "config.csh: FAILURE: cannot create $PETOOLS_PREFIX/bin" 
           echo  "config.csh: FAILURE: cannot create $PETOOLS_PREFIX/bin" >> $CONF_LOG
           exit 1
      endif   
      chmod 775 $PETOOLS_PREFIX/share
endif
support/cc_version.csh version > $PETOOLS_PREFIX/share/cc_version.txt
if ( $status != 0 ) then
     $ECHO "config.csh: FAILURE: cannot write in $PETOOLS_PREFIX/share" 
     echo  "config.csh: FAILURE: cannot write in $PETOOLS_PREFIX/share"  >> $CONF_LOG
     exit 1
endif 
support/f95_version.csh version > $PETOOLS_PREFIX/share/fortran_version.txt
#
if ( $PETOOLS_SHORT_TEST == 1 ) then
     set out_vars = $PETOOLS_PREFIX/bin/petools_vars
#
# -- Write down compiler related variables to petools_vars
#
     if ( -f $out_vars ) rm -f $out_vars
     $ECHO "setenv MK5_FC              ${qt}$MK5_FC${qt}"         >> $out_vars
     $ECHO "setenv MK5_F95_NOOPT       ${qt}$MK5_F95_NOOPT${qt}"  >> $out_vars
     if ( $PETOOLS_NOOPT == "NO" ) then
          $ECHO "setenv MK5_F95        ${qt}$MK5_F95${qt}"        >> $out_vars
          $ECHO "setenv MK5_F95_OPT    ${qt}$MK5_F95_OPT${qt}"    >> $out_vars
          $ECHO "setenv MK5_F95_OPTEST ${qt}$MK5_F95_OPTEST${qt}" >> $out_vars
       else
          $ECHO "setenv MK5_F95        ${qt}$MK5_F95_NOOPT${qt}"  >> $out_vars
          $ECHO "setenv MK5_F95_OPT    ${qt}$MK5_F95_NOOPT${qt}"  >> $out_vars
          $ECHO "setenv MK5_F95_OPTEST ${qt}$MK5_F95_NOOPT${qt}"  >> $out_vars
     endif
     $ECHO "setenv MK5_C               ${qt}$MK5_C${qt}"          >> $out_vars
     $ECHO "setenv MK5_LINK            ${qt}$MK5_LINK${qt}"       >> $out_vars
     $ECHO "setenv MK5_C_LINK          ${qt}$MK5_C_LINK${qt}"     >> $out_vars
     $ECHO "setenv MK5_COMPILER        ${qt}$MK5_COMPILER${qt}"   >> $out_vars
     $ECHO "setenv MK5_C_OPENMP        ${qt}$MK5_C_OPENMP${qt}"   >> $out_vars
     $ECHO "setenv MK5_F_OPENMP        ${qt}$MK5_F_OPENMP${qt}"   >> $out_vars
#
# -- Write 
#
     if ( -d ${PETOOLS_PREFIX}            == 0 ) mkdir ${PETOOLS_PREFIX}
     if ( -d ${PETOOLS_PREFIX}/bin        == 0 ) mkdir ${PETOOLS_PREFIX}/bin
     cp $out_vars ${PETOOLS_PREFIX}/bin/
     uname -a
     if ( `uname` == "Linux" ) then
          if ( -f /etc/system-release ) cat /etc/system-release 
          cat /proc/cpuinfo | sort -u | grep "model name\|vendor"
          cat /proc/meminfo | grep MemTotal
       else if ( `uname` == "Darwin" ) then
          system_profiler SPSoftwareDataType | grep "System Version"
          /usr/bin/xcodebuild -version | grep Xcode
          set cpu_string = `sysctl -n machdep.cpu.brand_string`
          set mem_size = `sysctl -n  hw.memsize`
          echo "CPU: $cpu_string"
          echo "Memory size: $mem_size"
     endif
     /usr/bin/df -h >& /tmp/__config__$$
     cat /tmp/__config__$$ | grep -v run
     rm  /tmp/__config__$$ 
     echo "Number of cores: " $num_cores
     echo "Environment variables:"
     echo " "
     setenv
#
     $ECHO "config.csh: short test successfully completed"
     echo  "config.csh: short test successfully completed" >> $CONF_LOG
     echo  "ok" >> $CONF_LOG
#
# --- Write a simple Makefile
#
     echo '.PHONY: short-test'   > Makefile
     echo 'short-test:'         >> Makefile
     echo '	@echo "ok"'     >> Makefile
     echo '	@exit 0'        >> Makefile
     exit 0 
endif
#
# --- Check zlib
#
if ( PETOOLS_ZLIB_DIR == "/usr" || PETOOLS_ZLIB_DIR == "/usr/local" ) then
     setenv ZLIB_INC_DIR ${PETOOLS_ZLIB_DIR}/include
     setenv ZLIB_LIB_DIR ${PETOOLS_ZLIB_DIR}/lib64
  else if ( PETOOLS_ZLIB_DIR == "/usr/" || PETOOLS_ZLIB_DIR == "/usr/local/" ) then
     setenv ZLIB_INC_DIR ${PETOOLS_ZLIB_DIR}include
     setenv ZLIB_LIB_DIR ${PETOOLS_ZLIB_DIR}lib64
  else
     setenv ZLIB_INC_DIR ${PETOOLS_ZLIB_DIR}/include
     setenv ZLIB_LIB_DIR ${PETOOLS_ZLIB_DIR}/lib
endif
$ECHO "config.csh: Check zlib ... \c" | tee -a $CONF_LOG
${PETOOLS_ROOT}/support/check_zlib.csh  >>&    $CONF_LOG
if ( $status != 0 ) then
     echo "failure" | tee -a $CONF_LOG
     ${PETOOLS_ROOT}/support/check_zlib.csh
     echo "config.csh: Cannot compile/link against zlib library" | tee -a  $CONF_LOG
     echo "config.csh: Please install zlib and try again"        | tee -a  $CONF_LOG
     echo "config.csh: Look at file $CONF_LOG for more detail"
     exit 1
else
     echo "ok"
endif
#
# --- Check libpng
#
if ( PETOOLS_PNG_DIR == "/usr" || PETOOLS_PNG_DIR == "/usr/local" ) then
     setenv PNG_INC_DIR ${PETOOLS_PNG_DIR}/include
     setenv PNG_LIB_DIR ${PETOOLS_PNG_DIR}/lib64
  else if ( PETOOLS_PNG_DIR == "/usr/" || PETOOLS_PNG_DIR == "/usr/local/" ) then
     setenv PNG_INC_DIR ${PETOOLS_PNG_DIR}include
     setenv PNG_LIB_DIR ${PETOOLS_PNG_DIR}lib64
  else
     setenv PNG_INC_DIR ${PETOOLS_PNG_DIR}/include
     setenv PNG_LIB_DIR ${PETOOLS_PNG_DIR}/lib
endif
$ECHO "config.csh: Check png ... \c" | tee -a $CONF_LOG
${PETOOLS_ROOT}/support/check_png.csh  >>&    $CONF_LOG
if ( $status != 0 ) then
     echo "failure" | tee -a $CONF_LOG
     ${PETOOLS_ROOT}/support/check_png.csh
     echo "config.csh: Cannot compile/link against png library" | tee -a  $CONF_LOG
     echo "config.csh: Please install png and try again"        | tee -a  $CONF_LOG
     echo "config.csh: Look at file $CONF_LOG for more detail"
     exit 1
else
     echo "ok"
endif
#
setenv PETOOLS_BLAS_USE "YES"
if ( "$PETOOLS_BLAS" == "NO" )  then
     setenv PETOOLS_BLAS     $PETOOLS_ROOT/blas_stub/libblas_stub.a
     setenv PETOOLS_BLAS_USE "NO"
     $ECHO "config.csh: No blas library is supplied" | tee -a  $CONF_LOG
   else 
     if ( "`echo $PETOOLS_BLAS | grep atlas`" != "" ) then
          set blas = atlas
          set status = 0
        else if ( "`echo $PETOOLS_BLAS | grep openblas`" != "" ) then
          set blas = openblas 
          set status = 0
        else if ( "`echo $PETOOLS_BLAS | grep intel`" != "" ) then
          set blas = mkl
          set status = 0
        else 
          set blas = "unknown blas"
          set status = 1
     endif
     $ECHO "config.csh: Check $blas library... \c" | tee -a  $CONF_LOG
     if ( $status != 0 ) then
          echo "Failed to link against "$blas" library" | tee -a  $CONF_LOG
          echo "config.csh: Look at file $CONF_LOG for more detail"
          exit 1
     endif
     if ( "$blas" == atlas ) then
          support/get_atlas_version.csh >>& $CONF_LOG
          if ( $status != 0 ) then
               echo "Failure in checking functionilty of altas library" | tee -a  $CONF_LOG
               echo "See a failed test:"
               echo " "
               support/get_atlas_version.csh
               echo "config.csh: Look at file $CONF_LOG for more detail"
               exit 1
          endif
          echo "ok" 
          set atlas_version = `$PETOOLS_ROOT/bin/get_atlas_version | grep "ATLAS version" | awk '{print $3}'`
          $ECHO "config.csh: Check atlas version >= $atlas_min_version ... \c" | tee -a  $CONF_LOG
          if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh $atlas_version $atlas_min_version` == 0    ) then
               echo ""
               echo "Atlas version $atlas_version was found, but $atlas_min_version is required"
               echo "Please upgrade atlas library"
               echo "config.csh: Look at file $CONF_LOG for more detail"
               exit 1
          endif
          echo $atlas_version 
          $PETOOLS_ROOT/bin/get_atlas_version | grep "Lapack version"
          make -f $PETOOLS_ROOT/support/get_atlas_version.mak clean >>&  $CONF_LOG
     endif
     setenv SOLVE_LIB_BLAS "$PETOOLS_BLAS"
#
     $ECHO "config.csh: Check $blas library... \c"
     support/check_libblas.csh >>& $CONF_LOG
     if ( $status != 0 ) then
          echo "Test of BLAS library $blas failed" | tee -a  $CONF_LOG
          echo "config.csh: Look at file $CONF_LOG for more detail"
          exit 1
     endif
     echo "ok"
#
# -- Add blas flavour definition as a preprocessor variable
#
     setenv MK5_F95        `echo $MK5_F95        | sed "s@-fno-diagnostics-color@-D BLAS=$blas -fno-diagnostics-color@g" | sed "s@(@\(@g" | sed "s@)@\)@g"`
     setenv MK5_F95_OPT    `echo $MK5_F95_OPT    | sed "s@-fno-diagnostics-color@-D BLAS=$blas -fno-diagnostics-color@g" | sed "s@(@\(@g" | sed "s@)@\)@g"`
     setenv MK5_F95_NOOPT  `echo $MK5_F95_NOOPT  | sed "s@-fno-diagnostics-color@-D BLAS=$blas -fno-diagnostics-color@g" | sed "s@(@\(@g" | sed "s@)@\)@g"`
     setenv MK5_F95_OPTEST `echo $MK5_F95_OPTEST | sed "s@-fno-diagnostics-color@-D BLAS=$blas -fno-diagnostics-color@g" | sed "s@(@\(@g" | sed "s@)@\)@g"`
endif
#
# --- Check ncurses
#
if ( $PETOOLS_NCURSES_DIR == "" ) then
     setenv PETOOLS_NCURSES_LIBDIR ""
 else
     if ( -d $PETOOLS_NCURSES_DIR == 0 ) then
           $ECHO "config.csh: Cannot find directory $PETOOLS_NCURSES_DIR spedicifed in --with-ncurses option"
           echo "config.csh: Look at file $CONF_LOG for more detail"
	   exit 1
     endif
     if ( -d ${PETOOLS_NCURSES_DIR}/lib64 ) then
           setenv PETOOLS_NCURSES_LIBDIR "-L ${PETOOLS_NCURSES_DIR}/lib64 -lncurses"
        else
           setenv PETOOLS_NCURSES_LIBDIR "-L ${PETOOLS_NCURSES_DIR}/lib -lncurses"
     endif
endif
$ECHO "config.csh: Check ncurses library... \c" | tee -a  $CONF_LOG
support/ncurses_check.csh >>& $CONF_LOG
if ( $status != 0 ) then
     support/ncurses_check.csh
     echo "Failed to link against ncurses library" | tee -a  $CONF_LOG
     echo "config.csh: Look at file $CONF_LOG for more detail"
     exit 1
endif
echo "ok"
#
# --- Check readline
#
if ( $PETOOLS_READLINE_DIR == "" ) then
     setenv PETOOLS_READLINE_LIBDIR ""
  else
     if ( -d $PETOOLS_READLINE_DIR == 0 ) then
           $ECHO "config.csh: Cannot find directory $PETOOLS_READLINE_DIR spedicifed in --with-readline option"
           echo "config.csh: Look at file $CONF_LOG for more detail"
	   exit 1
     endif
     if ( -f ${PETOOLS_READLINE_DIR}/lib64 ) then
           setenv PETOOLS_READLINE_LIBDIR "-L ${PETOOLS_READLINE_DIR}/lib64 -lreadline"
        else
           setenv PETOOLS_READLINE_LIBDIR "-L ${PETOOLS_READLINE_DIR}/lib -lreadline"
     endif
endif
$ECHO "config.csh: Check readline library... \c" | tee -a  $CONF_LOG
support/readline_check.csh >>& $CONF_LOG
if ( $status != 0 ) then
     support/readline_check.csh
     echo "Failed to link against readline library" | tee -a  $CONF_LOG
     echo "config.csh: Look at file $CONF_LOG for more detail"
     exit 1
endif
if ( $ZLIB_LIB_DIR == "/usr/lib64" ) then
     setenv SOLVE_EXTRA_LIB "$SOLVE_EXTRA_LIB -lz"
   else
     setenv SOLVE_EXTRA_LIB "$SOLVE_EXTRA_LIB -L $ZLIB_LIB_DIR -lz"
endif
if ( $PNG_LIB_DIR == "/usr/lib" ) then
     setenv SOLVE_EXTRA_LIB "$SOLVE_EXTRA_LIB -lpng"
   else
     setenv SOLVE_EXTRA_LIB "$SOLVE_EXTRA_LIB -L $PNG_LIB_DIR -lpng"
endif
support/readline_check.csh >>& $CONF_LOG
echo "ok"
#
set out_vars = ${PETOOLS_ROOT}/support/petools_vars
if ( -f $out_vars ) rm -f $out_vars
if ( "$PETOOLS_BLAS" == "NO" ) then
     setenv SOLVE_LIB_BLAS   ${PETOOLS_PREFIX}/lib/libblas_stub.a
endif
#
setenv PETOOLS_USE PETOOLS
#
$ECHO '#\!/bin/csh'                                           > $out_vars
$ECHO "setenv PETOOLS_USE         $PETOOLS_USE"              >> $out_vars
$ECHO "setenv PETOOLS_ROOT        $PETOOLS_ROOT"             >> $out_vars
$ECHO "setenv PETOOLS_PREFIX      $PETOOLS_PREFIX"           >> $out_vars
if ( $PETOOLS_NCURSES_DIR == "" ) then
     if ( $PETOOLS_READLINE_DIR == "" ) then
          $ECHO "setenv PETOOLS_LIB         ${qt}-L$PETOOLS_PREFIX/lib -lpetools    -lreadline -lncurses${qt}" >> $out_vars
          $ECHO "setenv PETOOLS_LIB_A       ${qt}$PETOOLS_PREFIX/lib/libpetools.a -lreadline -lncurses${qt}" >> $out_vars
          setenv READLINE_LIB "-lreadline"
          setenv CURSES_LIB   "-lncurses"
	else
          $ECHO "setenv PETOOLS_LIB         ${qt}-L$PETOOLS_PREFIX/lib -lpetools $PETOOLS_READLINE_LIBDIR -lncurses${qt}" >> $out_vars
          $ECHO "setenv PETOOLS_LIB_A       ${qt}$PETOOLS_PREFIX/lib/libpetools.a  $PETOOLS_READLINE_LIBDIR -lncurses${qt}" >> $out_vars
          setenv READLINE_LIB "$PETOOLS_READLINE_LIBDIR"
          setenv CURSES_LIB   "-lncurses"
     endif
   else
     if ( $PETOOLS_READLINE_DIR == "" ) then
          $ECHO "setenv PETOOLS_LIB         ${qt}-L$PETOOLS_PREFIX/lib -lpetools -lreadline $PETOOLS_NCURSES_LIBDIR${qt}" >> $out_vars
          $ECHO "setenv PETOOLS_LIB_A       ${qt}$PETOOLS_PREFIX/lib/libpetools.a  -lreadline $PETOOLS_NCURSES_LIBDIR${qt}" >> $out_vars
          setenv READLINE_LIB "--lreadline"
          setenv CURSES_LIB   "$PETOOLS_NCURSES_LIBDIR"
        else
          $ECHO "setenv PETOOLS_LIB         ${qt}-L$PETOOLS_PREFIX/lib -lpetools $PETOOLS_READLINE_LIBDIR $PETOOLS_NCURSES_LIBDIR${qt}" >> $out_vars
          $ECHO "setenv PETOOLS_LIB_A       ${qt}$PETOOLS_PREFIX/lib/libpetools.a  $PETOOLS_READLINE_LIBDIR $PETOOLS_NCURSES_LIBDIR${qt}" >> $out_vars
          setenv READLINE_LIB "$PETOOLS_READLINE_LIBDIR"
          setenv CURSES_LIB   "$PETOOLS_NCURSES_LIBDIR"
     endif
endif
#
if ( $PETOOLS_READLINE_DIR != "" ) then
     set extra_inc = $PETOOLS_READLINE_DIR/include
     if ( `echo $MK5_C | grep $extra_inc` == "" ) then
           setenv MK5_C "$MK5_C -I $extra_inc"
     endif
endif
if ( $PETOOLS_NCURSES_DIR != "" ) then
     set extra_inc = $PETOOLS_NCURSES_DIR/include
     if ( `echo $MK5_C | grep $extra_inc` == "" ) then
           setenv MK5_C "$MK5_C -I $extra_inc"
     endif
endif
#
$ECHO "setenv PETOOLS_BIN         $PETOOLS_ROOT/bin"              >> $out_vars
$ECHO "setenv PETOOLS_BITS        $PETOOLS_BITS"                  >> $out_vars
$ECHO "setenv PETOOLS_VERSION     $PETOOLS_VERSION"               >> $out_vars
$ECHO "setenv SOLVE_LIB_VEC       ${qt}$SOLVE_LIB_VEC${qt}"       >> $out_vars
$ECHO "setenv SOLVE_LIB_BLAS      ${qt}$SOLVE_LIB_BLAS${qt}"      >> $out_vars
$ECHO "setenv SOLVE_LIB_M         ${qt}$SOLVE_LIB_M${qt}"         >> $out_vars
$ECHO "setenv SOLVE_LIB_U77       ${qt}$SOLVE_LIB_U77${qt}"       >> $out_vars
$ECHO "setenv SOLVE_LIB_LCL       ${qt}$SOLVE_LIB_LCL${qt}"       >> $out_vars
$ECHO "setenv SOLVE_EXTRA_LIB     ${qt}$SOLVE_EXTRA_LIB${qt}"     >> $out_vars
$ECHO "setenv PETOOLS_BLAS_USE    ${qt}$PETOOLS_BLAS_USE${qt}"    >> $out_vars
if ( $OS_name == "HP-UX" ) then
   $ECHO "setenv SOLVE_HP_VERSION ${qt}$SOLVE_HP_VERSION${qt}"    >> $out_vars
endif
$ECHO "setenv SOLVE_LIB_CURSES    ${qt}$SOLVE_LIB_CURSES${qt}"    >> $out_vars
$ECHO "setenv SOLVE_C_INCLUDE     ${qt}$SOLVE_C_INCLUDE${qt}"     >> $out_vars
$ECHO "setenv MK5_PREP_DIRS       ${qt}$MK5_PREP_DIRS${qt}"       >> $out_vars
if ( $PETOOLS_PGPLOT == "YES" ) then
     setenv PGPLOT_DIR          ${PETOOLS_PREFIX}/bin                 
     setenv PGPLOT_FONT         ${PETOOLS_PREFIX}/share/grfont.dat
     setenv SOLVE_LIB_PGPLOT    "-L${PETOOLS_PREFIX}/lib -lpgplot"
     setenv SOLVE_LIB_PGPLOT_A  "${PETOOLS_PREFIX}/lib/libpgplot.a"
#
     $ECHO "setenv PGPLOT_DIR          ${qt}$PGPLOT_DIR${qt}"          >> $out_vars
     $ECHO "setenv PGPLOT_FONT         ${qt}$PGPLOT_FONT${qt}"         >> $out_vars
     $ECHO "setenv PGPLOT_DEV          ${qt}$PGPLOT_DEV${qt}"          >> $out_vars
     $ECHO "setenv PGPLOT_XW_MARGIN    $PGPLOT_XW_MARGIN"              >> $out_vars
     $ECHO "setenv PGPLOT_RGB          ${qt}$PGPLOT_DIR/rgb.txt${qt}"  >> $out_vars
     $ECHO "setenv SOLVE_PGPLOT_X_INC  ${qt}$SOLVE_PGPLOT_X_INC${qt}"  >> $out_vars
     $ECHO "setenv SOLVE_LIB_PGPLOT    ${qt}$SOLVE_LIB_PGPLOT${qt}"    >> $out_vars
     $ECHO "setenv SOLVE_LIB_PGPLOT_A  ${qt}$SOLVE_LIB_PGPLOT_A${qt}"  >> $out_vars
     $ECHO "setenv MK5_X11_INCLUDE     ${qt}$MK5_X11_INCLUDE${qt}"     >> $out_vars
     $ECHO "setenv MK5_X11_LIB         ${qt}$MK5_X11_LIB${qt}"         >> $out_vars
     $ECHO "setenv MK5_X11_BIN         ${qt}$MK5_X11_BIN${qt}"         >> $out_vars
     $ECHO "setenv SOLVE_LIB_XHP11     ${qt}$SOLVE_LIB_XHP11${qt}"     >> $out_vars
     $ECHO "setenv SOLVE_LIB_X11       ${qt}$SOLVE_LIB_X11${qt}"       >> $out_vars
     $ECHO "setenv SOLVE_LIB_XT        ${qt}$SOLVE_LIB_XT${qt}"        >> $out_vars
  else
     $ECHO "setenv MK5_X11_INCLUDE     ${qt} ${qt}"                    >> $out_vars
     $ECHO "setenv MK5_X11_LIB         ${qt} ${qt}"                    >> $out_vars
     $ECHO "setenv MK5_X11_BIN         ${qt} ${qt}"                    >> $out_vars
     $ECHO "setenv SOLVE_LIB_X11       ${qt} ${qt}"                    >> $out_vars
endif
$ECHO "setenv MK5_FC              ${qt}$MK5_FC${qt}"              >> $out_vars
$ECHO "setenv MK5_F95_NOOPT       ${qt}$MK5_F95_NOOPT${qt}"       >> $out_vars
if ( $PETOOLS_NOOPT == "NO" ) then
     $ECHO "setenv MK5_F95             ${qt}$MK5_F95${qt}"             >> $out_vars
     $ECHO "setenv MK5_F95_OPT         ${qt}$MK5_F95_OPT${qt}"         >> $out_vars
     $ECHO "setenv MK5_F95_OPTEST      ${qt}$MK5_F95_OPTEST${qt}"      >> $out_vars
  else
     $ECHO "setenv MK5_F95             ${qt}$MK5_F95_NOOPT${qt}"       >> $out_vars
     $ECHO "setenv MK5_F95_OPT         ${qt}$MK5_F95_NOOPT${qt}"       >> $out_vars
     $ECHO "setenv MK5_F95_OPTEST      ${qt}$MK5_F95_NOOPT${qt}"       >> $out_vars
endif
$ECHO "setenv MK5_C               ${qt}$MK5_C${qt}"               >> $out_vars
$ECHO "setenv MK5_LINK            ${qt}$MK5_LINK${qt}"            >> $out_vars
$ECHO "setenv MK5_C_LINK          ${qt}$MK5_C_LINK${qt}"          >> $out_vars
$ECHO "setenv MK5_COMPILER        ${qt}$MK5_COMPILER${qt}"        >> $out_vars
$ECHO "setenv MK5_C_OPENMP        ${qt}$MK5_C_OPENMP${qt}"        >> $out_vars
$ECHO "setenv MK5_F_OPENMP        ${qt}$MK5_F_OPENMP${qt}"        >> $out_vars
$ECHO "setenv PETOOLS_MAKE        ${qt}$PETOOLS_MAKE${qt}"        >> $out_vars
$ECHO "setenv PETOOLS_COMPVAR_DEF $PETOOLS_COMPVAR_DEF"           >> $out_vars
$ECHO "setenv PETOOLS_OS          `uname`"                        >> $out_vars
$ECHO "setenv HELP_DIR            $HELP_DIR"                      >> $out_vars
#
# --- Read the file with envirnonment variables
#
source ${PETOOLS_ROOT}/support/petools_vars
#
# --- Now we have to have a modified set of variables suitable for
# --- installation
#
set psi_vars = ${PETOOLS_ROOT}/support/petools_postinstall_vars
if ( -f $psi_vars ) rm -f $psi_vars
#
setenv SOLVE_C_INCLUDE   ${PETOOLS_PREFIX}/include/
setenv MK5_PREP_DIRS     ${PETOOLS_PREFIX}/include/mk5_preprocessor_directives.inc
if ( $PETOOLS_NCURSES_DIR == "" ) then
     if ( $PETOOLS_READLINE_DIR == "" ) then
          setenv PETOOLS_LIB   "-L$PETOOLS_PREFIX/lib -lpetools -lreadline -lncurses"
          setenv PETOOLS_LIB_A "$PETOOLS_PREFIX/lib/libpetools.a  -lreadline -lncurses"
	else
          setenv PETOOLS_LIB   "-L$PETOOLS_PREFIX/lib -lpetools $PETOOLS_READLINE_LIBDIR -lncurses"
          setenv PETOOLS_LIB_A "$PETOOLS_PREFIX/lib/libpetools.a  $PETOOLS_READLINE_LIBDIR -lncurses"
     endif
   else
     if ( $PETOOLS_READLINE_DIR == "" ) then
          setenv PETOOLS_LIB   "-L$PETOOLS_PREFIX/lib -lpetools -lreadline $PETOOLS_NCURSES_LIBDIR"
          setenv PETOOLS_LIB_A "$PETOOLS_PREFIX/lib/libpetools.a  -lreadline $PETOOLS_NCURSES_LIBDIR"
        else
          setenv PETOOLS_LIB   "-L$PETOOLS_PREFIX/lib -lpetools $PETOOLS_READLINE_LIBDIR $PETOOLS_NCURSES_LIBDIR"
          setenv PETOOLS_LIB_A "$PETOOLS_PREFIX/lib/libpetools.a  $PETOOLS_READLINE_LIBDIR $PETOOLS_NCURSES_LIBDIR"
     endif
endif
setenv PETOOLS_BIN       ${PETOOLS_PREFIX}/bin
setenv PETOOLS_INCLUDE   ${PETOOLS_PREFIX}/include
if ( `uname` == "SunOS" ) then
      setenv MK5_C       `echo "$MK5_C"       | sed 's|${PETOOLS_ROOT}|${PETOOLS_PREFIX}|g'`
      setenv MK5_F95     `echo "$MK5_F95"     | sed 's|${PETOOLS_ROOT}|${PETOOLS_PREFIX}|g'`
      setenv MK5_F95_OPT `echo "$MK5_F95_OPT" | sed 's|${PETOOLS_ROOT}|${PETOOLS_PREFIX}|g'`
      setenv MK5_F95_OPTEST `echo "$MK5_F95_OPTEST" | sed 's|${PETOOLS_ROOT}|${PETOOLS_PREFIX}|g'`
endif
#
#
# --- Now write down updated definitions. They later will be appended to
# --- the definition file which will be copied in the intsallation directory
#
$ECHO '# -----------------------------------------------------------' > $psi_vars
$ECHO "setenv SOLVE_C_INCLUDE     ${qt}$SOLVE_C_INCLUDE${qt}"        >> $psi_vars
$ECHO "setenv MK5_PREP_DIRS       ${qt}$MK5_PREP_DIRS${qt}"          >> $psi_vars
$ECHO "setenv PETOOLS_LIB         ${qt}$PETOOLS_LIB${qt}"            >> $psi_vars
$ECHO "setenv PETOOLS_LIB_A       ${qt}$PETOOLS_LIB_A${qt}"          >> $psi_vars
$ECHO "setenv PETOOLS_BIN         $PETOOLS_BIN"                      >> $psi_vars
$ECHO "setenv PETOOLS_INCLUDE     $PETOOLS_INCLUDE"                  >> $psi_vars
$ECHO "setenv MK5_FC              ${qt}$MK5_FC${qt}"                 >> $psi_vars
$ECHO "setenv MK5_F95_NOOPT       ${qt}$MK5_F95_NOOPT${qt}"          >> $psi_vars
if ( $PETOOLS_NOOPT == 1 ) then 
      $ECHO "setenv MK5_F95             ${qt}$MK5_F95_NOOPT${qt}"    >> $psi_vars
      $ECHO "setenv MK5_F95_OPT         ${qt}$MK5_F95_NOOPT${qt}"    >> $psi_vars
      $ECHO "setenv MK5_F95_OPTEST      ${qt}$MK5_F95_NOOPT${qt}"    >> $psi_vars
else
      $ECHO "setenv MK5_F95             ${qt}$MK5_F95${qt}"          >> $psi_vars
      $ECHO "setenv MK5_F95_OPT         ${qt}$MK5_F95_OPT${qt}"      >> $psi_vars
      $ECHO "setenv MK5_F95_OPTEST      ${qt}$MK5_F95_OPTEST${qt}"   >> $psi_vars
endif
#
$ECHO "setenv MK5_C               ${qt}$MK5_C${qt}"                  >> $psi_vars
$ECHO "setenv MK5_C_OPENMP        ${qt}$MK5_C_OPENMP${qt}"           >> $psi_vars
$ECHO "setenv MK5_F_OPENMP        ${qt}$MK5_F_OPENMP${qt}"           >> $psi_vars
if ( $PETOOLS_PGPLOT == "YES" ) then
     $ECHO "setenv PGPLOT_DIR         ${qt}$PGPLOT_DIR${qt}"         >> $psi_vars
     $ECHO "setenv PGPLOT_FONT        ${qt}$PGPLOT_FONT${qt}"        >> $psi_vars
     $ECHO "setenv PGPLOT_DEV         ${qt}$PGPLOT_DEV${qt}"         >> $psi_vars
     $ECHO "setenv PGPLOT_XW_MARGIN   $PGPLOT_XW_MARGIN"             >> $psi_vars
     $ECHO "setenv PGPLOT_RGB         ${qt}$PGPLOT_DIR/rgb.txt${qt}" >> $psi_vars
     $ECHO "setenv SOLVE_PGPLOT_X_INC ${qt}$SOLVE_PGPLOT_X_INC${qt}" >> $psi_vars
     $ECHO "setenv SOLVE_LIB_PGPLOT   ${qt}$SOLVE_LIB_PGPLOT${qt}"   >> $psi_vars
     $ECHO "setenv SOLVE_LIB_PGPLOT_A ${qt}$SOLVE_LIB_PGPLOT_A${qt}" >> $psi_vars
  else
     $ECHO "setenv MK5_X11_INCLUDE    ${qt} ${qt}"                   >> $psi_vars
     $ECHO "setenv MK5_X11_LIB        ${qt} ${qt}"                   >> $psi_vars
     $ECHO "setenv MK5_X11_BIN        ${qt} ${qt}"                   >> $psi_vars
     $ECHO "setenv SOLVE_LIB_PGPLOT   ${qt} ${qt}"                   >> $psi_vars
     $ECHO "setenv SOLVE_LIB_PGPLOT_A ${qt} ${qt}"                   >> $psi_vars
     $ECHO "setenv SOLVE_LIB_XT       ${qt} ${qt}"                   >> $psi_vars
     $ECHO "setenv SOLVE_LIB_XHP11    ${qt} ${qt}"                   >> $psi_vars
endif
if ( $PETOOLS_BLAS_USE == "NO" ) then
     $ECHO "setenv SOLVE_LIB_BLAS     ${qt}$SOLVE_LIB_BLAS${qt}"     >> $psi_vars
endif
#
# --- Read the file with envirnonment variables once more
#
source ${PETOOLS_ROOT}/support/petools_vars
#
# --- Checks available Unix commands and files
#
$ECHO "config.csh: Running checks ... " | tee -a  $CONF_LOG
$PETOOLS_ROOT/support/petools_checks.csh    | tee -a  $CONF_LOG
if ( $status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: Error in attempt to install petools" | tee -a $CONF_LOG
     echo "config.csh: Refer to installation instruction"
     echo "config.csh: Look at file $CONF_LOG for more detail"
     exit 1
endif
#
# --- Complie and link the program which makes inlcude files
#
set PWD_OLD = `pwd`
cd $PETOOLS_ROOT/support
$ECHO "config.csh: making  use_local  ... \c" | tee -a   $CONF_LOG
make -f ${PETOOLS_ROOT}/support/use_local.mak clean  >>& $CONF_LOG
make -f ${PETOOLS_ROOT}/support/use_local.mak all    >>& $CONF_LOG
set mk5_status = $status
if ( $mk5_status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: Error in an attempt to make use_local" | tee -a $CONF_LOG
     echo "config.csh: See file " $CONF_LOG
     exit ( $mk5_status )
endif
make -f ${PETOOLS_ROOT}/support/use_local.mak clean_obj  >>& $CONF_LOG
echo " done" | tee -a  $CONF_LOG
$ECHO "config.csh: Preprocessing diagi.i  ... \c" | tee -a $CONF_LOG
if ( $PETOOLS_BITS == "64" ) then
     cat $PETOOLS_ROOT/include/diagi.templ | sed 's@ADDRESS__TYPE@INTEGER*8@g' \
     >   $PETOOLS_ROOT/include/diagi.i
  else
     cat $PETOOLS_ROOT/include/diagi.templ | sed 's@ADDRESS__TYPE@INTEGER*4@g' \
     >   $PETOOLS_ROOT/include/diagi.i
endif
echo " done" | tee -a  $CONF_LOG
#
$ECHO "config.csh: Setting template files... \c" | tee -a  $CONF_LOG
set TMPFILE = /tmp/$$_make.tmp
set fortran_include = ${PETOOLS_ROOT}/include/solve_paths.i
echo " done" | tee -a  $CONF_LOG
#
# --- Creation of ./include/pgplot_path.i  (for Fortran)
#
$ECHO "config.csh: Creating ${PETOOLS_ROOT}/include/pgplot_path ... \c" | tee -a  $CONF_LOG
if (-f $TMPFILE) then
    rm $TMPFILE
endif
if (-f ${fortran_include} ) then
    rm ${fortran_include}
endif
cat > $TMPFILE <<END_OF_INPUT_I
!
! === This file was genereated automatically by the script
! === $PETOOLS_ROOT/support/config.csh
! === at `date`
!
      INTEGER*4    PGPLOT_DIR_LEN, PGPLOT_SHARE_LEN, PGPLOT_FONT_LEN, HELP_DIR_LEN, MK5_X11_LEN
      PARAMETER  ( PGPLOT_DIR_LEN   = `echo $PGPLOT_DIR|awk '{printf length }'` )
      PARAMETER  ( PGPLOT_SHARE_LEN = `echo $PETOOLS_PREFIX/share|awk '{printf length }'` )
      PARAMETER  ( PGPLOT_FONT_LEN  = `echo $PGPLOT_FONT|awk '{printf length }'` )
      PARAMETER  ( HELP_DIR_LEN     = `echo $PETOOLS_ROOT/doc|awk '{printf length }'` )
      PARAMETER  ( MK5_X11_LEN      = `echo $MK5_X11_BIN|awk '{printf length }'` )
      CHARACTER    PGPLOT_DIR_DEF*(PGPLOT_DIR_LEN),   &
     &             PGPLOT_SHARE_DEF*(PGPLOT_SHARE_LEN), &
     &             PGPLOT_FONT_DEF*(PGPLOT_FONT_LEN), &
     &             HELP_DIR_DEF*(HELP_DIR_LEN), &
     &             MK5_X11_BIN_DEF*(MK5_X11_LEN)
      DATA         PGPLOT_DIR_DEF   / "$PGPLOT_DIR" /
      DATA         PGPLOT_FONT_DEF  / "$PGPLOT_FONT" /
      DATA         HELP_DIR_DEF     / "$PETOOLS_ROOT/doc" /
      DATA         MK5_X11_BIN_DEF  / "$MK5_X11_BIN" /
      DATA         PGPLOT_SHARE_DEF / "$PETOOLS_PREFIX/share" /
END_OF_INPUT_I
cat $TMPFILE > $fortran_include
rm -f $TMPFILE
#
cat > $TMPFILE <<END_OF_INPUT_II
C
C --- Local defintion of the directory where pgplot data are stored
C
      INTEGER*4    PGPLOT_SHARE_LEN
      PARAMETER  ( PGPLOT_SHARE_LEN = `echo $PETOOLS_PREFIX/share/|awk '{printf length }'` )
      CHARACTER    PGPLOT_SHARE_DEF*(PGPLOT_SHARE_LEN)
      PARAMETER (  PGPLOT_SHARE_DEF = "$PETOOLS_PREFIX/share/" )
END_OF_INPUT_II
cat $TMPFILE > ../include/pgplot_local.i
rm -f $TMPFILE
echo " done" | tee -a  $CONF_LOG
#
# --- Make a program for transformation of template files to include files
#
# --- Remove old files
#
$ECHO "config.csh: Creating include files using templates... \c" | tee -a  $CONF_LOG
rm -f  ${PETOOLS_ROOT}/support/use_local.o
rm -f  ${PETOOLS_ROOT}/include/diagi_local.i
#
if ( -f $PETOOLS_LOCAL ) then
  else
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: File $PETOOLS_LOCAL with local prefernces was not found" | tee -a $CONF_LOG
     echo "config.csh: See file " $CONF_LOG
     exit ( 31 )
endif
echo " done" | tee -a  $CONF_LOG
#
# --- Create local include file for DiaGI
#
$ECHO "Making  diagi_local.i ... \c" | tee -a  $CONF_LOG
cat $PETOOLS_LOCAL | \
    sed "s@%%PETOOLS_VERSION_VAL%%@$PETOOLS_VERSION@g"      | \
    sed "s@%%PETOOLS_PREFIX_VAL%%@$PETOOLS_PREFIX@g"          \
    >    ${PETOOLS_ROOT}/include/diagi_local.ii
${PETOOLS_ROOT}/bin/use_local ${PETOOLS_ROOT}/include/diagi_local.templ \
                              ${PETOOLS_ROOT}/include/diagi_local.ii FORTRAN   \
                              ${PETOOLS_ROOT}/include/diagi_local.i
set mk5_status = $status
if ( $mk5_status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: Error in an attempt to make diagi_local" | tee -a $CONF_LOG
     echo "config.csh: See file " $CONF_LOG
     exit ( $mk5_status )
endif
if ( -f ${PETOOLS_ROOT}/include/diagi_local.ii  ) rm ${PETOOLS_ROOT}/include/diagi_local.ii
echo " done" | tee -a  $CONF_LOG
#
# --- Create local include file for matvec
#
$ECHO "Making  pgplot_local.h ... \c" | tee -a  $CONF_LOG
echo 'const char pgplot_dir[] = "'${PETOOLS_PREFIX}'/bin";' > ${PETOOLS_ROOT}/include/pgplot_local.h
echo " done" | tee -a  $CONF_LOG
$ECHO "Making  matvec.i ... \c" | tee -a  $CONF_LOG
${PETOOLS_ROOT}/bin/use_local ${PETOOLS_ROOT}/include/matvec.templ \
                              $PETOOLS_LOCAL FORTRAN   \
                              ${PETOOLS_ROOT}/include/matvec.i
set mk5_status = $status
if ( $mk5_status != 0 ) then
     echo "config.csh: FAILURE" | tee -a $CONF_LOG
     echo "config.csh: Error in an attempt to make matvec" | tee -a $CONF_LOG
     echo "config.csh: See file " $CONF_LOG
     exit ( $mk5_status )
endif
echo " done" | tee -a  $CONF_LOG
#
# --- Purge the trash
#
if (    -f ${PETOOLS_ROOT}/support/use_local_support.o  ) then
     rm -f ${PETOOLS_ROOT}/support/use_local_support.o
endif
#
set PETOOLS__LABEL = `echo $PETOOLS_VERSION | awk '{printf "petools %s%s%s\n", substr($0,0,4), substr($0,5,2), substr($0,7,2)}'`
set pv = ${PETOOLS_ROOT}/pet_util/petools_vers.f
#
echo '      SUBROUTINE PETOOLS_VERS ( VERS_STR )'                      > $pv
echo '      CHARACTER  VERS_STR*(*)'                                  >> $pv
echo '      CHARACTER  PETOOLS__LABEL*30'                             >> $pv
echo "      PETOOLS__LABEL = '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'" | \
      sed "s|@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@|$PETOOLS__LABEL|g"        >> $pv
echo '      VERS_STR = PETOOLS__LABEL'                                >> $pv
echo '      RETURN'                                                   >> $pv
echo '      END  SUBROUTINE PETOOLS_VERS'                             >> $pv
#
set out_file = ${PETOOLS_ROOT}/Makefile
if ( -f $out_file ) rm -f $out_file
cat ${PETOOLS_ROOT}/Makefile.in1 > $out_file
echo " " >> $out_file
$ECHO "#  Locally customized variables" >> $out_file
echo " " >> $out_file
#
$ECHO "COPT                =  ${qt} ${qt}"                    >> $out_file
$ECHO "FOPT                =  ${qt} ${qt}"                    >> $out_file
$ECHO "LDOPT               =  ${qt} ${qt}"                    >> $out_file
$ECHO "PETOOLS_USE         =  $PETOOLS_USE"                   >> $out_file
$ECHO "PETOOLS_ROOT        =  $PETOOLS_ROOT"                  >> $out_file
$ECHO "PETOOLS_PREFIX      =  $PETOOLS_PREFIX"                >> $out_file
$ECHO "PETOOLS_LIB         =  $PETOOLS_LIB"                   >> $out_file
$ECHO "PETOOLS_LIB_A       =  $PETOOLS_LIB_A"                 >> $out_file
$ECHO "PETOOLS_BIN         =  $PETOOLS_PREFIX/bin"            >> $out_file
$ECHO "PETOOLS_BITS        =  $PETOOLS_BITS"                  >> $out_file
$ECHO "PETOOLS_VERSION     =  $PETOOLS_VERSION"               >> $out_file
$ECHO "PETOOLS_BLAS_USE    =  $PETOOLS_BLAS_USE"              >> $out_file
$ECHO "SOLVE_LIB_VEC       =  $SOLVE_LIB_VEC"                 >> $out_file
$ECHO "SOLVE_LIB_BLAS      =  $SOLVE_LIB_BLAS"                >> $out_file
$ECHO "SOLVE_LIB_M         =  $SOLVE_LIB_M"                   >> $out_file
$ECHO "SOLVE_LIB_U77       =  $SOLVE_LIB_U77"                 >> $out_file
$ECHO "SOLVE_LIB_LCL       =  $SOLVE_LIB_LCL"                 >> $out_file
$ECHO "SOLVE_EXTRA_LIB     =  $SOLVE_EXTRA_LIB"               >> $out_file
$ECHO "SOLVE_LIB_CURSES    =  $SOLVE_LIB_CURSES"              >> $out_file
$ECHO "READLINE_LIB        =  $READLINE_LIB"                  >> $out_file
$ECHO "CURSES_LIB          =  $CURSES_LIB"                    >> $out_file
if ( $OS_name == "HP-UX" ) then
   $ECHO "SOLVE_HP_VERSION =  $SOLVE_HP_VERSION"              >> $out_file
endif
$ECHO "PETOOLS_PGPLOT      =  $PETOOLS_PGPLOT"                >> $out_file
if ( $PETOOLS_PGPLOT == "YES" ) then
     $ECHO "PGPLOT_DIR          =  $PGPLOT_DIR"               >> $out_file
     $ECHO "PGPLOT_FONT         =  $PGPLOT_FONT"              >> $out_file
     $ECHO "PGPLOT_DEV          =  $PGPLOT_DEV"               >> $out_file
     $ECHO "PGPLOT_RGB          =  $PGPLOT_RGB"               >> $out_file
     $ECHO "PGPLOT_XW_MARGIN    =       $PGPLOT_XW_MARGIN"    >> $out_file
     $ECHO "SOLVE_PGPLOT_X_INC  =  $SOLVE_PGPLOT_X_INC"       >> $out_file
     $ECHO "SOLVE_LIB_PGPLOT    =  $SOLVE_LIB_PGPLOT"         >> $out_file
     $ECHO "SOLVE_LIB_PGPLOT_A  =  $SOLVE_LIB_PGPLOT_A"       >> $out_file
     $ECHO "MK5_X11_INCLUDE     =  $MK5_X11_INCLUDE"          >> $out_file
     $ECHO "MK5_X11_LIB         =  $MK5_X11_LIB"              >> $out_file
     $ECHO "MK5_X11_BIN         =  $MK5_X11_BIN"              >> $out_file
     $ECHO "SOLVE_LIB_XHP11     =  $SOLVE_LIB_XHP11"          >> $out_file
     $ECHO "SOLVE_LIB_X11       =  $SOLVE_LIB_X11"            >> $out_file
     $ECHO "SOLVE_LIB_XT        =  $SOLVE_LIB_XT"             >> $out_file
  else
     $ECHO "SOLVE_LIB_X11       =   "                         >> $out_file
     $ECHO "MK5_X11_INCLUDE     =   "                         >> $out_file
     $ECHO "MK5_X11_LIB         =   "                         >> $out_file
     $ECHO "MK5_X11_BIN         =   "                         >> $out_file
     $ECHO "SOLVE_LIB_PGPLOT    =   "                         >> $out_file
     $ECHO "SOLVE_LIB_PGPLOT_A  =   "                         >> $out_file
     $ECHO "SOLVE_LIB_XT        =   "                         >> $out_file
     $ECHO "SOLVE_LIB_XHP11     =   "                         >> $out_file
endif
$ECHO "SOLVE_C_INCLUDE     =  $SOLVE_C_INCLUDE"               >> $out_file
$ECHO "MK5_CC              =  $MK5_CC"                        >> $out_file
$ECHO "MK5_FC              =  $MK5_FC"                        >> $out_file
$ECHO "MK5_F95_NOOPT       =  $MK5_F95_NOOPT"                 >> $out_file
if ( $PETOOLS_NOOPT == 1 ) then 
     $ECHO "MK5_F95             =  $MK5_F95_NOOPT"            >> $out_file
     $ECHO "MK5_F95_OPT         =  $MK5_F95_NOOPT"            >> $out_file
     $ECHO "MK5_F95_OPTEST      =  $MK5_F95_NOOPT"            >> $out_file
  else
     $ECHO "MK5_F95             =  $MK5_F95"                  >> $out_file
     $ECHO "MK5_F95_OPT         =  $MK5_F95_OPT"              >> $out_file
     $ECHO "MK5_F95_OPTEST      =  $MK5_F95_OPTEST"           >> $out_file
endif
$ECHO "MK5_C               =  $MK5_C"                         >> $out_file
$ECHO "MK5_LINK            =  $MK5_LINK"                      >> $out_file
$ECHO "MK5_C_LINK          =  $MK5_C_LINK"                    >> $out_file
$ECHO "MK5_COMPILER        =  $MK5_COMPILER"                  >> $out_file
$ECHO "MK5_C_OPENMP        =  $MK5_C_OPENMP"                  >> $out_file
$ECHO "MK5_F_OPENMP        =  $MK5_F_OPENMP"                  >> $out_file
$ECHO "MK5_PREP_DIRS       =  $MK5_PREP_DIRS"                 >> $out_file
$ECHO "PETOOLS_MAKE        =  $PETOOLS_MAKE"                  >> $out_file
$ECHO "PETOOLS_COMPVAR_DEF =  $PETOOLS_COMPVAR_DEF"           >> $out_file
$ECHO "HELP_DIR            =  $HELP_DIR"                      >> $out_file
$ECHO "PETOOLS_OS          =  `uname`"                        >> $out_file
$ECHO "NUM_PROC            = $num_cores"                      >> $out_file
$ECHO "CONF_LOG            = $CONF_LOG"                       >> $out_file
$ECHO "BUILD_LOG           = $BUILD_LOG"                      >> $out_file
#
echo " " >> $out_file
cat ${PETOOLS_ROOT}/Makefile.in2 >> $out_file
$ECHO "Remove stale include files, if present... \c" | tee -a  $CONF_LOG
cd $PETOOLS_ROOT
make uninstall_include >>& $CONF_LOG
#
chmod g+rw,o+r $PETOOLS_ROOT/Makefile
chmod g+rw,o+r $PETOOLS_ROOT/support/petools_vars
chmod g+rw,o+r $PETOOLS_ROOT/support/petools_postinstall_vars
#
echo "ok" | tee -a  $CONF_LOG
echo "config.csh is done"
