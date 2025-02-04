#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   C-shell routine compile_var defines environment variables which    *
# *   are needed for compiling and linking Calc/Solve.                   *
# *                                                                      *
# * ### 02-SEP-2002 compile_var.csh v1.55 (c) L. Petrov 16-APR-2022 ###  *
# *                                                                      *
# ************************************************************************
setenv LANG   C
setenv LC_ALL C
unsetenv PETOOLS_COMP_VAR_DEFINED
if ( $?PETOOLS_ROOT == 0 ) then
     echo "---  compile_var.csh:  Environment variable PETOOLS_ROOT was not defined  ---"
     exit ( 2 )
endif
#
if ( `uname` == "Linux" || `uname` == "Darwin"  ) then
      if ( $?PETOOLS_BITS == 0 ) then
          if ( "`uname -a | grep x86_64`" != "" ) then
                set bits_flag = "-m64"
                set addr_flag = "ADR_64BIT"
                set addr_type = "ADDRESS__TYPE=INTEGER\(8\)"
    	    else
                set bits_flag = "-m32"
                set addr_flag = "ADR_32BIT"
                set addr_type = "ADDRESS__TYPE=INTEGER\(4\)"
          endif
        else
          if ( $PETOOLS_BITS == 64 ) then
                set bits_flag = "-m64"
                set addr_flag = "ADR_64BIT"
                set addr_type = "ADDRESS__TYPE=INTEGER\(8\)"
              else
                set bits_flag = "-m32"
                set addr_flag = "ADR_32BIT"
                set addr_type = "ADDRESS__TYPE=INTEGER\(4\)"
          endif
      endif
endif
#
set hp_f95_min_vers    = "2.4"
set intel_f95_min_vers = "8.1"
set gnu_f95_min_vers   = "4.3.0"
set sun_f95_min_vers   = "6.0"
setenv COMPILE_VAR_VERSION "2020.10.26"
#
# --- Learn compiler vendor name
#
set f95_vendor  = "`${PETOOLS_ROOT}/support/f95_version.csh vendor $MK5_FC`"
if ( $status != 0 ) then
     echo "compiler_var.csh: Failure in an attempt to determine the vendor of Fortran compiler"
     exit 1
endif
#
set f95_version = "`${PETOOLS_ROOT}/support/f95_version.csh version $MK5_FC`"
if ( $status != 0 ) then
     echo "compiler_var.csh: Failure in an attempt to determine the version of Fortran compiler"
     exit 1
endif
#
set cc_vendor   = "`${PETOOLS_ROOT}/support/cc_version.csh vendor $MK5_FC`"
if ( $status != 0 ) then
     echo "compiler_var.csh: Failure in an attempt to determine the vendor of C compiler"
     exit 1
endif
#
set cc_version  = "`${PETOOLS_ROOT}/support/cc_version.csh version $MK5_FC`"
if ( $status != 0 ) then
     echo "compiler_var.csh: Failure in an attempt to determine the version of C compiler"
     exit 1
endif
#
if ( $?NO_ENDIAN_CHECK ) then
     set ENDIAN_D_FLAG = "UNKNOWN_ENDIAN"
  else
     if ( -x ${PETOOLS_ROOT}/bin/check_endian ) then
          set ENDIAN_D_FLAG = `${PETOOLS_ROOT}/bin/check_endian`
        else
          echo "compiler_var.csh: Fatal error:"
          echo "compiler_var.csh: Program ${PETOOLS_ROOT}/bin/check_endian was not found "
          echo "compiler_var.csh: Please, run ${PETOOLS_ROOT}/support/config.csh"
	  exit 1
     endif
endif
unsetenv NO_ENDIAN_CHECK
#
if ( $?NO_TRUE_FALSE_CHECK  ) then
     set FORTRAN_TRUE  = "1"
     set FORTRAN_FALSE = "0"
  else
     if ( -x ${PETOOLS_ROOT}/bin/learn_fortran_true ) then
          set FORTRAN_TRUE = `${PETOOLS_ROOT}/bin/learn_fortran_true`
        else
          echo "compiler_var.csh: Fatal error:"
          echo "compiler_var.csh: Program ${PETOOLS_ROOT}/bin/learn_fortran_true was not found "
          echo "compiler_var.csh: Please, run ${PETOOLS_ROOT}/support/config.csh"
	  exit 1
     endif
#
     if ( -x ${PETOOLS_ROOT}/bin/learn_fortran_false ) then
          set FORTRAN_FALSE = `${PETOOLS_ROOT}/bin/learn_fortran_false`
        else
          echo "compiler_var.csh: Fatal error:"
          echo "compiler_var.csh: Program ${PETOOLS_ROOT}/bin/learn_fortran_false was not found "
          echo "compiler_var.csh: Please, run ${PETOOLS_ROOT}/support/config.csh"
	  exit 1
     endif
endif
unsetenv NO_TRUE_FALSE_CHECK
setenv MK5_C_OPENMP ""
setenv MK5_F_OPENMP ""
#
if ( `uname` == Linux  && "$f95_vendor" == "Intel(R)" ) then
     setenv MK5_C_OPENMP "-fopenmp"
     setenv MK5_F_OPENMP "-openmp"
#
# ===========================
#    ||  INTEL COMPILER    ||
# ===========================
#
#
# -- Check version number
#
     set first_char = `expr substr $f95_version 1 1`
     if ( "$first_char" == "v" ) then
          set len = `expr length $f95_version`
	  set len = `expr $len - 1`
          set f95_version = `expr substr $f95_version 2 $len`
     endif
     if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh $f95_version $intel_f95_min_vers` == 0    ) then
#
          echo "You have version of Fortran95 compiler $f95_version"
          echo "but version $intel_f95_min_vers or higher is required"
          echo "FATAL ERROR: compile_var.csh"
	  exit 1
     endif
#
# -- Check whether we can apply Intel specific optimizations
#
     cat /proc/cpuinfo | grep GenuineIntel >& /dev/null
     set genuine_pentium = $status
     set f95_version = `${PETOOLS_ROOT}/support/f95_version.csh version`
     if ( $genuine_pentium == 0 ) then
#
# ------- Aga, this is Intel processor
#
          cat /proc/cpuinfo | grep sse2 >& /dev/null
          set sse2_enable = $status
          if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh \
               $f95_version 10.0` == 0    ) then
#
# ------------ Set Intel specific Penituim 4 optimizaition
#
               if ( $sse2_enable == 0 ) then
                    set ARCH_OPT = ( -tpp7 )
                 else
                    set ARCH_OPT = ( -tpp6 )
               endif
	     else
               set ARCH_OPT = ""
          endif
#
# ------- Check whether we can uset Core Duo or Prescott optimizations
#
	  cat /proc/cpuinfo | grep "Intel(R) Core(TM)" >& /dev/null
          set axt_status = $status
          cat /proc/cpuinfo | grep pni >& /dev/null
          set pni_status = $status
          cat /proc/cpuinfo | grep Pentium >& /dev/null
          set pentium_status = $status
          cat /proc/cpuinfo | grep Xeon >& /dev/null
          set xeon_status = $status
	  if ( $axt_status == 0 ) then
               set ARCH_OPT = ( "$ARCH_OPT" -axT -xT )
	     else
               if ( $pni_status == 0 && $pentium_status == 0) then
                   set ARCH_OPT = ( "$ARCH_OPT" -axP -xP )
               endif
          endif
        else
          set ARCH_OPT = ""
     endif
#
     setenv MK5_COMPILER LINUX_INTEL
     setenv MK5_PREP_DIRECTIVES_UNDSC   ${PETOOLS_ROOT}/include/mk5_linux_intel_underscore.inc
     setenv MK5_PREP_DIRECTIVES_NOUNDSC ${PETOOLS_ROOT}/include/mk5_linux_intel_nounderscore.inc
     if ( `${PETOOLS_ROOT}/support/version_equal_or_greater.csh "$f95_version" "7.1"` == 0 ) then
          set qlocation = "-Qlocation,fpp,$PETOOLS_ROOT/support"
          set qoption   = "-Qoption,fpp,-traditional,-I${PETOOLS_ROOT}/include,-DLINUX,-DINTEL,-D$ENDIAN_D_FLAG"
	else
          set qlocation = ""
          set qoption   = "-D LINUX -D INTEL -D $ENDIAN_D_FLAG"
     endif
     if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh "$f95_version" "9.0"` == 1 ) then
	  set Zp_align = "-Zp16"
	else
	  set Zp_align = "-Zp"
     endif
#
     set f95_flags = \
     ( \
       -FR \
       -fpp \
       -openmp \
       $Zp_align \
       -align all \
       -assume noprotect_constants \
       -assume byterec \
       -check bounds \
       -pad  \
       -nbs \
       -w90 \
       -w95 \
       -nus \
       -cm  \
       -fPIC \
        $qlocation \
        $qoption \
       -traceback \
       -D $ENDIAN_D_FLAG \
       -D "$addr_type" \
       -D $addr_flag \
       -D QSORT=QSORT_ \
       -D qsort=QSORT_ \
       -D CTIME=CTIME_ \
       -D ctime=CTIME_ \
       -D SLEEP=SLEEP_ \
       -D sleep=SLEEP_ \
       -D RENAME=RENAME_ \
       -D rename=RENAME_ \
       -D READDIR=READDIR64 \
       -D readdir=readdir64 \
       -module ${PETOOLS_ROOT}/module \
       -I ./ \
       -I ../include \
     )
#
     set f95_flags_norm_opt = \
     ( \
       "$ARCH_OPT" \
       -O3 \
     )
#
     set f95_flags_high_opt = \
     ( \
       -FR \
       -fpp \
       -fPIC \
       $Zp_align \
       -align all \
       -assume byterec \
       -pad  \
       "$ARCH_OPT" \
       -O3 \
       -nbs \
       -w90 \
       -w95 \
       -nus \
       -cm  \
       -traceback \
       -complex-limited-range \
       $qlocation \
       $qoption \
       -D $ENDIAN_D_FLAG \
       -D "$addr_type" \
       -D $addr_flag \
       -D QSORT=QSORT_ \
       -D qsort=QSORT_ \
       -D CTIME=CTIME_ \
       -D ctime=CTIME_ \
       -D SLEEP=SLEEP_ \
       -D sleep=SLEEP_ \
       -D RENAME=RENAME_ \
       -D rename=RENAME_ \
       -D READDIR=READDIR64 \
       -D readdir=readdir64 \
       -module ${PETOOLS_ROOT}/module \
       -I ./ \
       -I ../include \
     )
#
     set copt = \
                ( \
		  "$bits_flag" \
                  -fPIC \
                  -D LINUX \
                  -D $ENDIAN_D_FLAG \
                  -D FORTRAN_TRUE=$FORTRAN_TRUE \
                  -D FORTRAN_FALSE=$FORTRAN_FALSE \
                  -D _FILE_OFFSET_BITS=64 \
                  -D _LARGEFILE_SOURCE \
		  -fopenmp \
                  -I ./ \
                  -I ../include \
                  -I /usr/include \
                  -I /usr/local/include \
                )
#
     setenv COPT   "$copt"
     if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh "$f95_version" "10.1"` == 1 ) then
          setenv LDOPT  "-Vaxlib -static-libgcc -openmp -Xlinker -zmuldefs"
       else if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh "$f95_version" "10.0"` == 1 ) then
          setenv LDOPT  "-Vaxlib -posixlib -static-libcxa -Xlinker -zmuldefs"
       else
          setenv LDOPT  "-Vaxlib -static-libcxa -openmp -Xlinker -zmuldefs"
     endif
#
     unsetenv FOPT
     setenv MK5_F95_NOOPT  "$MK5_FC  $f95_flags"
     setenv MK5_F95        "$MK5_FC  $f95_flags $f95_flags_norm_opt"
     setenv MK5_F95_OPT    "$MK5_FC  $f95_flags_high_opt"
     setenv MK5_F95_OPTEST "$MK5_FC  $f95_flags_high_opt"
     setenv MK5_C          "$MK5_CC  $COPT"
     setenv MK5_LINK       "$MK5_FC  $LDOPT"
     setenv MK5_C_LINK     "$MK5_FC  $LDOPT -nofor-main"
#
  else if ( ( `uname` == "Linux" || `uname` == "Darwin" ) && \
            "$f95_vendor" == "GNU") then
#
# ===========================
#    ||  GNU   COMPILER    ||
# ===========================
#
     setenv MK5_C_OPENMP "-fopenmp"
     setenv MK5_F_OPENMP "-fopenmp"
#
# -- Check version number
#
#     set first_char = `expr substr $f95_version 1 1`
     set first_char = `echo $f95_version | awk '{print substr($0,1,1)}'`
     if ( "$first_char" == "v" ) then
          set len = `expr length $f95_version`
	  set len = `expr $len - 1`
          set f95_version = `expr substr $f95_version 2 $len`
     endif
     if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh $f95_version $gnu_f95_min_vers` == 0    ) then
#
          echo "You have version of Fortran95 compiler $f95_version"
          echo "but version $gnu_f95_min_vers or higher is required"
          echo "FATAL ERROR: compile_var.csh"
	  exit 1
     endif
     if ( `uname` == "Linux" ) then
          set str = "`as --version | head -1`"
	  set as_version = `echo $str | awk 'NF>1{print $NF}'`
          if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh $as_version "2.18.0.0"` == 1 ) then
#
# ------------ Option exceptions is incompatible with fopenmp and bound_checks if
# ------------ assembler is older than 2.18.
#
               set opt_exceptions = "-fexceptions"
	     else 
               set opt_exceptions = "" 
          endif
        else
          set opt_exceptions = "-fexceptions"
     endif
#
     setenv MK5_COMPILER LINUX_GNU
#
# -fbacktrace
#
     if ( `uname` == "Linux" ) then
          set OS_str = "-D LINUX"
          setenv MK5_PREP_DIRECTIVES_UNDSC   ${PETOOLS_ROOT}/include/mk5_linux_intel_underscore.inc
          setenv MK5_PREP_DIRECTIVES_NOUNDSC ${PETOOLS_ROOT}/include/mk5_linux_intel_nounderscore.inc
     else if ( `uname` == "Darwin"  ) then
          set OS_str = "-D DARWIN"
          setenv MK5_PREP_DIRECTIVES_UNDSC   ${PETOOLS_ROOT}/include/mk5_darwin_intel_underscore.inc
          setenv MK5_PREP_DIRECTIVES_NOUNDSC ${PETOOLS_ROOT}/include/mk5_darwin_intel_nounderscore.inc
     endif 
     set f95_flags_base = \
     ( \
        "$bits_flag" \
       -ffree-form \
       -ffree-line-length-none \
       -fmax-errors=16 \
       -fno-underscoring \
       -fdollar-ok \
       -fopenmp \
       -x f95-cpp-input \
       -finit-integer=-2147483647 \
       -finit-logical=false \
       -finit-real=nan \
       -finit-character=0 \
       -fbacktrace \
       -ftrapv \
       -fPIC \
       -ffpe-trap=overflow,invalid,zero \
       $opt_exceptions \
       -fPIC \
       $OS_str \
       -D GNU \
       -D $ENDIAN_D_FLAG \
       -D "$addr_type" \
       -D $addr_flag \
     )
     if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh $f95_version "5.1.0"` == 1 ) then
          set f95_flags_base = ( $f95_flags_base -fno-diagnostics-color )
     endif
     if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh $f95_version "10.0.0"` == 1 ) then
          set f95_flags_base = ( $f95_flags_base -fallow-argument-mismatch )
     endif
#
     if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh $f95_version "4.4.0"` == 1 ) then
           set f95_flags_02 = "-J ${PETOOLS_ROOT}/module"
       else
           set f95_flags_02 = "-M ${PETOOLS_ROOT}/module"
     endif
#
     set f95_flags_03 = \
     ( \
       -I ./ \
       -I ../include \
     )
#
     if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh $f95_version "4.5.0"` == 1 ) then
           set f95_flags_no_opt = \
              ( \
                -fcheck=all \
                -O0 \
                -g  \
              )
        else 
           set f95_flags_no_opt = \
              ( \
                -O0 \
                -fbounds-check \
                -g \
              )
     endif
#
     if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh $f95_version "4.5.0"` == 1 ) then
           set f95_flags_norm_opt = \
              ( \
                -fcheck=all \
              )
       else 
           set f95_flags_norm_opt = \
              ( \
                -fbounds-check \
              )
     endif
#
     set f95_flags_high_opt = \
     ( \
        "$bits_flag" \
       -ffree-form \
       -ffree-line-length-none \
       -fmax-errors=16 \
       -fno-underscoring \
       -fdollar-ok \
       -fopenmp \
       -x f95-cpp-input \
       -finit-integer=-2147483647 \
       -finit-logical=false\
       -finit-real=nan \
       -finit-character=0 \
       -fbacktrace \
       -fPIC \
       -mtune=native \
       $OS_str \
       -D GNU \
       -D $ENDIAN_D_FLAG \
       -D "$addr_type" \
       -D $addr_flag \
     )
     if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh $f95_version "5.1.0"` == 1 ) then
          set f95_flags_high_opt = ( $f95_flags_high_opt -fno-diagnostics-color )
     endif
     if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh $f95_version "10.0.0"` == 1 ) then
          set f95_flags_high_opt = ( $f95_flags_high_opt -fallow-argument-mismatch )
     endif
     set f95_flags_high_extra_opt = ""
     set f95_flags_highest_extra_opt = ""
     set f95_flags_extra_opt = "-O2"
     if ( `uname` == "Linux" ) then
          if ( `cat /proc/cpuinfo | grep flags | sort -u | grep avx2` != "" ) then
               set f95_flags_high_opt = ( $f95_flags_high_opt -mavx2 )
               set f95_flags_extra_opt = "-O2 -mavx2"
          endif
       else if ( `uname` == "Darwin" ) then
          if ( `sysctl -n machdep.cpu.features | grep AVX2` != "" ) then
               set f95_flags_high_opt = ( $f95_flags_high_opt -mavx2 )
               set f95_flags_extra_opt = "-O2 -mavx2"
          endif
     endif
     if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh $f95_version "4.7.0"` == 1 ) then
#
#  ------- Version 4.7.2 has a bug. Option Ofast may cause crashes.
#
#          $f95_flags_high_opt -Ofast \
#
	   if ( `uname` == "Linux" ) then
                 if ( "`cat /proc/cpuinfo | grep flag | sort -u | grep avx`" != "" ) then
                      set f95_flags_high_extra_opt = \
                          ( \
                            $f95_flags_high_extra_opt -mavx \
                          )
                 endif
             else if ( `uname` == "Darwin" ) then
                 if ( `sysctl -n machdep.cpu.features | grep AVX` != "" ) then
                      set f95_flags_high_extra_opt = \
                          ( \
                            $f95_flags_high_extra_opt -mavx \
                          )
                 endif
           endif
           set f95_flags_highest_extra_opt = \
               ( \
                   $f95_flags_high_extra_opt "-Ofast -ftree-vectorize" \
               \ )
           set f95_flags_high_extra_opt = \
               ( \
                   $f95_flags_high_extra_opt "-O3" \
               \ )
        else       
           set f95_flags_high_extra_opt    = ( "-O2" )
           set f95_flags_highest_extra_opt = ( "-O2" )
     endif
#
     set copt = \
                ( \
		   "$bits_flag" \
                  -mtune=native \
		  -fopenmp \
                  -fPIC \
                  $OS_str \
                  -D $ENDIAN_D_FLAG \
                  -D FORTRAN_TRUE=$FORTRAN_TRUE \
                  -D FORTRAN_FALSE=$FORTRAN_FALSE \
                  -D _FILE_OFFSET_BITS=64 \
                  -D _LARGEFILE_SOURCE \
                  -I ./ \
                  -I ../include \
                  -I /usr/include \
                  -I /usr/local/include \
                )
#
     if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh $cc_version "5.1.0"` == 1 ) then
          set copt = ( $copt -fno-diagnostics-color )
     endif
     setenv COPT   "$copt"
#
# -- The differences betwee LDOPT and LDOPT_CLINK, is that LDOPT_CLINK is 
# -- also suitable for creation of a shared library
#
     if ( `uname` == "Linux" ) then
           setenv LDOPT        "$bits_flag -fopenmp -Wl,--gc-sections,--no-warn-search-mismatch"
           setenv LDOPT_CLINK  "$LDOPT"
	 else if ( `uname` == "Darwin" ) then
           setenv LDOPT_CLINK  "$bits_flag -fopenmp -Wl,-dead_strip"
           setenv LDOPT        "$bits_flag -fopenmp -Wl,-dead_strip,-stack_size,0x10000000000"
     endif
#
     unsetenv FOPT
     setenv MK5_F95_NOOPT  "$MK5_FC  $f95_flags_base $f95_flags_02 $f95_flags_03 $f95_flags_no_opt"
     setenv MK5_F95        "$MK5_FC  $f95_flags_base $f95_flags_02 $f95_flags_03 $f95_flags_norm_opt $f95_flags_extra_opt"
     setenv MK5_F95_OPT    "$MK5_FC  $f95_flags_high_opt $f95_flags_02 $f95_flags_03 $f95_flags_high_extra_opt"
     setenv MK5_F95_OPTEST "$MK5_FC  $f95_flags_high_opt $f95_flags_02 $f95_flags_03 $f95_flags_highest_extra_opt"
     setenv MK5_C          "$MK5_CC  $COPT"
     setenv MK5_LINK       "$MK5_FC  $LDOPT"
     setenv MK5_C_LINK     "$MK5_FC  $LDOPT_CLINK"
  else if ( `uname` == HP-UX && $f95_vendor == "HP" ) then
     setenv MK5_C_OPENMP "-fopenmp"
     setenv MK5_F_OPENMP "-openmp"
#
# =======================
#   ||   SUN compiler  ||
# =======================
#
# -- Check version number
#
     set tmp_version        = /tmp/__version__$$
     set tmp_version_sorted = /tmp/__version__sorted__$$
     set first_char = `expr substr $f95_version 1 1`
     if ( "$first_char" == "v" ) then
          set len = `expr length $f95_version`
	  set len = `expr $len - 1`
          set f95_version = `expr substr $f95_version 2 $len`
     endif
     if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh  \
           $f95_version $hp_f95_min_vers` == 0 ) then
#
          echo "You have version of Fortran95 compiler $f95_version"
          echo "but version $hp_f95_min_vers or higher is required"
          echo "FATAL ERROR: compile_var.csh"
	  exit 1
     endif
     setenv MK5_PREP_DIRECTIVES_UNDSC   ${PETOOLS_ROOT}/include/mk5_hpux.inc
     setenv MK5_PREP_DIRECTIVES_NOUNDSC ${PETOOLS_ROOT}/include/mk5_hpux.inc
     set f95_flags = \
         (  \
            +cpp=yes \
            -D HPUX \
            -D $ENDIAN_D_FLAG \
            -D ADDRESS__TYPE=\'INTEGER\(4\)\' \
            -D ADR_32BIT \
            -D GETARG=GETARG_ \
            -D getarg=GETARG_ \
            +source=free \
            +check=all \
            +fp_exception \
            +extend_source \
	    +noppu \
            +U77 \
           -I ./ \
         )
     set f95_flags_norm_opt = \
         (  \
            +O2 \
         )
#
     set f95_flags_high_opt = \
         (  \
            +cpp=yes \
            -D HPUX \
            -D ADDRESS__TYPE=\'INTEGER\(4\)\' \
            -D ADR_32BIT \
            -D $ENDIAN_D_FLAG \
            -D GETARG=GETARG_ \
            -D getarg=GETARG_ \
            +source=free \
            +check=none \
            +O3 \
            +Oaggressive \
            +fp_exception \
            +extend_source \
	    +noppu \
            +U77 \
            -I ./ \
         )
#
     set copt = \
         ( \
           -Aa \
           -Ae \
           -D HPUX \
           -D $ENDIAN_D_FLAG \
           -D FORTRAN_TRUE=$FORTRAN_TRUE \
           -D FORTRAN_FALSE=$FORTRAN_FALSE \
	   -I ${PETOOLS_ROOT}/libs/hopslib/include \
           -I ./ \
         )
#
     unsetenv FOPT
     setenv COPT "$copt"
     setenv LDOPT   "+U77 -N -Wl,+vnocompatwarnings"
#
     setenv MK5_F95        "$MK5_FC $f95_flags $f95_flags_norm_opt"
     setenv MK5_F95_NOOPT  "$MK5_FC $f95_flags"
     setenv MK5_F95_OPT    "$MK5_FC $f95_flags_high_opt"
     setenv MK5_F95_OPTEST "$MK5_FC $f95_flags_high_opt"
     setenv MK5_C          "$MK5_CC $COPT"
     setenv MK5_LINK       "$MK5_FC $LDOPT"
     setenv MK5_C_LINK     "$MK5_FC $LDOPT"
     setenv MK5_COMPILER  HP_F90
  else if ( `uname` == "SunOS" && $f95_vendor == "Sun" ) then
     setenv MK5_C_OPENMP "-fopenmp"
     setenv MK5_F_OPENMP "-openmp"
     set first_char = `echo "$f95_version"  | awk '{print substr ($1, 1, 1)}'`
     if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh  $f95_version $sun_f95_min_vers` == 0    ) then
#
          echo "You have version of Fortran95 compiler $f95_version"
          echo "but version $sun_f95_min_vers or higher is required"
          echo "FATAL ERROR: compile_var.csh"
	  exit 1
     endif
     set copt = \
                ( \
                  -D SUN \
                  -D $ENDIAN_D_FLAG \
                  -D ADDRESS__TYPE=\'INTEGER\(4\)\' \
                  -D ADR_32BIT \
                  -D FORTRAN_TRUE=$FORTRAN_TRUE \
                  -D FORTRAN_FALSE=$FORTRAN_FALSE \
                  -I /usr/include \
                  -I /usr/local/include \
                  -I ./ \
                )
     setenv COPT   "$copt"
     setenv MK5_C  "$MK5_CC $COPT"
#
     which fpversion >& /dev/null
     if ( $status == 0 ) then
#
# ------- Special trick. SUN-os has the program fpversion.
# ------- It tells which architecture options should be used for compiling.
# ------- Omitting these options may result in generating code which will be
# ------- 10-100 times slower
#
          set arch_opt = `fpversion | grep Use | awk '{printf "%s %s %s %s %s %s\n", $2, $3, $4, $5, $6, $7}' | sed 's@"@@g' | sed s/"code-generation option."/""/`
       else
          set arch_opt = ""
     endif
#
     set f95_flags = \
         (  \
	    -free \
            -fpp \
	    -ext_names=plain \
	     $arch_opt \
            -D SUN \
            -D ILEN=ILEN__SUN \
            -D ADDRESS__TYPE=\'INTEGER\(4\)\' \
            -D ADR_32BIT \
            -D ilen=ilen__sun \
            -D $ENDIAN_D_FLAG \
            -D GETARG=GETARG_ \
            -D getarg=GETARG_ \
            -D FREE=FREE_ \
            -D free=FREE_ \
            -D ETIME=ETIME_ \
            -D FDATE=FDATE_ \
            -D FLUSH=FLUSH_ \
            -D IARGC=IARGC_ \
            -D RAN=RAN_ \
            -D FTELL=FTELL_ \
            -D SECNDS=SECNDS_ \
            -D IDATE=IDATE_ \
            -D etime=ETIME_ \
            -D fdate=FDATE_ \
            -D flush=FLUSH_ \
            -D iargc=IARGC_ \
            -D ran=RAN_ \
            -D ftell=FTELL_ \
            -D secnds=SECNDS_ \
            -D idate=IDATE_ \
	    -D SLEEP=SLEEP_ \
	    -D sleep=SLEEP_ \
	    -D RENAME=RENAME_ \
	    -D rename=RENAME_ \
            -I./ \
         )
     set f95_flags_opt = \
         (  \
	    -O5 \
	    -free \
            -fpp \
	    -ext_names=plain \
	     $arch_opt \
            -D SUN \
            -D ILEN=ILEN__SUN \
            -D ADDRESS__TYPE=\'INTEGER\(4\)\' \
            -D ADR_32BIT \
            -D ilen=ilen__sun \
            -D $ENDIAN_D_FLAG \
            -D GETARG=GETARG_ \
            -D getarg=GETARG_ \
            -D FREE=FREE_ \
            -D free=FREE_ \
            -D ETIME=ETIME_ \
            -D FDATE=FDATE_ \
            -D FLUSH=FLUSH_ \
            -D IARGC=IARGC_ \
            -D RAN=RAN_ \
            -D FTELL=FTELL_ \
            -D SECNDS=SECNDS_ \
            -D IDATE=IDATE_ \
            -D etime=ETIME_ \
            -D fdate=FDATE_ \
            -D flush=FLUSH_ \
            -D iargc=IARGC_ \
            -D ran=RAN_ \
            -D ftell=FTELL_ \
            -D secnds=SECNDS_ \
            -D idate=IDATE_ \
	    -D SLEEP=SLEEP_ \
	    -D sleep=SLEEP_ \
	    -D RENAME=RENAME_ \
	    -D rename=RENAME_ \
            -I./ \
         )
     set f95_flags_high_opt = \
         (  \
	    -O5 \
	    -free \
            -fpp \
	    -ext_names=plain \
	     $arch_opt \
            -D SUN \
            -D ADDRESS__TYPE=\'INTEGER\(4\)\' \
            -D ADR_32BIT \
            -D ILEN=ILEN__SUN \
            -D ilen=ilen__sun \
            -D $ENDIAN_D_FLAG \
            -D GETARG=GETARG_ \
            -D getarg=GETARG_ \
            -D FREE=FREE_ \
            -D free=FREE_ \
            -D ETIME=ETIME_ \
            -D FDATE=FDATE_ \
            -D FLUSH=FLUSH_ \
            -D IARGC=IARGC_ \
            -D RAN=RAN_ \
            -D FTELL=FTELL_ \
            -D SECNDS=SECNDS_ \
            -D IDATE=IDATE_ \
            -D etime=ETIME_ \
            -D fdate=FDATE_ \
            -D flush=FLUSH_ \
            -D iargc=IARGC_ \
            -D ran=RAN_ \
            -D ftell=FTELL_ \
            -D secnds=SECNDS_ \
            -D idate=IDATE_ \
	    -D SLEEP=SLEEP_ \
	    -D sleep=SLEEP_ \
	    -D RENAME=RENAME_ \
	    -D rename=RENAME_ \
            -I./ \
         )
#
     unsetenv FOPT
     setenv LDOPT         " "
     setenv MK5_F95       "$MK5_FC $f95_flags"
     setenv MK5_F95_NOOPT "$MK5_FC $f95_flags"
     setenv MK5_F95_OPT   "$MK5_FC $f95_flags_opt"
     setenv MK5_LINK      "$MK5_FC $LDOPT"
     setenv MK5_C_LINK    "$MK5_FC $LDOPT"
     setenv MK5_COMPILER   SUN_F95
     setenv MK5_PRE_DIR                 ${PETOOLS_ROOT}/include/mk5_sunos.inc
     setenv MK5_PREP_DIRECTIVES_UNDSC   ${PETOOLS_ROOT}/include/mk5_sunos.inc
     setenv MK5_PREP_DIRECTIVES_NOUNDSC ${PETOOLS_ROOT}/include/mk5_sunos.inc
  else
     echo "Unknown compiler vendor $f95_vendor for operating system `uname`"
     exit 1
endif
#
# --- If there is a local file with compilation variables, let's source it
#
setenv local_comp $PETOOLS_ROOT/local/comp.lcl
if ( -f $local_comp  ) then
     source $local_comp
endif
setenv PETOOLS_COMPVAR_DEF "yes"
