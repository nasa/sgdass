#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Script  make_pgplot522  makes pgplot522  for using it in PETOOLS.  *
# *                                                                      *
# * ###  18-JUN-1998  make_pgplot522  v6.2 (c) L. Petrov 31-JAN-2025 ### *
# *                                                                      *
# ************************************************************************
unalias mv
unalias rm
#
if ( $1 == "" ) then
     echo "Usage: make_pgplot522 {prefix} {output_log_file}"
     exit 1
endif
if ( $?PETOOLS_ROOT == 0 ) then
     echo "--- make_pgplot522:  Environment variable PETOOLS_ROOT was not defined  ---"
     exit 1
endif
if ( -f ${PETOOLS_ROOT}/lib/libpgplot.a ) then
     exit 0
endif 
setenv PREFIX $1
setenv LOG    $2
setenv SUPPORT ${PETOOLS_ROOT}/support
if ( $?MK5_FC == 0 ) then
     setenv MK5_FC `${PETOOLS_ROOT}/support/fortran_compiler_guess.csh`
endif
setenv MK5_CC `echo $MK5_C | awk '{print $1}'`
echo "---  pgplot522  is being compiled and linked now  ---"
echo "---             used fortran compiler: $MK5_FC"
echo "---             used c       compiler: $MK5_CC"
#
# --- Temporary
#
if ( ${?NULL} == "0" ) then
     set null_device = /dev/null
  else
     set null_device = $NULL
endif
set temp_make_pgplot522 = /tmp/make_pgplot__$$
if ( -f $temp_make_pgplot522 ) then
     rm -f $temp_make_pgplot522
endif
#
# --- Save current directory name
#
set PWD_SAVE=`pwd`
set COM=$0
if ( $COM:h != $COM ) then
  cd $COM:h
endif
#
# --- Go to pgplot
#
cd ${PETOOLS_ROOT}/pgplot
#
# --- Create build directory or purge it if it already exists
#
if ( -d ./build ) then
     rm -fR ./build/* >&! $null_device
  else
     mkdir build
endif
#
cd ${PETOOLS_ROOT}/pgplot/build
#
# --- Copy to the build directory several files needed for installation
#
cp ../makemake     ./
cp ${SUPPORT}/pgplot522_driver.list ./drivers.list
if ( -f $LOG ) then
   else
     set DATE_LONG = `date "+%d-%h-%Y %H:%M:%S" | tr "[a-z]" "[A-Z]"`
     echo $DATE_LONG > $LOG
endif
#
source ${SUPPORT}/search_X11.csh
#
set f95_vendor  = "`${PETOOLS_ROOT}/support/f95_version.csh vendor`"
#
if ( `uname` == "Darwin"  ) then
      set sedi = 'sed -i ""'
   else
      set sedi = 'sed -i'
endif
if ( `uname` == "Linux" || `uname` == "Darwin"  ) then
      if ( $?PETOOLS_BITS == 0 ) then
           if ( "`uname -a | grep x86_64`" != "" ) then
                set bits_flag = "-m64"
    	    else
                set bits_flag = "-m32"
          endif
        else
          if ( "$PETOOLS_BITS" == "64" ) then
               set bits_flag = "-m64"
             else
               set bits_flag = "-m32"
          endif
      endif
endif
#
if ( ( `uname` == "Linux" || `uname` == "Darwin" ) && "$f95_vendor" == "GNU" ) then
#
# === Linux branch, GNU Fortran compiler ===
#
      echo "Current directory `pwd`" >>& $LOG
#
# --- Create system/compiler specific directory if it does not exist
#
      if ( -d ../sys_linux/gfortran_f95_src ) then#

# -------- ... otherwise purge it
#
           rm -f ../sys_linux/gfortran_f95_src/* >&! $null_device
         else
	   if ( -d ../sys_linux ) then
             else
                mkdir ../sys_linux
	   endif
           mkdir ../sys_linux/gfortran_f95_src
      endif
#
# --- Copy configuration file
#
      if ( "$bits_flag" == "-m64" ) then
           cp ${SUPPORT}/m64_f95_gfortran_gcc.conf    ${PETOOLS_ROOT}/pgplot/sys_linux/mk5_f95_gfortran_gcc.conf    
         else 
           cp ${SUPPORT}/mk5_f95_gfortran_gcc.conf    ${PETOOLS_ROOT}/pgplot/sys_linux/
      endif
#
# --- Copy system/compiler specific files there
#
      if ( -d ${PETOOLS_ROOT}/pgplot_gfortran_f95_src ) then
           cp -p ${PETOOLS_ROOT}/pgplot_gfortran_f95_src/* ../sys_linux/gfortran_f95_src/
      endif
#
      echo "--- beginning of file ${PETOOLS_ROOT}/pgplot/sys_linux/mk5_f95_gfortran_gcc.conf ---" >>& $LOG
      cat  ${PETOOLS_ROOT}/pgplot/sys_linux/mk5_f95_gfortran_gcc.conf >>& $LOG
      echo "--- end of file ${PETOOLS_ROOT}/pgplot/sys_linux/mk5_f95_gfortran_gcc.conf ---" >>& $LOG
      echo "--- beginning of file PETOOLS environment variables list ---" >>& $LOG
      env | grep PETOOLS >>& $LOG
      echo "--- end of file PETOOLS environment variables list ---" >>& $LOG
      $sedi  's@PGPLOT_LIB -lpng -lz@PGPLOT_LIB@g' ./makemake
#
# --- Build the make file
#
      ./makemake ${PETOOLS_ROOT}/pgplot     linux mk5_f95_gfortran_gcc      >>& $LOG
      set str=`grep FCOMPL ../sys_linux/mk5_f95_gfortran_gcc.conf | sed 's@"@ @g' | sed 's@=@ @' | sed s@-m32@$bits_flag@g`
      if ( "$bits_flag" == "-m64" ) then
           $sedi "s@-m32@$bits_flag@g"         ./makefile
           $sedi "s@-DADR_32BIT@-DADR_64BIT@g" ./makefile
           set str=`echo $str | sed "s@-DADR_32BIT@-DADR_64BIT@g"`
      endif
      set f95_vendor  = "`${PETOOLS_ROOT}/support/f95_version.csh vendor $MK5_FC`"
      set f95_version = "`${PETOOLS_ROOT}/support/f95_version.csh version $MK5_FC`"
      if ( "$f95_vendor" == "GNU") then
           if ( `$PETOOLS_ROOT/support/version_equal_or_greater.csh $f95_version "10.0.0"` == 1 ) then
	        $sedi "s@-fbacktrace @-fbacktrace -fallow-argument-mismatch @g" ./makefile
           endif
      endif
#
# --- Borrow environment variables for FCOMPL, CCOMPL and CFLAGC
#
      set str_val = `echo $str | awk '{print substr($0,8)}'`
      cat > $temp_make_pgplot522 << EOF_1
#!/bin/csh -f
echo $str_val
EOF_1
      chmod o+x,g+x,u+x $temp_make_pgplot522
      setenv  FCOMPL "`$temp_make_pgplot522 | sed s@-m32@$bits_flag@g`"
      if ( "$bits_flag" == "-m64" ) then
           setenv  FCOMPL "`$temp_make_pgplot522 | sed s@-m32@$bits_flag@g | sed s@-DADR_32BIT@-DADR_64BIT@g`"
	 else 
           setenv  FCOMPL "`$temp_make_pgplot522 | sed s@-m32@$bits_flag@g`"
      endif
      rm -f $temp_make_pgplot522
#
      set str=`grep FFLAGC ../sys_linux/mk5_f95_gfortran_gcc.conf | \
          sed 's@"@ @g' | sed 's@=@ @' | sed s@-m32@$bits_flag@g  | \
          sed s@-DADR_32BIT@-DADR_64BIT@g`
      set str_val = `echo $str | awk '{print substr($0,8)}'`
      setenv  FFLAGC `echo "$str_val"`
#
#      set str=`grep CCOMPL ../sys_linux/mk5_f95_gfortran_gcc.conf | sed 's@"@ @g' | sed 's@=@ @g' | sed s@-m32@$bits_flag@g`
#      set str_val = `echo $str | awk '{print substr($0,8)}'`
#
      setenv CCOMPL "$MK5_CC"
#
      set str=`grep CFLAGC ../sys_linux/mk5_f95_gfortran_gcc.conf | sed 's@"@ @g' | sed 's@=@ @' | sed s@-m32@$bits_flag@g`
      set str_val = `echo $str | awk '{print substr($0,8)}'`
      setenv  CFLAGC `echo "$str_val"`
#
      set str=`grep CFLAGD ../sys_linux/mk5_f95_gfortran_gcc.conf | sed 's@"@ @g' | sed 's@=@ @' | sed s@-m32@$bits_flag@g`
      set str_val = `echo $str | awk '{print substr($0,8)}'`
      setenv  CFLAGD `echo "$str_val"`
#
# --- Remove temporary file
#
      rm -f ${PETOOLS_ROOT}/pgplot/sys_linux/mk5_f95_gfortran_gcc.conf
#
##      cat   ${SUPPORT}/pgplot522_driver.list | awk '{print $LOG}' | grep -s PNDRIV >& /dev/null
#
# --- Check for PNDRIV
#
      cat   ${SUPPORT}/pgplot522_driver.list | grep -s PNDRIV >& /dev/null
      set   pndriv_present = $status
      if ( $pndriv_present == 0 ) then
#
# -------- Copy include files needed for png driver
#
           if ( -f ${PREFIX}/include/png.h ) then
                cp ${PREFIX}/include/png.h ./
	        chmod o+r,g+rw  png.h
              else
                echo "File ${PREFIX}/include/png.h was not found. Please remove "
                echo "PNDRIV driver from pgplot522_drivers.list file "
	        exit 1
           endif
           if ( -f ${PREFIX}/include/pngconf.h ) then
                cp ${PREFIX}/include/pngconf.h ./
	        chmod o+r,g+rw  pngconf.h
              else
                echo "File ${PREFIX}/include/pngconf.h was not found. Please remove "
                echo "PNDRIV driver from pgplot522_drivers.list file "
	        exit 1
           endif
#
           if ( -f ${PREFIX}/include/zlib.h ) then
                cp ${PREFIX}/include/zlib.h ./
	        chmod o+r,g+rw  zlib.h
              else
                echo "File ${PREFIX}/include/zlib.h was not found. Please remove "
                echo "PNDRIV driver from pgplot522_drivers.list file "
	        exit 1
           endif
           if ( -f ${PREFIX}/include/zconf.h ) then
                cp ${PREFIX}/include/zconf.h ./
	        chmod o+r,g+rw  zconf.h
              else
                echo "File ${PREFIX}/include/zconf.h was not found. Please remove "
                echo "PNDRIV driver from pgplot522_drivers.list file "
	        exit 1
           endif
      endif
endif
if ( `uname` == Linux  && "$f95_vendor" == "Intel(R)" ) then
#
# === Linux branch, Intel(R) Fortran compiler ===
#
      echo "Current directory `pwd`" >>& $LOG
#
# --- Copy configuration file
#
      cp ${SUPPORT}/mk5_f95_intel_gcc.conf    ${PETOOLS_ROOT}/pgplot/sys_linux/
      if ( "$bits_flag" == "-m64" ) then
           $sedi  "s@-m32@$bits_flag@g"         ./makefile
           $sedi  "s@-DADR_32BIT@-DADR_64BIT@g" ./makefile
      endif
#
# --- Create system/compiler specific directory if it does not exist
#
      if ( -d ../sys_linux/intel_f95_src ) then
#
# -------- ... otherwise purge it
#
           rm -f ../sys_linux/intel_f95_src/* >&! $null_device
         else
	   if ( -d ../sys_linux ) then
             else
                mkdir ../sys_linux
	   endif
           mkdir ../sys_linux/intel_f95_src
      endif
#
# --- Copy system/compiler specific files there
#
      cp -p ${PETOOLS_ROOT}/pgplot_intel_f95_src/* ../sys_linux/intel_f95_src/
#
      echo "--- beginning of file ${PETOOLS_ROOT}/pgplot/sys_linux/mk5_f95_intel_gcc.conf ---" >>& $LOG
      cat  ${PETOOLS_ROOT}/pgplot/sys_linux/mk5_f95_intel_gcc.conf >>& $LOG
      echo "--- end of file ${PETOOLS_ROOT}/pgplot/sys_linux/mk5_f95_intel_gcc.conf ---" >>& $LOG
      echo "--- beginning of file PETOOLS environment variables list ---" >>& $LOG
      env | grep PETOOLS >>& $LOG
      echo "--- end of file PETOOLS environment variables list ---" >>& $LOG
#
# --- Build the make file
#
      ./makemake ${PETOOLS_ROOT}/pgplot     linux mk5_f95_intel_gcc      >>& $LOG
      if ( "$bits_flag" == "-m64" ) then
           $sedi  "s@-m32@$bits_flag@g" ./makefile
      endif
#
# --- Borrow environment variables for FCOMPL, CCOMPL and CFLAGC
#
      set str=`grep FCOMPL ../sys_linux/mk5_f95_intel_gcc.conf | sed 's@"@ @g' | sed 's@=@ @' `
      set str_val = `echo $str | awk '{print substr($0,8)}'`
      cat > $temp_make_pgplot522 << EOF_2
#!/bin/csh -f
echo $str_val
EOF_2
      chmod o+x,g+x,u+x $temp_make_pgplot522
#
      setenv  FCOMPL "`$temp_make_pgplot522`"
      rm -f $temp_make_pgplot522
#
      set str=`grep FFLAGC ../sys_linux/mk5_f95_intel_gcc.conf | sed 's@"@ @g' | sed 's@=@ @'`
      set str_val = `echo $str | awk '{print substr($0,8)}'`
      setenv  FFLAGC `echo "$str_val"`
#
      set str=`grep CCOMPL ../sys_linux/mk5_f95_intel_gcc.conf | sed 's@"@ @g' | sed 's@=@ @' | sed s@-m32@$bits_flag@g`
      set str_val = `echo $str | awk '{print substr($0,8)}'`
      setenv  CCOMPL `echo "$str_val"`
#
      set str=`grep CFLAGC ../sys_linux/mk5_f95_intel_gcc.conf | sed 's@"@ @g' | sed 's@=@ @' | sed s@-m32@$bits_flag@g`
      set str_val = `echo $str | awk '{print substr($0,8)}'`
      setenv  CFLAGC `echo "$str_val"`
#
      set str=`grep CFLAGD ../sys_linux/mk5_f95_intel_gcc.conf | sed 's@"@ @g' | sed 's@=@ @' | sed s@-m32@$bits_flag@g`
      set str_val = `echo $str | awk '{print substr($0,8)}'`
      setenv  CFLAGD `echo "$str_val"`
#
# --- Remove temporary file
#
      rm -f ${PETOOLS_ROOT}/pgplot/sys_linux/mk5_f95_intel_gcc.conf
      cat   ${SUPPORT}/pgplot522_driver.list | grep -s PNDRIV
      set   pndriv_present = $status
      if ( $pndriv_present == 0 ) then
#
# -------- Copy include files needed for png driver
#
           if ( -f ${PREFIX}/include/png.h ) then
                cp ${PREFIX}/include/png.h ./
	        chmod o+r,g+rw  png.h
              else
                echo "File ${PREFIX}/include/png.h was not found. Please remove "
                echo "PNDRIV driver from pgplot522_drivers.list file "
	        exit 1
           endif
           if ( -f ${PREFIX}/include/pngconf.h ) then
                cp ${PREFIX}/include/pngconf.h ./
	        chmod o+r,g+rw  pngconf.h
              else
                echo "File ${PREFIX}/include/pngconf.h was not found. Please remove "
                echo "PNDRIV driver from pgplot522_drivers.list file "
	        exit 1
           endif
#
           if ( -f ${PREFIX}/include/zlib.h ) then
                cp ${PREFIX}/include/zlib.h ./
	        chmod o+r,g+rw  zlib.h
              else
                echo "File ${PREFIX}/include/zlib.h was not found. Please remove "
                echo "PNDRIV driver from pgplot522_drivers.list file "
	        exit 1
           endif
           if ( -f ${PREFIX}/include/zconf.h ) then
                cp ${PREFIX}/include/zconf.h ./
	        chmod o+r,g+rw  zconf.h
              else
                echo "File ${PREFIX}/include/zconf.h was not found. Please remove "
                echo "PNDRIV driver from pgplot522_drivers.list file "
	        exit 1
           endif
      endif
endif
#
if ( `uname` == "HP-UX" ) then
#
# == HP-UX branch ===
#
     echo "Current directory `pwd`" >>& $LOG
     cp ${SUPPORT}/mk5_f90_c89_hp.conf    ${PETOOLS_ROOT}/pgplot/sys_hp/
#
     echo "--- beginning of file ${PETOOLS_ROOT}/pgplot/sys_hp/mk5_f90_c89_hp.conf     ---" >>& $LOG
     cat  ${PETOOLS_ROOT}/pgplot/sys_hp/mk5_f90_c89_hp.conf   >>& $LOG
     echo "--- end of file ${PETOOLS_ROOT}/pgplot/sys_hp/mk5_f90_c89_hp.conf   ---" >>& $LOG
     echo "--- beginning of file PETOOLS environment variables list ---" >>& $LOG
     env | grep PETOOLS >>& $LOG
     echo "--- end of file PETOOLS environment variables list ---" >>& $LOG
#
     if ( -f ./pgxwin_server ) then
          rm -f ./pgxwin_server       >>& $LOG
     endif
     if ( -d bin/ ) then
          if ( -f ./bin/pgxwin_server ) then
               rm -f ./bin/pgxwin_server       >>& $LOG
          endif
     endif
#
     ./makemake ${PETOOLS_ROOT}/pgplot     hp mk5_f90_c89_hp >>& $LOG
#
# -- Borrow environment variables for FCOMPL, CCOMPL and CFLAGC
#
     set str=`grep FCOMPL ../sys_hp/mk5_f90_c89_hp.conf | sed 's@"@ @g' | sed 's@=@ @' `
     set str_val = `echo $str | awk '{print substr($0,8)}'`
     cat > $temp_make_pgplot522 << EOF_3
#!/bin/csh -f
echo $str_val
EOF_3
     chmod o+x,g+x,u+x $temp_make_pgplot522
     setenv  FCOMPL "`$temp_make_pgplot522`"
     rm -f $temp_make_pgplot522
#
     set str=`grep FFLAGC ../sys_hp/mk5_f90_c89_hp.conf | sed 's@"@ @g' | sed 's@=@ @' `
     if ( $FFLAGS == "" ) then
          set str_val = `echo $str | awk '{print substr($0,8)}'`
          setenv  FFLAGC "$str_val"
       else
          setenv  FFLAGC " "
     endif
#
     set str=`grep CCOMPL ../sys_hp/mk5_f90_c89_hp.conf | sed 's@"@ @g' | sed 's@=@ @' `
     set str_val = `echo $str | awk '{print substr($0,8)}'`
     setenv  CCOMPL "$str_val"
#
     set str=`grep CFLAGC ../sys_hp/mk5_f90_c89_hp.conf | sed 's@"@ @g' | sed 's@=@ @' `
     set str_val = `echo $str | awk '{print substr($0,8)}'`
     setenv  CFLAGC "$str_val"
#
# -- Remove temporary file
#
     rm -f    ${PETOOLS_ROOT}/pgplot/sys_hp/mk5_f90_c89_hp.conf
endif
#
if ( `uname` == "SunOS" ) then
#
# --- NB: SUN OS has expr in the non-standard place!
#
      set exe_expr = /usr/ucb/expr
#
# === SunOS branch ===
#
      echo "Current directory `pwd`" >>& $LOG
#
# --- Create system/compiler specific directory if it does not exist
#
      if ( -d ${PETOOLS_ROOT}/pgplot/sys_sunos ) then
         else
	   mkdir ${PETOOLS_ROOT}/pgplot/sys_sunos
      endif
#
      if ( -d ${PETOOLS_ROOT}/pgplot/sys_sunos/f95_cc ) then
#
# -------- ... otherwise purge it
#
           rm -f ${PETOOLS_ROOT}/pgplot/sys_sunos/f95_cc >&! $null_device
         else
           if ( -d ${PETOOLS_ROOT}/pgplot/sys_sunos ) then
	     else
                mkdir ${PETOOLS_ROOT}/pgplot/sys_sunos
           endif
           mkdir ${PETOOLS_ROOT}/pgplot/sys_sunos/f95_cc
      endif
#
# --- Copy configuration file
#
      cp ${SUPPORT}/mk5_f95_cc_sunos.conf    ${PETOOLS_ROOT}/pgplot/sys_sunos/
#
# --- Copy system/compiler specific files there
#
      cp -p ${PETOOLS_ROOT}/pgplot_sunos_f95_src/* ${PETOOLS_ROOT}/pgplot/sys_sunos/f95_cc
#
      echo "--- beginning of file ${PETOOLS_ROOT}/pgplot/sys_sunos/mk5_f95_cc_sunos.conf ---" >>& $LOG
      cat  ${PETOOLS_ROOT}/pgplot/sys_sunos/mk5_f95_cc_sunos.conf  >>& $LOG
      echo "--- end of file ${PETOOLS_ROOT}/pgplot/sys_sunos/mk5_f95_cc_sunos.conf   ---" >>& $LOG
      echo "--- beginning of file PETOOLS environment variables list ---" >>& $LOG
      env | grep PETOOLS >>& $LOG
      echo "--- end of file PETOOLS environment variables list ---" >>& $LOG
#
# --- Build the make file
#
      ./makemake ${PETOOLS_ROOT}/pgplot     sunos mk5_f95_cc_sunos >>& $LOG
#
# --- Borrow environment variables for FCOMPL, CCOMPL and CFLAGC
#
      set str=`grep FCOMPL ${PETOOLS_ROOT}/pgplot/sys_sunos/mk5_f95_cc_sunos.conf | sed 's@="@ @g' | sed 's@"@ @g' `
      set str_val = `echo $str | awk '{print substr($0,8)}'`
      cat > $temp_make_pgplot522 << EOF_4
#!/bin/csh -f
echo $str_val
EOF_4
      chmod o+x,g+x,u+x $temp_make_pgplot522
      setenv  FCOMPL "`$temp_make_pgplot522`"
      rm -f $temp_make_pgplot522
#
      set str=`grep FFLAGC ${PETOOLS_ROOT}/pgplot/sys_sunos/mk5_f95_cc_sunos.conf | sed 's@="@ @g' | sed 's@"@ @g' `
      set str_val = `echo $str | awk '{print substr($0,8)}'`
      setenv  FFLAGC "$str_val"
#
      set str=`grep CCOMPL ${PETOOLS_ROOT}/pgplot/sys_sunos/mk5_f95_cc_sunos.conf | sed 's@="@ @g' | sed 's@"@ @g' `
      set str_val = `echo $str | awk '{print substr($0,8)}'`
      setenv  CCOMPL "$str_val"
#
      set str=`grep CFLAGC ${PETOOLS_ROOT}/pgplot/sys_sunos/mk5_f95_cc_sunos.conf | sed 's@="@ @g' | sed 's@"@ @g' `
      set str_val = `echo $str | awk '{print substr($0,8)}'`
      setenv  CFLAGC "$str_val"
#
# --- Remove temporary file
#
      rm -f    ${PETOOLS_ROOT}/pgplot/sys_sunos/mk5_f95_cc_sunos.conf
#
# --- Replace BYTE with INTEGER*1 in one of the drivers
#
      cp -p ${PETOOLS_ROOT}/pgplot/drivers/gidriv.f ${PETOOLS_ROOT}/pgplot/drivers/gidriv.f_orig
      sed "s/BYTE/INTEGER*1/" ${PETOOLS_ROOT}/pgplot/drivers/gidriv.f_orig > \
                              ${PETOOLS_ROOT}/pgplot/drivers/gidriv.f
endif
#
# ==== End of OS specific branches ====
#
echo "make_pgplot522: FCOMPL = $FCOMPL " >>&  $LOG
echo "make_pgplot522: FFLAGC = $FFLAGC " >>&  $LOG
echo "make_pgplot522: CCOMPL = $CCOMPL " >>&  $LOG
echo "make_pgplot522: CFLAGC = $CFLAGC " >>&  $LOG
echo "make_pgplot522: MK5_X11_INCLUDE  $MK5_X11_INCLUDE" >>&  $LOG
echo "make_pgplot522: MK5_X11_LIB      $MK5_X11_LIB"     >>&  $LOG
#
$sedi  's@FCOMPL=$FCOMPL@'"FCOMPL=$FCOMPL@g" makemake
$sedi  's@FFLAGC=$FFLAGC@'"FFLAGC=$FFLAGC@g" makemake
$sedi  's@CCOMPL=$CCOMPL@'"CCOMPL=$CCOMPL@g" makemake
$sedi  's@CFLAGC=$CFLAGC@'"CFLAGC=$CFLAGC@g" makemake
$sedi  's@CFLAGD=$CFLAGD@'"CFLAGD=$CFLAGD@g" makemake
#
# --- Copy routines from plgplot_plus directory to a compiler specific directory
#
cp ${PETOOLS_ROOT}/pgplot_plus/* ./
#
# --- Create binary directory, or purge it if already exists
#
if ( -d ../bin/ ) then
      rm -f ../bin/* >&! $null_device
   else
      mkdir ../bin
endif
#
# --- Create include directory, or purge it if already exists
#
if ( -d ../include/ ) then
      rm -f ../include/* >&! $null_device
   else
      mkdir ../include
endif
#
if ( `uname` == "Linux" || `uname` == "Darwin" || `uname` == "HP-UX" ) then
#
# ------ Special trick: we do not complile cpgdemo, since make file
# ------ try to link through Fortran compiler which requires a special
# ------ option -nofor_Main when the main program is non-Fortran
#
         set temp_make = /tmp/make__pgplot522__$$
         sed "s/cpgplot.h cpgdemo/cpgplot.h/g" makefile > $temp_make
         mv $temp_make makefile
endif
#
# --- Compile and link pgplot_plus
#
$PETOOLS_MAKE  clean                     >>&  $null_device
$PETOOLS_MAKE  -f pgplot_plus.mak clean  >>&  $null_device
$PETOOLS_MAKE                            >>&  $LOG
if ( $status != 0 ) then
     echo "Error in building pgplot" | tee -a $LOG
     exit  1
endif
$PETOOLS_MAKE  -f pgplot_plus.mak        >>&  $LOG
if ( `uname` == "Linux" || `uname` == "Darwin" || `uname` == "HP-UX" ) then
      $PETOOLS_MAKE  cpg                  >>&  $LOG
endif
#
   cd ../include/
#
# --- Copy include files in the include directories
#
      mv        ../build/pgplot.inc   ./
      mv        ../build/grpckg1.inc  ./
      if ( `uname` == "Linux" || `uname` == "Darwin" || `uname` == "HP-UX" ) then
            mv  ../build/cpgplot.h    ./
      endif
   cd ${PETOOLS_ROOT}/bin/
#
# --- Copy binary files to the binary directory
#
      mv $PETOOLS_ROOT/pgplot/build/libpgplot.a   ./
      if ( `uname` == "HP-UX" ) then
           if ( -d $PETOOLS_ROOT/pgplot/shared_build ) then
                rm -fR $PETOOLS_ROOT/pgplot/shared_build/ >!& /dev/null
           endif
#
# -------- Create shared library libpgplot.sl
#
           $PETOOLS_ROOT/support/make_hp_shared.csh `pwd`/libpgplot.a  \
                                                `pwd`/libpgplot.sl \
                              ${PETOOLS_ROOT}/pgplot/shared_build  >>&  $LOG
#
           mv $PETOOLS_ROOT/pgplot/build/libcpgplot.a   ./
           mv $PETOOLS_ROOT/pgplot/build/cpgdemo        ./
      endif
      if ( `uname` == "Linux" ) then
           # if ( "$bits_flag" == "-m64" ) then
           #      ld -shared --whole-archive -o libpgplot.sl  libpgplot.a
           #      mv $PETOOLS_ROOT/pgplot/build/libcpgplot.a   ./
           #      ld -shared --whole-archive -o libcpgplot.sl libcpgplot.a
	   #    else
           #      ld -shared --whole-archive -m elf_i386 -o libpgplot.sl  libpgplot.a
           #      mv $PETOOLS_ROOT/pgplot/build/libcpgplot.a   ./
           #      ld -shared --whole-archive -m elf_i386 -o libcpgplot.sl libcpgplot.a
           # endif
           $CCOMPL -shared -Wl,-soname,libpgplot.so.5 -o $PETOOLS_ROOT/lib/libpgplot.so.5.2 \
                    $PETOOLS_ROOT/pgplot/build/*.o
	else if ( `uname` == "Darwin" ) then
           $CCOMPL -dynamiclib -install_name $PETOOLS_ROOT/lib/libpgplot.1.dylib \
                   -o  $PETOOLS_ROOT/lib/libpgplot.1.dylib $PETOOLS_ROOT/pgplot/build/*.o  \
                   -lgfortran -lc -lpthread -lm -lgcc -lgomp $SOLVE_LIB_X11 $SOLVE_EXTRA_LIB
      endif
      if ( -d $PETOOLS_PREFIX/share == 0 ) mkdir $PETOOLS_PREFIX/share/
      endif
#
# --- Copy rgb.txt file since some Linux distributions may drop it
#
      cp $PETOOLS_ROOT/pgplot/rgb.txt  $PETOOLS_PREFIX/share/
#
# --- Clean building directory
#
      $PETOOLS_MAKE  clean                     >>&  $null_device
      $PETOOLS_MAKE  -f pgplot_plus.mak clean  >>&  $null_device
#
      mv $PETOOLS_ROOT/pgplot/build/pgxwin_server ./
      mv $PETOOLS_ROOT/pgplot/build/pgdemo?       ./
      mv $PETOOLS_ROOT/pgplot/build/pgdemo1?      ./
      mv $PETOOLS_ROOT/pgplot/build/grfont.dat    ./
      mv $PETOOLS_ROOT/pgplot/build/rgb.txt       ./
      cp ${PETOOLS_ROOT}/bin/libpgplot.a  ${PETOOLS_ROOT}/lib/libpgplot.a 
      if ( -f ${PETOOLS_ROOT}/bin/libpgplot.sl ) cp ${PETOOLS_ROOT}/bin/libpgplot.sl ${PETOOLS_ROOT}/lib/libpgplot.sl
#
   cd ${PETOOLS_ROOT}/doc
#
# --- Copy documentation in the doc directory
#
      mv $PETOOLS_ROOT/pgplot/build/pgplot.doc    ./

   cd ${PETOOLS_ROOT}/pgplot
#
# --- Remove temporary files from build directory
#
rm -fR build/ >&! $null_device
#
# --- Remove system/compiler specific directory in order to make
# --- distribution pristine clean
#
if ( `uname` == "Linux" || `uname` == "Darwin" ) then
      rm -fR sys_linux/intel_f95_src/ >&! $null_device
endif
if ( `uname` == "SunOS" ) then
      mv drivers/gidriv.f_orig drivers/gidriv.f
      rm -fR sys_sunos/f95_cc >&! $null_device
endif
cd $PWD_SAVE
