#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   C-shell program searches for X11 headers and libraries.            *
# *                                                                      *
# * ### 01-JUN-2004 search_X11.csh v3.1 (c)  L. Petrov  18-APR-2022 ###  *
# *                                                                      *
# ************************************************************************
setenv LANG   C
setenv LC_ALL C
set include_x11_array = ' '
set include_x11_array = "$include_x11_array /opt/X11/include/X11"
set include_x11_array = "$include_x11_array /usr/X11/include"
set include_x11_array = "$include_x11_array /usr/X11/include/X11"
set include_x11_array = "$include_x11_array /usr/X11R6/include"
set include_x11_array = "$include_x11_array /usr/X11R5/include"
set include_x11_array = "$include_x11_array /usr/X11R4/include"
set include_x11_array = "$include_x11_array /usr/include/X11"
set include_x11_array = "$include_x11_array /usr/include/X11R6"
set include_x11_array = "$include_x11_array /usr/include/X11R5"
set include_x11_array = "$include_x11_array /usr/include/X11R4"
set include_x11_array = "$include_x11_array /usr/local/X11/include"
set include_x11_array = "$include_x11_array /usr/local/X11R6/include"
set include_x11_array = "$include_x11_array /usr/local/X11R5/include"
set include_x11_array = "$include_x11_array /usr/local/X11R4/include"
set include_x11_array = "$include_x11_array /usr/local/include/X11"
set include_x11_array = "$include_x11_array /usr/local/include/X11R6"
set include_x11_array = "$include_x11_array /usr/local/include/X11R5"
set include_x11_array = "$include_x11_array /usr/local/include/X11R4"
set include_x11_array = "$include_x11_array /usr/X386/include"
set include_x11_array = "$include_x11_array /usr/x386/include"
set include_x11_array = "$include_x11_array /usr/XFree86/include/X11"
set include_x11_array = "$include_x11_array /usr/include"
set include_x11_array = "$include_x11_array /usr/local/include"
set include_x11_array = "$include_x11_array /usr/unsupported/include"
set include_x11_array = "$include_x11_array /usr/athena/include"
set include_x11_array = "$include_x11_array /usr/local/x11r5/include"
set include_x11_array = "$include_x11_array /usr/lpp/Xamples/include"
set include_x11_array = "$include_x11_array /usr/openwin/include"
set include_x11_array = "$include_x11_array /usr/openwin/share/include"
set include_x11_array = "$include_x11_array /opt/X11/include"
#
set lib_x11_array = ' '
set lib_x11_array = "$lib_x11_array /opt/X11/lib"
set lib_x11_array = "$lib_x11_array /opt/X11/lib/X11"
set lib_x11_array = "$lib_x11_array /usr/X11/lib"
set lib_x11_array = "$lib_x11_array /usr/X11R6/lib"
set lib_x11_array = "$lib_x11_array /usr/X11R5/lib"
set lib_x11_array = "$lib_x11_array /usr/X11R4/lib"
set lib_x11_array = "$lib_x11_array /usr/X11R6/lib64"
set lib_x11_array = "$lib_x11_array /usr/X11R5/lib64"
set lib_x11_array = "$lib_x11_array /usr/X11R4/lib64"
set lib_x11_array = "$lib_x11_array /usr/lib/X11"
set lib_x11_array = "$lib_x11_array /usr/lib/X11R6"
set lib_x11_array = "$lib_x11_array /usr/lib/X11R5"
set lib_x11_array = "$lib_x11_array /usr/lib/X11R4"
set lib_x11_array = "$lib_x11_array /usr/local/X11/lib"
set lib_x11_array = "$lib_x11_array /usr/local/X11R6/lib"
set lib_x11_array = "$lib_x11_array /usr/local/X11R5/lib"
set lib_x11_array = "$lib_x11_array /usr/local/X11R4/lib"
set lib_x11_array = "$lib_x11_array /usr/local/X11R6/lib64"
set lib_x11_array = "$lib_x11_array /usr/local/X11R5/lib64"
set lib_x11_array = "$lib_x11_array /usr/local/X11R4/lib64"
set lib_x11_array = "$lib_x11_array /usr/local/lib/X11"
set lib_x11_array = "$lib_x11_array /usr/local/lib/X11R6"
set lib_x11_array = "$lib_x11_array /usr/local/lib/X11R5"
set lib_x11_array = "$lib_x11_array /usr/local/lib/X11R4"
set lib_x11_array = "$lib_x11_array /usr/X386/lib"
set lib_x11_array = "$lib_x11_array /usr/x386/lib"
set lib_x11_array = "$lib_x11_array /usr/XFree86/lib/X11"
set lib_x11_array = "$lib_x11_array /usr/lib"
set lib_x11_array = "$lib_x11_array /usr/lib32"
set lib_x11_array = "$lib_x11_array /usr/lib64"
set lib_x11_array = "$lib_x11_array /usr/local/lib"
set lib_x11_array = "$lib_x11_array /usr/unsupported/lib"
set lib_x11_array = "$lib_x11_array /usr/athena/lib"
set lib_x11_array = "$lib_x11_array /usr/local/x11r5/lib"
set lib_x11_array = "$lib_x11_array /usr/lpp/Xamples/lib"
set lib_x11_array = "$lib_x11_array /usr/openwin/lib"
set lib_x11_array = "$lib_x11_array /usr/openwin/share/lib"
set lib_x11_array = "$lib_x11_array /usr/lib/x86_64-linux-gnu"
#
#
set bin_x11_array = ' '
set bin_x11_array = "$bin_x11_array /opt/X11/bin"
set bin_x11_array = "$bin_x11_array /usr/X11R6/bin"
set bin_x11_array = "$bin_x11_array /usr/X11R5/bin"
set bin_x11_array = "$bin_x11_array /usr/X11R4/bin"
set bin_x11_array = "$bin_x11_array /usr/local/X11/bin"
set bin_x11_array = "$bin_x11_array /usr/local/X11R6/bin"
set bin_x11_array = "$bin_x11_array /usr/local/X11R5/bin"
set bin_x11_array = "$bin_x11_array /usr/local/X11R4/bin"
set bin_x11_array = "$bin_x11_array /usr/X386/bin"
set bin_x11_array = "$bin_x11_array /usr/x386/bin"
set bin_x11_array = "$bin_x11_array /usr/XFree86/bin"
set bin_x11_array = "$bin_x11_array /usr/bin"
set bin_x11_array = "$bin_x11_array /usr/local/bin"
set bin_x11_array = "$bin_x11_array /usr/unsupported/bin"
set bin_x11_array = "$bin_x11_array /usr/athena/bin"
set bin_x11_array = "$bin_x11_array /usr/local/x11r5/bin"
set bin_x11_array = "$bin_x11_array /usr/openwin/bin"
#
if ( $?MK5_X11_INCLUDE == 1 ) then
     setenv MK5_X11_INCLUDE  "" 
endif
if ( "$MK5_X11_INCLUDE" == "" ) then
#
# -- Directory with X11 header files was not specified by configuration file.
# -- Try to find it.
#
     setenv MK5_X11_INCLUDE ""
     foreach dir ($include_x11_array)
        if ( -f $dir/X11/Intrinsic.h && -f $dir/X11/Xlib.h ) then
             setenv MK5_X11_INCLUDE $dir
     	     break
        endif
     end
     if ( $MK5_X11_INCLUDE == "" ) then
          echo "Cannot find X11 headers files"
          echo "In paritular, could not find X11/Intrinsic.h asnd X11/Xlib.h in $include_x11_array"
          exit 1
     endif
  else
     if ( -d $MK5_X11_INCLUDE == 0 ) then
          echo "Cannot find directory with X11 headers specified in configure"
          exit 1
     endif
endif
#
if ( $?PETOOLS_BITS == 0 ) then
     if ( "`uname -a | grep x86_64`" != "" ) then
           set bits_flag = "-m64"
       else
           set bits_flag = "-m32"
     endif
  else
     if ( $PETOOLS_BITS == 64 ) then
          set bits_flag = "-m64"
        else
          set bits_flag = "-m32"
     endif
endif
#
if ( $?MK5_X11_LIB == 0 ) then
     setenv MK5_X11_LIB "" 
endif
if ( "$MK5_X11_LIB" == "" ) then
#
# -- Directory with X11 library files was not specified by configuration file.
# -- Try to find it.
#
     setenv MK5_X11_LIB ""
     foreach dir ($lib_x11_array)
         if ( -f $dir/libX11.so ) then
     	       if ( "`uname`" == "Linux" ) then
   	            if ( "$bits_flag" == "-m32" ) then
                         if ( "`file -L $dir/libX11.so | grep 32-bit`" != "" ) then
                               setenv MK5_X11_LIB $dir
     	                       break
                         endif
                         if ( "`file -L $dir/libX11.dylib | grep 32-bit`" != "" ) then
                               setenv MK5_X11_LIB $dir
     	                       break
                         endif
                    endif
	            if ( "$bits_flag" == "-m64" ) then
                         if ( "`file -L $dir/libX11.so | grep 64-bit`" != "" ) then
                              setenv MK5_X11_LIB $dir
      	                      break
                         endif
                         if ( "`file -L $dir/libX11.dylub | grep 64-bit`" != "" ) then
                              setenv MK5_X11_LIB $dir
      	                      break
                         endif
                    endif
                  else
                    setenv MK5_X11_LIB $dir
     	            break
               endif
           else if ( -f $dir/libX11.dylib ) then
               setenv MK5_X11_LIB $dir
     	       break
         endif
     end
     if ( $MK5_X11_LIB == "" ) then
          echo "Cannot find X11 libraries"
          exit 1
     endif
  else
     if ( -d $MK5_X11_LIB == 0 ) then
          echo "Cannot find directory with X11 libraries specified in configure"
          exit 1
     endif
     if ( -f $MK5_X11_LIB/libX11.so ) then
#
# ------- Found shared library.
#
     	  if ( "`uname`" == "Linux" ) then
#
# ------------ Check, whether it has the same architecture
#
	       if ( "$bits_flag" == "-m32" ) then
                    if ( "`file -L $MK5_X11_LIB/libX11.so | grep 32-bit`" == "" ) then
                         echo "The directory with X11 libraries specified in configure contains 64-bit shared libraries instead of 32-bit libraries"
                         exit 1
                    endif
               endif
	       if ( "$bits_flag" == "-m64" ) then
                    if ( "`file -L $MK5_X11_LIB/libX11.so | grep 64-bit`" == "" ) then
                         echo "The directory with X11 libraries specified in configure contains 64-bit shared libraries instead of 32-bit libraries"
                         exit 1
                    endif
               endif
          endif
          setenv SOLVE_LIB_X11  $MK5_X11_LIB/libX11.so
        else if ( -f $MK5_X11_LIB/libX11.dylib ) then
          setenv SOLVE_LIB_X11  $MK5_X11_LIB/libX11.dylib 
          set exit = 0
        else
          echo "Cannot find libX11.so in the X11 libraries specified in configure"
          exit 1
     endif
endif
#
if ( "$MK5_X11_BIN" == "" ) then
     foreach dir ($bin_x11_array)
         if ( -e $dir/xrdb ) then
              setenv MK5_X11_BIN $dir
              break
         endif
     end
endif
if ( "$MK5_X11_BIN" == "" ) then
     echo "Cannot find xrdb in X11 binaries libraries specified in configure"
     exit 1
endif
if ( -e $MK5_X11_BIN/xrdb == "" ) then
     echo "Cannot find xrdb in $MK5_X11_BIN "
     exit 1
endif
#
if ( $1 == "show" ) then
     echo " MK5_X11_INCLUDE  $MK5_X11_INCLUDE"
     echo " MK5_X11_LIB      $MK5_X11_LIB"
endif
#
setenv SOLVE_PGPLOT_X_INC "$MK5_X11_INCLUDE"
## setenv SOLVE_LIB_X11      $MK5_X11_LIB
