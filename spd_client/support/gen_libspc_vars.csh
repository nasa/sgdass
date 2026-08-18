#!/bin/csh -f
if ( `uname` == "Linux" ) then
      set get_lib = ldd
  else if ( `uname` == "Darwin" ) then
      set get_lib = "otool -L"
endif
#
set libspc      = $2
set libpetool   = `$get_lib $1 | grep libpetool   | awk '{print $3}'`
set liblapack   = `$get_lib $1 | grep liblapack   | awk '{print $3}'`
set libgfortran = `$get_lib $1 | grep libgfortran | awk '{print $3}'`
#
set ldi = \$\{LIBSPC\}
echo "setenv LIBSPC $libspc"             > $SPC_ROOT/lib/libspc_vars.csh
echo "setenv LIBGFORTRAN $libgfortran"  >> $SPC_ROOT/lib/libspc_vars.csh
echo "export LIBSPC=$libspc"             > $SPC_ROOT/lib/libspc_vars.sh
echo "export LIBGFORTRAN=$libgfortran"  >> $SPC_ROOT/lib/libspc_vars.sh
if ( "${libpetool}x" != "x" ) then
     echo "setenv LIBPETOOL $libpetool" >> $SPC_ROOT/lib/libspc_vars.csh
     echo "export LIBPETOOL=$libpetool" >> $SPC_ROOT/lib/libspc_vars.sh
     set ldi = "${ldi}":\$\{LIBPETOOL\}
endif
if ( "${liblapack}x" != "x"  ) then
     echo "setenv LIBLAPACK $liblapack" >> $SPC_ROOT/lib/libspc_vars.csh
     echo "export LIBLAPACK=$liblapack" >> $SPC_ROOT/lib/libspc_vars.sh
     set ldi = "${ldi}":\$\{LIBLAPACK\}
endif
set ldi = "${ldi}":\$\{LIBGFORTRAN\}
if ( `uname` == "Linux" ) then
      echo "setenv LD_PRELOAD $ldi" >> $SPC_ROOT/lib/libspc_vars.csh
      echo "export LD_PRELOAD=$ldi" >> $SPC_ROOT/lib/libspc_vars.sh
  else
      echo "setenv DYLD_INSERT_LIBRARY $ldi" >> $SPC_ROOT/lib/libspc_vars.csh
      echo "export DYLD_INSERT_LIBRARY=$ldi" >> $SPC_ROOT/lib/libspc_vars.sh
endif
