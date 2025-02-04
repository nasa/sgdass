#!/bin/bash
export PETOOLS_ROOT=/opt64 
export PETOOLS_PREFIX=/opt64 
export SPD_CLIENT_PREFIX=/opt64 
export SPD_CLIENT_LIB=/opt64/lib 
export NERS_LIB=/opt64/lib 
export NERS_INC=/opt64/include 
export VTD_LIB=/opt64/lib 
export VTD_INC=/opt64/include 
export GVH_LIB=/opt64/lib 
export GVH_INC=/opt64/include 
export VEX_LIB=/opt64/lib 
export VEX_INC=/opt64/include 
export SOLVE_LIB_GVH="-L/opt64/lib -lgvh -lvcat" 
export SOLVE_LIB_VTD="-L/opt64/lib -lvtd -L/opt64/lib -lspc -L/opt64/lib -lners" 
export SOLVE_LIB_PETOOLS="-L/opt64/lib -lpetools -L /opt64/lib -lreadline -L /opt64/lib64 -lncurses" 
export SOLVE_LIB_PGPLOT="-L/opt64/lib -lpgplot" 
export SOLVE_LIB_X11="/usr/lib64/libX11.so" 
export SOLVE_LIB_XT=" " 
export SOLVE_LIB_X11="/usr/lib64/libX11.so" 
export SOLVE_LIB_XHP11=" " 
export SOLVE_EXTRA_LIB="-lpthread -lrt -L /opt64/lib -lz -L /opt64/lib -lpng" 
export SOLVE_LIB_VEC=" " 
export SOLVE_LIB_BLAS="-L/opt64/lib -lopenblas" 
export MK5_C="/usr/bin/gcc -m64 -mtune=native -fopenmp -fpermissive -fPIC -D LINUX -D LITTLE_ENDIAN -D FORTRAN_TRUE=1 -D FORTRAN_FALSE=0 -D _FILE_OFFSET_BITS=64 -D _LARGEFILE_SOURCE -I ./ -I ../include -I /usr/include -I /usr/local/include -fno-diagnostics-color -I /opt64/include" 
export MK5_X11_INCLUDE="/usr/include" 
export MK5_F95="/usr/bin/gfortran -m64 -ffree-form -ffree-line-length-none -fmax-errors=16 -fno-underscoring -fdollar-ok -fopenmp -x f95-cpp-input -finit-integer=-2147483647 -finit-logical=false -finit-real=nan -finit-character=0 -fbacktrace -ftrapv -fPIC -ffpe-trap=overflow,invalid,zero -fexceptions -fPIC -D LINUX -D GNU -D LITTLE_ENDIAN -D ADDRESS__TYPE=INTEGER\(8\) -D ADR_64BIT -D BLAS=openblas -fno-diagnostics-color -fallow-argument-mismatch -Wno-align-commons -J /opt64/module -I ./ -I /f1/progs/psolve_20241125/include -I /opt64/include -fcheck=all -O2 -mavx2" 
export MK5_F95_OPT="/usr/bin/gfortran -m64 -ffree-form -ffree-line-length-none -fmax-errors=16 -fno-underscoring -fdollar-ok -fopenmp -x f95-cpp-input -finit-integer=-2147483647 -finit-logical=false -finit-real=nan -finit-character=0 -fbacktrace -fPIC -mtune=native -D LINUX -D GNU -D LITTLE_ENDIAN -D ADDRESS__TYPE=INTEGER\(8\) -D ADR_64BIT -D BLAS=openblas -fno-diagnostics-color -fallow-argument-mismatch -Wno-align-commons -mavx2 -J /opt64/module -I ./ -I /f1/progs/psolve_20241125/include -I /opt64/include -mavx -O3" 
export MK5_F95_OPTEST="/usr/bin/gfortran -m64 -ffree-form -ffree-line-length-none -fmax-errors=16 -fno-underscoring -fdollar-ok -fopenmp -x f95-cpp-input -finit-integer=-2147483647 -finit-logical=false -finit-real=nan -finit-character=0 -fbacktrace -fPIC -mtune=native -D LINUX -D GNU -D LITTLE_ENDIAN -D ADDRESS__TYPE=INTEGER\(8\) -D ADR_64BIT -D BLAS=openblas -fno-diagnostics-color -fallow-argument-mismatch -Wno-align-commons -mavx2 -J /opt64/module -I ./ -I /f1/progs/psolve_20241125/include -I /opt64/include -mavx -Ofast -ftree-vectorize" 
export MK5_F95_NOOPT="/usr/bin/gfortran -m64 -ffree-form -ffree-line-length-none -fmax-errors=16 -fno-underscoring -fdollar-ok -fopenmp -x f95-cpp-input -finit-integer=-2147483647 -finit-logical=false -finit-real=nan -finit-character=0 -fbacktrace -ftrapv -fPIC -ffpe-trap=overflow,invalid,zero -fexceptions -fPIC -D LINUX -D GNU -D LITTLE_ENDIAN -D ADDRESS__TYPE=INTEGER\(8\) -D ADR_64BIT -D BLAS=openblas -fno-diagnostics-color -fallow-argument-mismatch -Wno-align-commons -J /opt64/module -I ./ -I /f1/progs/psolve_20241125/include -I /opt64/include -fcheck=all -O0 -g" 
export MK5_LINK="/usr/bin/gfortran -m64 -fopenmp -Wl,--gc-sections,--no-warn-search-mismatch" 
export MK5_C_LINK="/usr/bin/gfortran -m64 -fopenmp -Wl,--gc-sections,--no-warn-search-mismatch" 
export SOLVE_ROOT=/f1/progs/psolve_20241125 
export SOLVE_PREFIX=/opt64 
export SOLVE_BIN=/opt64/bin 
export SOLVE_INC=/opt64/include 
export SOLVE_LIB=/opt64/lib 
export SOLVE_SCRATCH_DATA=/scr/psolve 
export SOLVE_SAVE_DATA=/opt64/share/psolve 
export SOLVE_GVF_DATA=/l2/gvf 
export SOLVE_SCRATCH_PIMA=undefined 
export SOLVE_CENTER_ABR=NAS 
export SOLVE_CENTER_NAM="NASA GSFC" 
export SOLVE_PS_VIEWER=/usr/bin/display 
export SOLVE_GIF_VIEWER=/usr/bin/display 
export SOLVE_CFITSIO_LIB="-L /opt64/lib -lcfitsio" 
export SOLVE_FITSLIB_LIB="-L /opt64/lib -lfitslib" 
export SOLVE_OPT_NOSTRUC="" 
export SOLVE_OS=Linux 
export SOLVE_VERSION=20241125 
export 0022=
