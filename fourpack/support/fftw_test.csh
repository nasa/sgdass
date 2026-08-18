#!/bin/csh -f
if ( -f $FOURPACK_ROOT/support/fftw_test.o ) rm $FOURPACK_ROOT/support/fftw_test.o 
if ( -f $FOURPACK_ROOT/support/fftw_test.e ) rm $FOURPACK_ROOT/support/fftw_test.e
if ( "$2" == "non_openmp" ) then
     $MK5_F95 -I $1/include -c -o $FOURPACK_ROOT/support/fftw_test.o $FOURPACK_ROOT/support/fftw_test.f
     set test_status = $status
     if ( $test_status == 0 ) then
          $MK5_LINK -m64 -fopenmp -o $FOURPACK_ROOT/support/fftw_test.e $FOURPACK_ROOT/support/fftw_test.o \
          -L $1/lib -lfftw3 -lfftw3f
          set test_status = $status
       else 
          if ( -f $FOURPACK_ROOT/support/fftw_test.o ) rm $FOURPACK_ROOT/support/fftw_test.o 
          exit 1
     endif
     if ( -f $FOURPACK_ROOT/support/fftw_test.o ) rm $FOURPACK_ROOT/support/fftw_test.o
     if ( -f $FOURPACK_ROOT/support/fftw_test.e ) rm $FOURPACK_ROOT/support/fftw_test.e
     exit $test_status 
  else if ( $2 == "openmp" ) then
     $MK5_F95 -I $1/include -D OPENMP_TEST -c -o $FOURPACK_ROOT/support/fftw_test.o $FOURPACK_ROOT/support/fftw_test.f
     set test_status = $status
     if ( $test_status == 0 ) then
          $MK5_LINK  -m64 -fopenmp -o $FOURPACK_ROOT/support/fftw_test.e $FOURPACK_ROOT/support/fftw_test.o \
          -L $1/lib -lfftw3 -lfftw3_omp -lfftw3f -lfftw3f_omp
          set test_status = $status
       else 
          if ( -f $FOURPACK_ROOT/support/fftw_test.o ) rm $FOURPACK_ROOT/support/fftw_test.o 
          exit 1
     endif
     if ( -f $FOURPACK_ROOT/support/fftw_test.o ) rm $FOURPACK_ROOT/support/fftw_test.o
     if ( -f $FOURPACK_ROOT/support/fftw_test.e ) rm $FOURPACK_ROOT/support/fftw_test.e
     exit $test_status 
endif
