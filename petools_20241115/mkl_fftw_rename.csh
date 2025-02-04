#!/bin/csh -f
set mkl_dir = /opt/intel/mkl
set out_dir = /tmp/
#
set temp_file = /tmp/mkl__$$.sym
#
nm $mkl_dir/lib/intel64/libmkl_gf_lp64.a    | grep fftw | grep ' T ' | awk '{printf "%s intel_%s\n", $3,$3}' > $temp_file
objcopy --redefine-syms=$temp_file $mkl_dir/lib/intel64/libmkl_gf_lp64.a   ${out_dir}/libmkl_gf_lp64_nofftw.a
objcopy --redefine-syms=$temp_file $mkl_dir/lib/intel64/libmkl_gf_lp64.so  ${out_dir}/libmkl_gf_lp64_nofftw.so
#
nm $mkl_dir/lib/intel64/libmkl_intel_lp64.a | grep fftw | grep ' T ' | awk '{printf "%s intel_%s\n", $3,$3}' > $temp_file
objcopy --redefine-syms=$temp_file $mkl_dir/lib/intel64/libmkl_intel_lp64.a   ${out_dir}/libmkl_intel_lp64_nofftw.a
objcopy --redefine-syms=$temp_file $mkl_dir/lib/intel64/libmkl_intel_lp64.so  ${out_dir}/libmkl_intel_lp64_nofftw.so
rm $temp_file
