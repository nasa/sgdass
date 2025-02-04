#!/bin/csh
setenv fcon_exe /home/lpetrov/exec/fcon.e
setenv fcon_cnf /home/lpetrov/fcon/fcon.cnf
setenv inp_dir  /tmp/diagi_exe
#
$fcon_exe $fcon_cnf $inp_dir/diagi_rst.f    " "  diagi_rst.f
$fcon_exe $fcon_cnf $inp_dir/diagi_demo.f   " "  diagi_demo.f
$fcon_exe $fcon_cnf $inp_dir/diagi_dec.f    " "  diagi_dec.f
$fcon_exe $fcon_cnf $inp_dir/diagi_user.f   " "  diagi_user.f
$fcon_exe $fcon_cnf $inp_dir/diagi_batch.f  " "  diagi_batch.f
$fcon_exe $fcon_cnf $inp_dir/md_demo.f      " "  md_demo.f
