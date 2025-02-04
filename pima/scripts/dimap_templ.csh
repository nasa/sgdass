#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Command file for running difmap in an interactive mode.            *
# *                                                                      *
# *  ### 08-SEP-2013     dimap     v2.0 (c)  L. Petrov  25-FEB-2020 ###  *
# *                                                                      *
# ************************************************************************
if ( $1 == "" ) then
     echo "Uesage: dimap uva-file"
     exit 1
endif
set dimap_cnf = %%pima_path%%/share/pima/pima_manual_difmap.cnf
#
set uva = $1
set len = `expr length $uva:r - 4`
set prefix = `expr substr $uva:r 1 $len`
onintr cleanup
set difmap_temp1 = /tmp/difmap__$$.1
set difmap_temp2 = /tmp/difmap__$$.2
echo '#+SV save '"$prefix; " > $difmap_temp2 
echo "@$dimap_cnf; @$difmap_temp2; obs $uva;"  > $difmap_temp1
setenv DIFMAP_LOGIN $difmap_temp1 
grep -q "astrogeo patches" %%difmap_exec%%
if ( $status != 0 ) then
     echo "dimap cannot run -- wrong version of difmap"
     echo "You need apply astrogeo patches an re-compile difmap"
     exit 1
endif
%%difmap_exec%%
cleanup:
if ( -f ${prefix}.uvf  ) mv ${prefix}.uvf  ${prefix}_uvs.fits 
if ( -f ${prefix}.fits ) mv ${prefix}.fits ${prefix}_map.fits 
rm $difmap_temp1
rm $difmap_temp2
