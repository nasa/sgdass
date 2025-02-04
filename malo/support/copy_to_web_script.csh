#!/bin/csh -f
#
setenv MALO_WEB_EXE $1
setenv MALO_ROOT    $2
set dir_from = $MALO_WEB_EXE
set dir_to   = $MALO_ROOT/web_script
#
cp -p $dir_from/aam_now.csh            $dir_to/
cp -p $dir_from/malo_check_status.csh  $dir_to/
cp -p $dir_from/malo_ondemand.csh      $dir_to/
cp -p $dir_from/loading_form.csh       $dir_to/
#
cp -p $dir_from/check_malo_ond.py      $dir_to/
cp -p $dir_from/check_sta_list.py      $dir_to/
cp -p $dir_from/get_aam.py             $dir_to/
cp -p $dir_from/malo_check_catcha.py   $dir_to/
cp -p $dir_from/malo_check_date.py     $dir_to/
cp -p $dir_from/malo_check_limit.py    $dir_to/
cp -p $dir_from/malo_check_stafil.py   $dir_to/
cp -p $dir_from/malo_limit.py          $dir_to/
cp -p $dir_from/malo_load_int.py       $dir_to/
cp -p $dir_from/malo_ondemand.py       $dir_to/
cp -p $dir_from/malo_query.py          $dir_to/
cp -p $dir_from/malo_select_file.py    $dir_to/
cp -p $dir_from/malo_set_catcha.py     $dir_to/
cp -p $dir_from/malo_subs.py           $dir_to/
