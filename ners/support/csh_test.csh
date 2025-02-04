#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Test that csh is a symbolic link to tcsh.                          *
# *                                                                      *
# *  ###  19-OCT-2017  csh_test  v1.1  (c)  L. Petrov  28-NOV-2017  ###  *
# *                                                                      *
# ************************************************************************
setenv LANG   C
setenv LC_ALL C
set csh_exe = "/bin/csh"
if ( `uname` == "Darwin" ) then
     if ( `csh -cf 'echo $version' | awk '{print $1}'` == "tcsh" ) then
          exit 0
     endif
endif
set res=`file $csh_exe | sed -s "s@'@@g" | sed -s 's@\x60@@g' | sed -s 's@(@@g' | sed -s 's@)@@g'`
if ( "`echo $res | grep -v linked | grep link`" != "" ) then
     set csh_exe = `echo "$res"  | awk '{print  $5}'`
     set res=`file $csh_exe | sed -s "s@'@@g" | sed -s 's@\x60@@g' | sed -s 's@(@@g' | sed -s 's@)@@g'`
endif 
if ( "`echo $res | grep -v linked | grep link`" != "" ) then
     set csh_exe = `echo "$res"  | awk '{print  $5}'`
     set res=`file $csh_exe | sed -s "s@'@@g" | sed -s 's@\x60@@g' | sed -s 's@(@@g' | sed -s 's@)@@g'`
endif 
if ( "`echo $res | grep -v linked | grep link`" != "" ) then
     set csh_exe = `echo "$res"  | awk '{print  $5}'`
     set res=`file $csh_exe | sed -s "s@'@@g" | sed -s 's@\x60@@g' | sed -s 's@(@@g' | sed -s 's@)@@g'`
endif 
#
if ( "`echo $csh_exe |grep tcsh`" == "" ) then
     set res=`file /bin/csh | sed -s "s@'@@g" | sed -s 's@\x60@@g'`
     echo "Your csh executable is $csh_exe. This case is not supported"
     echo "$res"
     set  orig_csh_exe = `echo "$res"  | awk '{print  $5}'`
     echo "You need have tcsh installed and replace symbolic link of /bin/csh to $orig_csh_exe"
     echo "with symbolic link to /usr/bin/tcsh"
     echo " "
     echo "csh is now obsolete. tcsh takes all functionality of csh."
     echo "For compatibility, tcsh should be invoked when csh is used."
     echo " "
     if ( -f /bin/tcsh == 0 ) then
          echo "To fix the problem, you need first install tcsh as a superuser"
          echo "and then execute the following command as a superuser:"
       else
          echo "To fix the problem you need execute the following command as a superuser:"
     endif
     echo "update-alternatives --install /bin/csh     csh /usr/bin/tcsh 200"
     echo "This will cause tcsh shell be executed when csh is invoked."
     echo " "
     echo "In event if after installation you might want to undo this change, just run"
     echo "update-alternatives --remove csh /usr/bin/tcsh"
     echo " "
     exit ( 1 )
endif
exit 0
