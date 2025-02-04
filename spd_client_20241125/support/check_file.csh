#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Usage: check_file.csh <file> <mode>                                *
# *                                                                      *
# *   C-shell program check_file.csh  checks existing fiule or a commend *
# *   in dependence of the second argument.                              *
# *                                                                      *
# *   1) file_read: Checks whether the file specified in the first       *
# *      argument exists and has a reading permission.                   *
# *                                                                      *
# *   2) file_execute: Checks whether the file specified in the first    *
# *      argument exists and has an execution permission.                *
# *                                                                      *
# *   3) command: Checks whether the command present in the system and   *
# *      can be executed.                                                *
# *                                                                      *
# *   check_file.csh  reports his activity at the screen. It returns     *
# *   status 0 in the case of success and 1 in the case of failure.      *
# *                                                                      *
# *  ### 02-JUN-2000 check_file.csh v1.2 (c) L. Petrov  03-MAY-2004 ###  *
# *                                                                      *
# ************************************************************************
#
if ( $1 == "" ) exit 0
set OS_name = `uname`
switch ( $OS_name )
   case "HP-UX":
     set ECHO = "/bin/echo"
     set qt = "\0042"
     breaksw
   case "SunOS":
     set ECHO = "/bin/echo"
     set qt = "\042"
     breaksw
   case "Linux":
     set ECHO = "/bin/echo -e"
     set qt = "\042"
     breaksw
   case "Darwin":
     set ECHO = "/bin/echo"
     set qt = "\042"
     breaksw
endsw
switch ( $2 ) 
    case "file_read":
#
      $ECHO "Checking file $1 ...\c"
      if ( -e $1 ) then
        else
           $ECHO "  not found"
           exit (1)
      endif
      if ( -r $1) then
           $ECHO "  ok"
           exit ( 0 )
         else 
           $ECHO "  exist, but not readable"
           exit (1)
      endif
      breaksw
    case "file_execute":
#
      $ECHO "Checking file $1 ...\c"
      if (-e $1) then
        else
           $ECHO "  not found"
           exit (1)
      endif
      if ( -x $1) then
           $ECHO "  ok"
           exit ( 0 )
         else 
           $ECHO "  exist, but not executable"
           exit (1)
      endif
      breaksw
    case "command":
#
      $ECHO "Checking command $1 ...\c"
      set which_answer = `which $1`
#      set which_substr = `expr substr "$which_answer" 1 1`
      set which_substr = `echo "$which_answer"  | awk '{print substr ($1, 1, 1)}'`
      if ( "$which_substr" != "/" ) then
           $ECHO "  not found"
           exit ( 1 )
        else 
	   if ( -e "$which_answer[1]" ) then
                $ECHO "  ok"
                exit ( 0 )
             else
                $ECHO "  found, but not executable"
                exit ( 1 )
           endif
      endif
      breaksw
    case default:
      $ECHO "check_file.csh: Wrong second argument $2"
      exit (-2)
endsw
