      SUBROUTINE UNIX_FILE_SORT ( FILE_NAME, OPTIONS_STRING )
      implicit none
      Character*(*) file_name,options_string
      Character*200 string
      Integer*2 ilen, trimlen, jlen,ierr,system2_sh
      Integer*4 Ierr4
!
!     :93.07.01:jwr: string contstruct made more complicated
!               to avoid range checking error.
!   pet  990409  Replaced call of system with call of system2_sh in order to
!                fix SYSTEM-APR99 bug
!
      string= ' '
      string = file_name
      ilen = trimlen(string)
      jlen = trimlen(options_string)
!
      If(jlen.gt.0) then
        string = &
     &    'sort '//options_string(1:jlen)// &
     &    ' -o'//File_name(1:ilen)// &
     &    ' '//File_name(1:ilen)
      else
        string = &
     &    'sort '// &
     &    ' -o'//File_name(1:ilen)// &
     &    ' '//File_name(1:ilen)
      endif
      Call zterm(string,Ierr4)
      ilen = trimlen(string)
!     write(6,'("unix_file_sort: ",a)') string (1:ilen)
      ierr = system2_sh(string,Ierr)
      if(ierr.ne.0) then
        write(6,'( &
     &  "Error in sorting file = ",a,/, &
     &  " ierr = ",i5)') File_name, ierr
        stop
      Endif
!
      return
      end
