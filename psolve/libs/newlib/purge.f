      SUBROUTINE PURGE ( LU, CFIL )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! Ths routine simply purges the named file.  Lu is the logical
! unit number associated with the file and cfil is the file
! name.
!
      INTEGER*2     LU
      INTEGER*4     ios
      CHARACTER*128 CFIL
!
      CLOSE ( LU, STATUS='DELETE', IOSTAT=IOS )
      if ( ios .eq. 0) then ! no error in closing file
           open  (lu,file=cfil,status='new')
         else
           write ( 6, * ) 'error purging file'
      end if
!
      RETURN
      END  !#!  PURGE  #!#
