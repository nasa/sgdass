      SUBROUTINE NUMERICTIME ( SECS, OUTST )
      IMPLICIT NONE
      INTEGER*4 SECS
      CHARACTER*(*) OUTST
      INCLUDE 'fclib.i'
!
!  convert SECS: time in seconds since 70/01/01.00:00:00 UT
!  to a character string OUTST: in the format: 700101.000000
!  blank fill or truncate if OUTST is not 13 characters long
!
      CHARACTER*4 SEC,MIN,MON
      CHARACTER*5 HOUR,MDAY,YEAR
      PARAMETER ( &
     &           SEC ='sec'//CHAR(0), &
     &           MIN ='min' //CHAR(0), &
     &           HOUR='hour'//CHAR(0), &
     &           MDAY='mday'//CHAR(0), &
     &           MON='mon'//CHAR(0), &
     &           YEAR='year'//CHAR(0) &
     &          )
!
      INTEGER*2   I
      INTEGER*4   IERR4, I4, VALS(9)
      ADDRESS__TYPE :: TM
      CHARACTER   DINTTODECMLR*2, STRING*13
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   pet   2001.02.26  Fixed a bug: there was function ptr_ch in one place which
!                     requires ptr_nc. Improved comments.
!   pet   2017.10.20  Replaced with call gfortran GMTIME
!
!CCC
!
! --- use gmtime to convert the time to useful quantities
!
#ifdef GNU
      CALL GMTIME ( SECS, VALS ) 
      IF ( VALS(6) .GE. 100 ) VALS(6) = VALS(6) - 100
      WRITE ( UNIT=STRING(1:13), FMT='(I2,I2,I2,".",I2,I2,I2)' ) &
     &        VALS(6), VALS(5)+1, VALS(4), VALS(3), VALS(2), VALS(1) 
#else
!
! --- This is part is broken
!
      TM = FC_GMTIME ( PTR_CH(SECS) )
!
! --- year:
!
      IERR4 = FC_TM_G ( TM, PTR_CH(YEAR), I4 )
      STRING(1:2) = DINTTODECMLR(I4)
!
! --- month:
!
      IERR4 = FC_TM_G ( TM, PTR_CH(MON), I4 )
      i4=i4+1
      STRING(3:4) = DINTTODECMLR(I4)
!
! --- doy of month:
!
      IERR4 = FC_TM_G ( TM, PTR_CH(MDAY), I4 )
      STRING(5:6) = DINTTODECMLR(I4)
!
      STRING(7:7)='.'
!
! --- hour:
!
      IERR4 = FC_TM_G ( TM, PTR_CH(HOUR), I4 )
      STRING(8:9) = DINTTODECMLR(I4)
!
! --- min:
!
      IERR4 = FC_TM_G ( TM, PTR_CH(MIN), I4 )
      STRING(10:11) = DINTTODECMLR(I4)
!
! --- sec:
!
      IERR4 = FC_TM_G ( TM, PTR_CH(SEC), I4 )
      STRING(12:13) = DINTTODECMLR(I4)
#endif
!
! --- Replace leading blanks with zeroes
!
      DO I=1,13
        IF(STRING(I:I).EQ.' ') STRING(I:I)='0'
      ENDDO
!
! --- Let fortran string assignment rules take care of truncation or padding
!
      OUTST=STRING
!
      RETURN
      END  !#!  NUMERICTIME  #!#
