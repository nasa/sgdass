      SUBROUTINE ADDSTR(CNTCAL,TOKEN)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ADDSTR PROGRAM SPECIFICATION
!
! 1.1 Add a string to current frame.
!
! 1.2 REFERENCES:
!
! 2.  ADDSTR INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) TOKEN
      INTEGER*2 CNTCAL
!
! CNTCAL - Current frame number
! TOKEN - Stirng to be added
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'solve.i'
      INCLUDE 'dcali.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gcalib,getgrp,gtcalst
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*8 BUFF
      INTEGER*2 IBUFF(4),ISTART,IEND,I
!
      EQUIVALENCE (BUFF,IBUFF(1))
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  ADDSTR PROGRAM STRUCTURE
!
      ISTART=ABS(IACALI(CNTCAL))*4    + CNTCAL
      IACALI(CNTCAL) = IACALI(CNTCAL) + SIGN(INT2(1),IACALI(CNTCAL))
      IEND=ABS(IACALI(CNTCAL))*4      + CNTCAL
      IF ( IEND .GT. MAX_CALI_WORDS ) THEN
           CALL FERR( INT2(14010), 'TOO MANY CALIBRATIONS', INT2(0), INT2(0) )
      ENDIF
!
      BUFF=TOKEN
      DO I=1,8
         IF(BUFF(I:I).EQ.'_') BUFF(I:I)=' '
      ENDDO
      DO I=1,4
         IACALI(ISTART+I)=IBUFF(I)
      ENDDO
!
      RETURN
      END
