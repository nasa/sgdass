      SUBROUTINE NEWFRM(CNTCAL)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  NEWFRM PROGRAM SPECIFICATION
!
! 1.1 Advance the calibration array one frame.
!
! 1.2 REFERENCES:
!
! 2.  NEWFRM INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 CNTCAL
!
! CNTCAL - Array position of current frame
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'solve.i'
      INCLUDE 'dcali.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gcalib,getgrp,newsec
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
! 4. HISTORY
!   WHO   WHEN  WHAT
!
! 5.  NEWFRM PROGRAM STRUCTURE
!
! If this is the first one, set position to beginning of array
!
      IF(CNTCAL.LT.1) THEN
        CNTCAL=1
      ELSE
!
! Advance in the array by an appropriate amount (one frame)
!
        IF(ABS(IACALI(CNTCAL)).LT.1) THEN
          CALL FERR( INT2(12010), 'EMPTY CALIBRATION CLAUSE', INT2(0), &
     &         INT2(0) )
        ENDIF
        CNTCAL=CNTCAL+4*ABS(IACALI(CNTCAL))+1
!
! check whether we have too many calibrations
!
        IF(CNTCAL.GT.MAX_CALI_WORDS) THEN
          CALL FERR( INT2(12020), 'TOO MANY CALIBRATIONS', INT2(0), INT2(0) )
        ENDIF
      ENDIF
!
      IACALI(CNTCAL)=0
      RETURN
      END
