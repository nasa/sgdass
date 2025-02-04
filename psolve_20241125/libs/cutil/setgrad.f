      SUBROUTINE SETGRAD(FJDOBS,NTAGS,INDAY)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SETGRAD PROGRAM SPECIFICATION
!
! 1.1 Set up gradient epochs in automatic mode.
!
! 1.2 REFERENCES:
!
! 2.  SETGRAD INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2   NTAGS
      REAL*8  FJDOBS,INDAY
!
! FJDOBS - Julian date of the observation
! INDAY - Interval between epochs, in days
! NTAGS - Number of gradient epochs
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: clall
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   I
      REAL*8  NEWEP
      REAL*8  MINUTE
!
! I - Loop index
! MINUTE - One minute expressed as fraction of a day
! NEWEP - Julian date of next new epoch
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  SETGRAD PROGRAM STRUCTURE
!
      MINUTE=1.0D0/(60.0*24.0)
!
!  Set first epoch
!
      CALL SBIT( LGRAD(1), INT2(1), INT2(1) )
      IF(NTAGS.LT.1) NTAGS=NTAGS+1
      TGRAD(1)=FJDOBS-MINUTE
!
!  Set remaining epochs
!
      DO I=2,NTAGS
          NEWEP=TGRAD(1)+(I-1)*INDAY
          CALL SBIT( LGRAD(1), I, INT2(1) )
          TGRAD(I)=NEWEP
      ENDDO
      DO I=1,NUMSTA
          NUMGRAD(I)=NTAGS
      ENDDO
      RETURN
      END
