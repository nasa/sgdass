      SUBROUTINE SETAT(FJDOBS,NTAGS,INDAY)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SETAT PROGRAM SPECIFICATION
!
! 1.1 Set up atmosphere epochs in automatic mode.
!
! 1.2 REFERENCES:
!
! 2.  SETAT INTERFACE
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
! NTAGS - Number of atmosphere epochs
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
! 5.  SETAT PROGRAM STRUCTURE
!
      MINUTE=1.0D0/(60.0*24.0)
      CALL CLALL()
!
!  Set first epoch
!
      CALL SBIT( LATM(1,1), INT2(1), INT2(1) )
      CALL SBIT( LATM(1,2), INT2(1), INT2(0) )
      CALL SBIT( LATM(1,3), INT2(1), INT2(0) )
      IF(NTAGS.LT.1) NTAGS=NTAGS+1
      TATM(1)=FJDOBS-MINUTE
!
!  Set remaining epochs
!
      DO I=2,NTAGS
          NEWEP=TATM(1)+(I-1)*INDAY
          CALL SBIT( LATM(1,1), I, INT2(1) )
          CALL SBIT( LATM(1,2), I, INT2(0) )
          CALL SBIT( LATM(1,3), I, INT2(1) )
          TATM(I)=NEWEP
      ENDDO
      DO I=1,NUMSTA
          NUMATM(I)=NTAGS
      ENDDO
      RETURN
      END
