      SUBROUTINE SETEP_STA(ISITE,FJDOBS,NEPOCS,INDAY)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SETEP_STA PROGRAM SPECIFICATION
!
! 1.1 Set up automatic atmosphere parameterization for one station.
!
! 1.2 REFERENCES:
!
! 2.  SETEP_STA INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2   ISITE,NEPOCS
      REAL*8  FJDOBS,INDAY
!
! FJDOBS - Julian date at start of observation
! INDAY - Interval between epochs, in days
! ISITE - Site number of this station
! NEPOCS - Number of atmosphere epochs
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: autoa
!       CALLED SUBROUTINES: clatm
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   JATM,I
      REAL*8  NEWEP
      REAL*8  MINUTE
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  SETEP_STA PROGRAM STRUCTURE
!
! Calculate one minute as a fraction of a day
!
      MINUTE=1.0D0/1440.0D0
!
! Clear out atmosphere epochs for this site
!
      CALL CLATM(ISITE )
      CALL SBIT( LATM(1,1), INT2(IATSTR(ISITE)+1), INT2(1) )
      CALL SBIT( LATM(1,2), INT2(IATSTR(ISITE)+1), INT2(0) )
      CALL SBIT( LATM(1,3), INT2(IATSTR(ISITE)+1), INT2(0) )
      TATM(IATSTR(ISITE)+1)=FJDOBS-MINUTE
      DO I=2,NEPOCS
          NEWEP=TATM(IATSTR(ISITE)+1)+(I-1)*INDAY
          CALL INATM(ISITE,NEWEP,JATM )
          CALL SBIT( LATM(1,1), JATM, INT2(1) )
          CALL SBIT( LATM(1,2), JATM, INT2(0) )
          CALL SBIT( LATM(1,3), JATM, INT2(1) )
      ENDDO
      RETURN
      END
