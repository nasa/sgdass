      SUBROUTINE SETCL_STA(ISTA,NEPOC,INDAY)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SETCL_STA PROGRAM SPECIFICATION
!
! 1.1 Set up automatic clock parameterization for one station.
!
! 1.2 REFERENCES:
!
! 2.  SETCL_STA INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NEPOC,ISTA
      REAL*8  INDAY
!
! INDAY - Epoch interval, in days
! ISTA - Site number of this station
! NEPOC - Number of clock epochs
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: autoc
!       CALLED SUBROUTINES: clclk
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J,K,KLOC
      REAL*8 NEWEP
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  SETCL_STA PROGRAM STRUCTURE
!
!   clear out clock epocs for this station
!
      CALL CLCLK(ISTA )
      call sbit( iclsta(1,iclstr(ista)+1), ista, INT2(1) )
!
!   add continued rates to list after the offset epoc
!
      DO I=2,NEPOC
          NEWEP=FJDCL(ICLSTR(ISTA)+1)+(I-1)*INDAY
          CALL INCLK( ISTA, NEWEP, KLOC, FALSE__L2 )
          IF(KLOC.NE.-1) THEN
              CALL SBIT( LCLK(KLOC), INT2(1), INT2(1) )
              CALL SBIT( LCLK(KLOC), INT2(13), INT2(1) )
          ELSE
              CALL PAUSE ( 'continued clock epoc not inserted' )
          ENDIF
      ENDDO
      RETURN
      END
