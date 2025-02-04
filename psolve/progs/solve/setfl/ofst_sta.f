      SUBROUTINE OFST_STA ( ISTA, FJDOBS, MAX_GCLOCK_DEG )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  OFST_STA PROGRAM SPECIFICATION
!
! 1.1 Reset the first epoch of this station to the start time of
!     the observation.
!
! 1.2 REFERENCES:
!
! 2.  OFST_STA INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      REAL*8 FJDOBS
      INTEGER*2 ISTA
      integer*4 max_gclock_deg
!
! FJDOBS - Start time of this observation
! ISTA - Site number of this station
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: autoc
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 POS,KLOC
      REAL*8 MINUTE
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  OFST_STA PROGRAM STRUCTURE
!
!   reset first epoc of this station to FJDOBS
!
      MINUTE=1.0D0/1440.0D0
      POS=ICLSTR(ISTA)+1
      IF(NUMCLK(ISTA).EQ.0)CALL INCLK( ISTA, FJDOBS-MINUTE, KLOC, TRUE__L2)
      IF(NUMCLK(ISTA).GT.0.OR.KLOC.NE.-1) THEN
          LCLK(POS)=0
          if(max_gclock_deg .ge. 0) CALL SBIT( LCLK(POS), INT2(1), INT2(1) )
          if(max_gclock_deg .ge. 1) CALL SBIT( LCLK(POS), INT2(2), INT2(1) )
          if(max_gclock_deg .ge. 2) CALL SBIT( LCLK(POS), INT2(3), INT2(1) )
          FJDCL(POS)=FJDOBS-MINUTE
      ELSE
          call ferr( INT2(258), 'inserting interactive autoclock offset', &
     &         INT2(0), INT2(0) )
      ENDIF
      RETURN
      END
