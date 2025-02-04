      SUBROUTINE GETUSE(STRING)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GETUSE PROGRAM SPECIFICATION
!
! 1.1 Get a calibration use/for entry.
!
! 1.2 REFERENCES:
!
! 2.  GETUSE INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING
!
! STRING - Input string
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'calcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gcalib
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  GETUSE PROGRAM STRUCTURE
!
! increment counter and check whether we have too many entries
!
      ICLUSE=ICLUSE+1
      IF(ICLUSE.GT.MAX_USE) THEN
        CALL FERR( INT2(13005), 'TOO MANY USE/FOR ENTRIES', INT2(0), INT2(0) )
      ENDIF
!
! Pull out argument of the USE clause
!
      CALL SPLITSTRING(STRING,CALUSE(ICLUSE),STRING )
      CALL UNDSCR(CALUSE(ICLUSE) )
!
! Pull out next token and make sure it is 'FOR'
!
      CALL SPLITSTRING(STRING,CALFOR(ICLUSE),STRING )
      IF(CALFOR(ICLUSE).NE.'FOR') THEN
        CALL FERR( INT2(13010), 'NO FOR IN USE/FOR ENTRY', INT2(0), INT2(0) )
      ENDIF
!
! Pull out argument of the FOR clause; issue error if missing
!
      CALL SPLITSTRING(STRING,CALFOR(ICLUSE),STRING )
      CALL UNDSCR(CALFOR(ICLUSE) )
      IF(CALFOR(ICLUSE).EQ.' ') THEN
        CALL FERR( INT2(13015), 'MISSING FOR CLAUSE IN USE/FOR ENTRY', &
     &       INT2(0), INT2(0) )
      ENDIF
!
      RETURN
      END
