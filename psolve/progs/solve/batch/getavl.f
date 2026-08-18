      SUBROUTINE GETAVL(STRING)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GETAVL PROGRAM SPECIFICATION
!
! 1.1 Get the list of available flyby calibrations.  Used to get it from
!      the batch control file.  (The list was composed of the list following
!      the available keyword in the $calibration section.)  Now getting the
!      list from a standard file used throughout SOLVE.  This file
!      theoretically tells which flyby calibrations SOCAL is set up to handle,
!      but must be kept up to date with socal, to be accurate.
!
! 1.2 REFERENCES:
!
! 2.  GETAVL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables: input string
!     eventually should be blank, as more users switch their batch control
!     files to the new format for the available keyword.  But for now,
!     for backwards compatibility, need to parse it to throw away a list
!     of calibration names, that will no longer be used, but must be read
!     to get to the next keyword.
!
      CHARACTER*(*) STRING
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
      INTEGER*2 IUNIT, LIMIT, IERR, I
!
      CHARACTER*8 TOKEN
      INTEGER*2 LENGTH,CFREAD,IDUM,jcaff1(7)
      LOGICAL*2 CFEOF
      CHARACTER*1 BSLASH
      BSLASH = CHAR(92)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   kdb   8/6/91 totally restructured, to access standard file of flyby cals,
!                not the batch control file
!
! 5.  GETAVL PROGRAM STRUCTURE
!
! Call the subroutine which reads a file to get the list of flyby cals that
!   are theoretically available to the current version of socal
!
      IUNIT = 65
      LIMIT = MAX_FLY
      CALL FLYAVL_N(IUNIT,LIMIT,AVAIL,IAVAIL,IERR,jcaff1 )
      IF (IERR.NE.0) THEN
        CALL FERR( IERR, 'ERR RETURN FROM FLYAVL_N IN GETAVL - STOPPING', &
     &       INT2(0), INT2(0) )
        CALL FATAL('ERR RETURN FROM FLYAVL_N IN GETAVL' )
      END IF
!
      IF (IAVAIL .EQ. 0) THEN
        CALL FERR( INT2(13007), ' IN GETAVL - FLYCAL AVAIL FILE EMPTY', &
     &       INT2(0), INT2(0) )
        CALL FATAL('IN GETAVL - FLYCAL AVAIL FILE EMPTY' )
      END IF
!
!     Replace any underscores with blanks
!
      DO I = 1,IAVAIL
        CALL UNDSCR(AVAIL(I) )
      END DO
!
!     For backwards compatibility, if the user has specified
!     a list of flyby calibs after the keyword, just read them
!     off and ignore them, to get in position for the next keyword.
!
      if(string.ne.'DEFAULT') then
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      DO WHILE (TOKEN.NE.' ')
        IF(TOKEN.EQ.BSLASH) THEN
          LENGTH=CFREAD(STRING)
          IF(STRING(1:1).EQ.'$'.OR.CFEOF(IDUM)) THEN
            CALL FERR( INT2(13010), 'ILLEGAL CONTINUATION LINE '// &
     &           STRING(1:10), INT2(0), INT2(0) )
          ENDIF
        END IF
        CALL SPLITSTRING(STRING,TOKEN,STRING )
      ENDDO
      endif
!
      RETURN
      END
