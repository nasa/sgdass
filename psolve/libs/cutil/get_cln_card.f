      SUBROUTINE GET_CLN_CARD (IDB,CTYPE,NUMC,CALNAMES,KERR)
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GET_CLN_CARD PROGRAM SPECIFICATION
!
! 1.1 Read in a list of calibration/contribution names
!     of a given type (contributions or regular, flyby or
!     regualar zenith calibrations for display/identification)
!     for a given data base from the namfil.
!
! 1.2 REFERENCES:
!
! 1.3 assumptions:  assuming the names are either 8 or 16 characters long
!
! 2.  GET_CLN_CARD INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IDB,NUMC
      CHARACTER*4 CTYPE
!
!  IDB - number of the data base
!  CTYPE - type of calibration/contribution; the actual code for the namfil
!          card that lists the names for that type
!  NUMC - the number of calibration/contributions of this type contained
!         in the namfil
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) CALNAMES(*)
      INTEGER*2 KERR
!
! CALNAMES - the list of calibration/contribution names returned
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: getcard,ferr
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*70 JBUF
      CHARACTER*100 ERRSTR
      INTEGER*2 ICARD,ICT,IERR,ILOAD,ILOAD1,ILOAD2,INEXT,ISIZE, &
     &    NUM_CARDS,NUM_PER_CARD
      INTEGER*4  ios
      CHARACTER*16 TNAMES(8)
!
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  910716  Created
!
!
! 5.  GET_CLN_CARD PROGRAM STRUCTURE
!
!     Pull calibration/contribution name cards out of the namfil until
!     all of the names have been read into a list.
!
      KERR = 0
      IF (CTYPE .EQ. 'CLZN') THEN
        ISIZE = 16
      ELSE
        ISIZE = 8
      END IF
!
      NUM_PER_CARD = 64 / ISIZE
      IF (NUMC / NUM_PER_CARD * NUM_PER_CARD .EQ. NUMC) THEN
        NUM_CARDS = NUMC / NUM_PER_CARD
      ELSE
        NUM_CARDS = NUMC / NUM_PER_CARD + 1
      END IF
!
      INEXT = 1
      DO ICARD = 1, NUM_CARDS
        CALL GETCARD(IDB,CTYPE,INEXT,JBUF,IERR )
        INEXT = 0
        IF (IERR.EQ.0) THEN
          IF (ISIZE .EQ. 16) THEN
            READ (JBUF,"(5X,4A16,1X)", &
     &           IOSTAT=ios)(TNAMES(ICT),ICT = 1,4)
          ELSE
            READ (JBUF,"(5X,8A8,1X)", &
     &            IOSTAT=ios)(TNAMES(ICT)(1:8),ICT = 1,8)
          END IF
          call ferr( INT2(ios), "Reading NAMFIL cal card", INT2(0), INT2(0) )
          ILOAD1 = (ICARD - 1) * NUM_PER_CARD + 1
          ILOAD2 = ICARD * NUM_PER_CARD
          IF (ILOAD2 .GT. NUMC) ILOAD2 = NUMC
          DO  ILOAD = ILOAD1,ILOAD2
            ICT = ILOAD - ILOAD1 + 1
            CALNAMES(ILOAD)(1:ISIZE) = TNAMES(ICT)(1:ISIZE)
          END DO
        ELSE
          KERR = IERR
          IF (IERR .EQ. 1) THEN
            WRITE (6,'("NAMFIL CARD WHICH COUNTS CALIBS THOUGHT ")')
            WRITE (6,'("IT HAD ",I3, " CALIBS = ",I3, &
     &        " NAME CARDS FOR THIS CALIB TYPE")')  NUMC, NUM_CARDS
            WRITE (6,'("BUT ONLY ",I3," NAME CARDS")') ICARD - 1
          END IF
          WRITE (errstr, &
     &      '("GET_CLN_CARD ERROR ",I5, " FOR ",A4 )') IERR,CTYPE
          call ferr( INT2(502), errstr, INT2(0), INT2(0) )
        END IF
      END DO
!
      RETURN
      END
