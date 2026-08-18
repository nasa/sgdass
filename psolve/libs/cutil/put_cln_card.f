      SUBROUTINE PUT_CLN_CARD (IDB,CTYPE,IMODE,NUMC,CALNAMES,KERR)
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  PUT_CLN_CARD PROGRAM SPECIFICATION
!
! 1.1 Write a list of calibration/contribution names
!     of a given type (contributions or regular, flyby or
!     regular zenith calibrations for display/identification)
!     for a given data base to the namfil.
!     Packs the names into some number of namfil records and writes
!     them using putcard.  Can be called to create or update a
!     section.
!
! 1.2 REFERENCES:
!
! 1.3 assumptions:
!                  assuming the names are either 8 or 16 characters
!
!
! 2.  PUT_CLN_CARD INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IDB,NUMC,IMODE
      CHARACTER*4 CTYPE
      CHARACTER*(*) CALNAMES(*)
!
!  IDB - number of the data base
!  CTYPE - type of calibration/contribution; the actual code for the namfil
!          card that lists the names for that type
!  IMODE = 1 for updating the section
!          2 for creating the section
!  NUMC - the number of calibration/contributions of this type to be stored
!         in this namfil list
! CALNAMES - the list of calibration/contribution names to be written
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 KERR
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: putcard,ferr
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*70 JBUF
      CHARACTER*16 TNAMES(8)
      CHARACTER*100 ERRSTR
      INTEGER*2 ICARD,ICT,IERR,ILOAD,ILOAD1,ILOAD2,NUM_CARDS, &
     &   NUM_PER_CARD,ISIZE
!
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  910717  Created
!
!
! 5.  PUT_CLN_CARD PROGRAM STRUCTURE
!
!     Put calibration/contribution name cards into the namfil until
!     the desired number of names has been written.
!
      KERR = 0
      IF (CTYPE .EQ. 'CLZN') THEN
        ISIZE = 16
      ELSE
        ISIZE = 8
      END IF
      NUM_PER_CARD = 64 / ISIZE
      IF (NUMC / NUM_PER_CARD * NUM_PER_CARD .EQ. NUMC) THEN
        NUM_CARDS = NUMC / NUM_PER_CARD
      ELSE
        NUM_CARDS = NUMC / NUM_PER_CARD + 1
      END IF
!
!     The namfil access routines cannot add records, once analysis has
!     begun.  They can only rewrite existing cards.  So the namfil must
!     contain cards for every possible calibration name from the start.
!     This is not a problem with the non-flyby calibrations, the zenith
!     non-flyby calibrations and the contributions, which can only be
!     placed in the namfil when analysis starts, when it it easy to pull
!     the calibration values out of the data base.  However, flyby
!     calibrations can be added in the middle of analysis, since their values
!     are generated, not read from the data base.  So the user may want
!     to add some of these, and the full possible number of names must be
!     written when the namfil is created.
!
      IF (CTYPE .EQ. 'FCLN' .AND. IMODE .EQ. 2) NUM_CARDS = 14
!
      DO ICARD = 1, NUM_CARDS
!
!      Load up buffer for this card
!
        ILOAD1 = (ICARD - 1) * NUM_PER_CARD + 1
        ILOAD2 = ICARD * NUM_PER_CARD
        DO ILOAD = ILOAD1,ILOAD2
          ICT = ILOAD - ILOAD1 + 1
          IF (ILOAD .LE. NUMC) THEN
            TNAMES (ICT) = CALNAMES(ILOAD)
          ELSE
            TNAMES (ICT) = '-*-*-*-*-*-*-*-*'
          END IF
        END DO
!
        IF (ISIZE .EQ. 8) THEN
          WRITE (JBUF, &
     &      "(A4,1X,8A8,1X)")CTYPE, (TNAMES(ICT)(1:8),ICT = 1,8)
        ELSE ! ASSUMING ISIZE = 16
          WRITE (JBUF, &
     &      "(A4,1X,4A16,1X)")CTYPE, (TNAMES(ICT),ICT = 1,4)
        END IF
!
!       write this card
!
        IF (IMODE .EQ. 2) THEN
          CALL PUTCARD( IDB, CTYPE, INT2(2), JBUF, IERR )
        ELSE IF (ICARD .EQ. 1) THEN
          CALL PUTCARD( IDB, CTYPE, INT2(1), JBUF, IERR )
        ELSE
          CALL PUTCARD( IDB, CTYPE, INT2(0), JBUF, IERR )
        END IF
!
        IF (IERR.NE.0) THEN
          KERR = IERR
          WRITE (ERRSTR, &
     &      '("IN SUB PUT_CLN_CARD FOR ",A4)')CTYPE
          call ferr( IERR, errstr, INT2(0), INT2(0) )
        END IF
      END DO
!
      RETURN
      END
