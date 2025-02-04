      SUBROUTINE GCALNM ( FL_NOCAL, FL_NOCONT, FL_NOMAP, &
     &                    NAMCAL, NAMFCAL, NAMCON, NAMMCAL, &
     &                    ICALS,  IFCALS,  ICONS, L_CLM, &
     &                    MAXCAL, MAXFCAL, MAXCON, M_CLM, &
     &                    IZCALS )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GCALNM PROGRAM SPECIFICATION
!
! 1.1 Replaces gcorfl, which used to get names and counts of contributions
!     and calibrations from CORFIL.  Now will get these things from new
!     records in namfil, added about 8/91.
!
! 1.2 REFERENCES:
!
! 2.  GCALNM INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 MAXCAL, MAXFCAL, MAXCON, M_CLM, L_CLM
      LOGICAL*4 FL_NOCAL, FL_NOCONT, FL_NOMAP
!
! MAXCAL - Maximum number of non-flyby calibrations
! MAXFCAL - Maximum number of flyby calibrations
! MAXCON - Maximum number of contributions
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(8) NAMCAL(MAXCAL), NAMFCAL(MAXFCAL), NAMCON(MAXCON), &
     &              NAMMCAL(M_CLM)
      INTEGER*2 ICALS,IFCALS,ICONS,IZCALS
!
! ICALS - Number of non-flyby calibrations gotten from namfil
! IFCALS - Number of flyby calibrations gotten from namfil
! ICONS - Number of contributions gotten from namfil
! NAMCAL - non-flyby Calibration names from namfil
! NAMFCAL - flyby Calibration names from namfil
! NAMCON - Contribution names from namfil
! NAMMCAL - Mode calibration names from namfil
! izcals - number of zenith non-flyby calibrations from namfil
!    not used, but need in case the user adds flyby calibs and the
!    card counting all calib types must be updated
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: setnam
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IERR,NUMDB
      INTEGER*4 IOS
      CHARACTER*70 JBUF
      CHARACTER*4 LCARD
      CHARACTER*100 ERRSTR
!
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   kdb   8/5/91 created
!   pet   1999.11.18  Added support of mode calibrations
!   pet   2006.02.08  Added support of a case when no calibrations and/or &
!                     contributions and/or mapping are to be applied
!
! 5.  GCALNM PROGRAM STRUCTURE
!
      NUMDB = 1
      CALL GETCARD( NUMDB, 'CLCT', INT2(1), JBUF, IERR )
      IF ( IERR .NE. 0 ) THEN
           WRITE ( ERRSTR, '("IN SUB GCALNM GETTING A CLCT CARD ")')
           CALL FERR ( IERR, ERRSTR, INT2(0), INT2(0) )
        ELSE
           READ ( JBUF, "(4X,5(1X,I3),46X)", IOSTAT=IOS) &
     &           ICALS, IFCALS, IZCALS, ICONS, L_CLM
          CALL FERR (  INT2(IOS), "Decoding CLCT card", INT2(0), INT2(0) )
      END IF
!
!     Next the names of the non-flyby calibrations
!
      LCARD = 'CALN'
      CALL GET_CLN_CARD(NUMDB,LCARD,ICALS,NAMCAL,IERR )
      IF (IERR.NE.0) THEN
        WRITE (errstr, '("IN SUB GCALNM GETTING THE CALN CARDS ")')
        call ferr( IERR, errstr, INT2(0), INT2(0) )
      END IF
!
!     Next the names of the flyby calibrations
!
      LCARD = 'FCLN'
      CALL GET_CLN_CARD(NUMDB,LCARD,IFCALS,NAMFCAL,IERR )
      IF (IERR.NE.0) THEN
        WRITE (errstr, '("IN SUB GCALNM GETTING THE FCLN CARDS ")')
        call ferr( IERR, errstr, INT2(0), INT2(0) )
      END IF
!
! --- Next the names of the contributions
!
      LCARD = 'CNTN'
      CALL GET_CLN_CARD ( NUMDB, LCARD, ICONS, NAMCON, IERR )
      IF ( IERR .NE. 0 ) THEN
           WRITE ( ERRSTR, '("IN SUB GCALNM GETTING THE CNTN CARDS ")')
           CALL FERR ( IERR, ERRSTR, INT2(0), INT2(0) )
      END IF
!
! --- Next the names of the contributions
!
      LCARD = 'MCAL'
      CALL GET_CLN_CARD ( NUMDB, LCARD, L_CLM, NAMMCAL, IERR )
      IF ( IERR .NE. 0 ) THEN
           WRITE ( ERRSTR, '("IN SUB GCALNM GETTING THE MCAL CARDS ")')
           CALL FERR ( IERR, ERRSTR, INT2(0), INT2(0) )
      END IF
!
! --- Give error messages if the counts of any of the calib/contrib types
! --- is wrong (greater than the maximum number or less than one)
!
      IF ( ICALS .GT. MAXCAL ) CALL FERR ( INT2(16010), 'BATCH(gcalnm) '// &
     &    'Too many non-flyby cals in namfil', INT2(0), INT2(0) )
      IF ( IFCALS .GT. MAXFCAL)CALL FERR( INT2(16020), &
     &    'TOO MANY FLYBY CALS IN NAMFIL', INT2(0), INT2(0) )
      IF(ICONS.GT.MAXCON)CALL FERR( INT2(16030), &
     &  'TOO MANY CONTRIBS IN NAMFIL', INT2(0), INT2(0) )
!
      IF ( .NOT. FL_NOCAL ) THEN
!@           IF(ICALS.LT.1)  CALL FERR( INT2(16040), 'NO NON-FLYBY CALS IN NAMFIL', &
!@     &                               INT2(0), INT2(0) )
!@           IF(IFCALS.LT.1) CALL FERR( INT2(16050), 'NO FLYBY CALS IN NAMFIL', &
!@     &                                INT2(0), INT2(0) )
      END IF
      IF ( .NOT. FL_NOCONT ) THEN
!@          IF(ICONS.LT.1) CALL FERR( INT2(16060), 'NO CONTRIBS IN NAMFIL', &
!@     &                              INT2(0), INT2(0) )
      END IF
!
      RETURN
      END  !#!  GCALNM  #!#
