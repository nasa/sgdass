      SUBROUTINE DOCALI ( FL_NOCAL, NAMCAL, NAMFCAL, ICALS, IFCALS, &
     &                    IONCTL, DBNAME_MES )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DOCALI PROGRAM SPECIFICATION
!
! 1.1 Set up flyby and non-flyby calibrations, according to what
!     the user requested through the batch control file and what
!     the status of namfil is.
!
! 1.2 REFERENCES:
!
! 2.  DOCALI INTERFACE
!
! 2.1 Parameter File
!
      INCLUDE 'solve.i'
      INCLUDE 'dcali.i'
      INCLUDE 'socom.i'
!
! 2.2 INPUT Variables:
!
      LOGICAL*4  FL_NOCAL
      INTEGER*2  ICALS, IFCALS
      CHARACTER  NAMCAL(ICALS)*(*), NAMFCAL(112)*(*), IONCTL*(*), DBNAME_MES*(*)
      INTEGER*4  IUER
!
! ICALS - Number of non-flyby calibrations gotten from NAMFIL
! IFCALS - Number of flyby calibrations gotten from NAMFIL
! IONCTL - Ionosphere control variable (ON,OFF or DEFAULT)
! NAMCAL - non-flyby Calibration names from NAMFIL
! NAMFCAL - flyby Calibration names from NAMFIL
! DBNAME_MES - database name + version
!
! 2.3 OUTPUT Variables:
!
! NAMFCAL, IFCALS - these may be updated, if the called subs must add
!     flyby calibrations to fulfill what the batch control file requested
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: setnam
!       CALLED SUBROUTINES: doion,dositecals,some utilities
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*4 LCARD
      CHARACTER*8 NAMSIT(MAX_ARC_STA)
      CHARACTER*70 JBUF
      INTEGER*2 NUMDB, ICONT, IERR, IONFLG(MAX_ARC_STA), &
     &          NAMAVL(MAX_ARC_STA), NAMAPL(MAX_ARC_STA), ICT, IST, &
     &          JCAFFL(7,MAX_ARC_STA), NSSS, NSSF
      INTEGER*4 IOS
      CHARACTER*100 ERRSTR
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   kdb   8/5/91      NEW NAMFIL/CORFIL SCHEME (GET EVERYTHING FROM NAMFIL,
!                     AND REGULAR AND FLYBY CALS SEPARATED)
!   pet   2000.09.15  Inproved comments
!   pet   2006.07.06  Added support of argument FL_NOCAL
!
! 5.  DOCALI PROGRAM STRUCTURE
!
!     A. Get all the required information out of namfil, for every
!        station.   Can no longer get, apply and replace info, station
!        by station, because info is no longer kept in a single place
!        in namfil.
!
      NUMDB=1
!
      IF ( FL_NOCAL      .OR.  &
     &     FL_RESET      .OR.  &
     &     L_KEE .NE. 0  .OR.  &
     &     L_ENB .NE. 0  .OR.  &
     &     L_DIS .NE. 0        ) THEN
!
! -------- Post MAY2000 syntax of calibration
!
           IUER = -1
           CALL SET_CALIB ( FL_RESET, L_KEE, L_ENB, L_DIS, KEECAL, &
     &                      ENBCAL, DISCAL, IONCTL, DBNAME_MES, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 2861, -2, 'DOCALI(BATCH)', 'Error in attmpt '// &
     &              'to apply calibrations when the session '//DBNAME_MES// &
     &              ' was being processed' )
                CALL EXIT ( 1 )
           END IF
           RETURN
      END IF
!
! --- Get the bit array indicating which flyby cals are applied
!
      IST = 0
      IERR = 0
      NSSF = 0
      DO WHILE (IERR .EQ. 0)
         IST = IST + 1
         IF ( IST .EQ. 1 ) THEN
              CALL GETCARD ( NUMDB, 'FCLS', INT2(1), JBUF, IERR )
            ELSE
              CALL GETCARD ( NUMDB, 'FCLS', INT2(0), JBUF, IERR )
         END IF
         IF ( IERR .EQ. 0 ) THEN
              NSSF = NSSF + 1
              READ ( JBUF, "(13X,7(1X,I7),1X)", IOSTAT=IOS) &
     &                      ( JCAFFL(ICT,NSSF),ICT=1,7 )
              CALL FERR ( INT2(IOS), "DOCALI Error in Reading FCLS card", &
     &                    INT2(0), INT2(0) )
            ELSE IF (IERR .NE. 1) THEN ! not ok, not end of list -- so error
              WRITE ( ERRSTR, '("IN SUB DOCALI GETTING A FCLS CARD")' )
              CALL FERR ( IERR, ERRSTR, INT2(0), INT2(0) )
         END IF
      END DO
!
! --- Now the station names and bit variable indicating which
! --- non-flyby calibrations are applied (also info for which cals
! --- are available and for which iono cals are applied)
!
      IST = 0
      IERR = 0
      NSSS = 0
      DO WHILE ( IERR .EQ. 0 )
         IST = IST + 1
         IF ( IST .EQ. 1 ) THEN
              CALL GETCARD ( NUMDB, 'CALS', INT2(1), JBUF, IERR )
           ELSE
              CALL GETCARD ( NUMDB, 'CALS', INT2(0), JBUF, IERR )
         END IF
!
         IF ( IERR .EQ. 0 ) THEN
               NSSS = NSSS + 1
               READ ( JBUF, 99, IOSTAT=IOS ) NAMSIT(NSSS), IONFLG(NSSS), &
     &                                       NAMAVL(NSSS), NAMAPL(NSSS)
               CALL FERR ( INT2(IOS), "DOCALI Reading CALS card", INT2(0), &
     &                     INT2(0) )
99             FORMAT ( 5X, A8, 1X, I7, I7, I7, 35X )
           ELSE IF (IERR .NE. 1) THEN ! not ok, not end of list -- so error
               WRITE ( ERRSTR, '("IN SUB DOCALI GETTING A CALS CARD ")')
               CALL FERR ( IERR, ERRSTR, INT2(0), INT2(0) )
         END IF
      END DO
!
      IF ( NSSS .NE. NSSF ) THEN
           WRITE ( ERRSTR, '("IN SUB DOCALI ", "MISMATCH # STATS IN FCLS "// &
     &                       "CALS CARDS ",I5,I5)' ) NSSF, NSSS
           CALL FERR ( INT2(16130), ERRSTR, INT2(0), INT2(0) )
      END IF
!
! --- B. Now loop over the stations, applying the appropriate calibrations
! --- at each.  Comparing what the user has requested via the batch
! --- control file to what is applied in the superfile according to
! --- namfil, what is available for the non-flyby cals according to
! --- namfil and what is available for the flyby cals in the current version
! --- of socal.
!
      DO IST = 1,NSSS
!
! ------ Set up ionosphere calibration control
!
         CALL DOION ( IONCTL, IONFLG(IST) )
!
! ------ set up site dependent calibrations (flyby and non-flyby)
!
         CALL DOSITECALS ( NAMSIT(IST), NAMCAL, NAMFCAL, ICALS, IFCALS, &
     &                     NAMAVL(IST), NAMAPL(IST), JCAFFL(1,IST) )
      END DO
!
! --- C. A new set of calibrations is probably being applied for
! ---    this arc.  Rewrite its namfil to reflect the current choices.
!
! --- First doing the ionosphere and non-flyby calibrations
!
      LCARD = 'CALS'
      ICONT = 1
      DO IST = 1,NSSS
         WRITE ( JBUF, 199 ) LCARD, NAMSIT(IST), IONFLG(IST), &
     &                       NAMAVL(IST), NAMAPL(IST)
 199     FORMAT ( A4, 1X, A8, 1X, I7, I7, I7, 35X )
         CALL PUTCARD ( NUMDB, LCARD, ICONT, JBUF, IERR )
         CALL FERR ( IERR, 'DOCALI error in docali putting cals card', &
     &        INT2(0), INT2(0) )
         ICONT = 0
      END DO
!
! --- Next do the flyby calibrations
!
      LCARD = 'FCLS'
      ICONT = 1
      DO IST = 1,NSSF
         WRITE ( JBUF, "('FCLS',1X,A8,7(1X,I7),1X)") NAMSIT(IST), &
     &                   (JCAFFL(ICT,IST),ICT=1,7)
         CALL PUTCARD ( NUMDB, LCARD, ICONT, JBUF, IERR )
         CALL FERR ( IERR, 'DOCALI Error in docali putting fcls card', &
     &        INT2(0), INT2(0) )
         ICONT = 0
      END DO
!
      RETURN
      END  !#!  DOCALI  #!#
