      SUBROUTINE SET_IONOFLAG ( IDBSEL, IDATYP )
! ************************************************************************
! *                                                                      *
! *   Routine  SET_IONOFLAG sets forcibly ionsphere correction flag      *
! *   in accordance with the solution type for all databases selected    *
! *   in solution.                                                       *
! *                                                                      *
! *  ###  09-MAR-98  SET_IONOFLAG  v1.2  (c)  L. Petrov  14-APR-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      INTEGER*4  IDBSEL, IDATYP
      CHARACTER  CDBNAM(15)*10, JBUF*70
      INTEGER*2  LDBNAM(5,15)
      EQUIVALENCE (CDBNAM,LDBNAM(1,1))
      INTEGER*2  IDBVER(15), NUMDB
      INTEGER*4  IDBEND(15)
      LOGICAL*2  KBIT
      LOGICAL*4  DATYP_INQ
      INTEGER*4  SET_IONO, GION__IOT, PHION__IOT, NONE__IOT, IOS, IUER
      INTEGER*2  I1, NAMSIT(4), IICAL, ICALSTAT, ICALAPP, ICONT, JERR, KERR
      PARAMETER  (  NONE__IOT = 1 )
      PARAMETER  (  GION__IOT = 2 )
      PARAMETER  ( PHION__IOT = 3 )
!
      IUER = -1
!
! --- Inquiry: how many database do we have in scratch area? Answer: NUMDB
!
      CALL OPENNAMFIL()
      CALL DBPOX ( NUMDB, LDBNAM, IDBVER, IDBEND )
!
! --- Cycle on all database "in solution"
!
      DO 410 I1=1,NUMDB
         IF ( KBIT ( IDBSEL, I1 ) ) THEN ! We change status only for database
!                                        ! "in solution"
!
! ----------- Determination of what is the ionosphere type to be set up.
! ----------- It depends on solution type
!
              IF ( DATYP_INQ ( IDATYP, GRPRAT__DTP ) .OR. &
     &             DATYP_INQ ( IDATYP, SNBRAT__DTP ) .OR. &
     &             DATYP_INQ ( IDATYP, GRPONL__DTP ) .OR. &
     &             DATYP_INQ ( IDATYP, SNBONL__DTP ) .OR. &
     &             DATYP_INQ ( IDATYP, RATONL__DTP )     ) THEN
!
                   SET_IONO = GION__IOT
                ELSE IF ( DATYP_INQ ( IDATYP, PHSRAT__DTP ) .OR. &
     &                    DATYP_INQ ( IDATYP, PHSONL__DTP )     ) THEN
                   SET_IONO = PHION__IOT
                ELSE
                   SET_IONO = NONE__IOT
              END IF
!
! ----------- Update of ionosphere calibration status in NAMFIL for all stations
! ----------- of the I1-the database
!
              KERR  = 0
              ICONT = 1
              DO WHILE ( KERR .EQ. 0 )
!
! -------------- Reading calibration card
!
                 CALL GETCARD ( I1, 'CALS', ICONT, JBUF, KERR )
                 IF ( KERR .EQ. INT2(0) ) THEN ! Good card found
                      ICONT = 0
                      READ ( JBUF, 110, IOSTAT=IOS) &
     &                       NAMSIT, IICAL, ICALSTAT, ICALAPP
 110                  FORMAT ( 5X, 4A2, 1X, 3I7 )
                      IF ( IOS .NE. 0 ) THEN
                         CALL FERR ( INT2(1202), 'SET_IONOFLAG: Error while '// &
     &                       'reading NAMFIL CALS card', INT2(0), INT2(0) )
                         STOP 'SET_INOFLAG'
                      END IF
!
                      IF ( SET_IONO .EQ. NONE__IOT ) THEN
!
! ------------------------ Disable ionosphere calibration at all
!
                           CALL SBIT ( IICAL, INT2(4), INT2(0) )
                           CALL SBIT ( IICAL, INT2(5), INT2(0) )
                        ELSE IF ( SET_IONO .EQ. GION__IOT ) THEN
!
! ------------------------ Apply GION if available
!
                           IF ( KBIT ( IICAL, INT2(1) ) ) THEN
                                CALL SBIT ( IICAL, INT2(4), INT2(1) )
                                CALL SBIT ( IICAL, INT2(5), INT2(0) )
                           END IF
                        ELSE IF ( SET_IONO .EQ. PHION__IOT ) THEN
!
! ------------------------ Apply PHION if available
!
                           IF ( KBIT ( IICAL, INT2(2) ) ) THEN
                                CALL SBIT ( IICAL, INT2(4), INT2(0) )
                                CALL SBIT ( IICAL, INT2(5), INT2(1) )
                           END IF
                      END IF
!
                      WRITE ( JBUF, 120 ) NAMSIT, IICAL, ICALSTAT, ICALAPP
 120                  FORMAT ( "CALS ", 4A2, 1X, 3I7 )
                      CALL PUTCARD ( I1, 'CALS', INT2(4), JBUF, JERR )
                      IF ( JERR .NE. INT2(0) ) THEN
                         CALL FERR ( INT2(1204), 'SET_IONOFLAG: Error while '// &
     &                       'writing NAMFIL CALS card', INT2(0), INT2(0) )
                         STOP 'SET_INOFLAG'
                     END IF
                 END IF ! Good card found
              END DO
         END IF
 410  CONTINUE
      CALL CLOSENAMFIL()
!
      RETURN
      END  !#!  SET_IONOFLAG  #!#
