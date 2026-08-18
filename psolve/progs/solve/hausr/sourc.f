      SUBROUTINE SOURC ( INFO )
      IMPLICIT NONE
!
! 1.  SOURC PROGRAM SPECIFICATION
!
! 1.1 Setup information arrays and process the baseline name
!
! 1.2 REFERENCES:
!
! 2.  SOURC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 INFO(37)
!
! INFO - Information array
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc2.i'
      INCLUDE 'hausr.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: cumuloop
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER    JNAME*10
      INTEGER*4    NRECORDS, NSCA_TOT, NSCA_TOT_SUM, NSCA_USED, NSCA_USED_SUM
      REAL*8       WEI_EPO, WEI_SUM, WEI_EPO_ACC, WEI_SUM_ACC
!
!   common
!
!   type and dimension:  local
!
      INTEGER*2    SOURCE_NAME(4), NABF(16), NTMP(30), SOURCE(4)
      EQUIVALENCE  (SOURCE_NAME(1), SOU), &
     &             (SOURCE(1), SSL), &
     &             (NTMP(1), OBSU_SUM(1)), &
     &             (NTMP(7), WRMS_SUM(1)), &
     &             (NTMP(15), FACT_SUM(1)), &
     &             (NABF(1), RABF(1))
      INTEGER*4    OBSU_SUM(3), I, J, I4_ARR(2)
      REAL*8       WRMS_SUM(2), FACT_SUM(2)
      REAL*4       RABF(7)
      REAL*4       EPS_MIN, EPS_MAX
      PARAMETER  ( EPS_MAX = 2.E16  )
      PARAMETER  ( EPS_MIN = 1.0/EPS_MAX )
      LOGICAL*2    FOUND
      CHARACTER*8  SOU, SSL
      LOGICAL*4,   EXTERNAL :: IS_R8_NAN, IS_R4_NAN
!
! 4.  HISTORY
!  WHO  WHEN        WHAT
!  pet  2003.08.28  Added sanity check in order to prevent abnormal termination 
!                   when SARFxx is corrupted
!  pet  2024.07.09  Added support for computation of scan statistics and weighted epoch
!                   for a given source
!
! 5.  SOURC PROGRAM STRUCTURE
!
! --- Set up information arrays
!
      DO I = 1, 4
         SOURCE_NAME(I) = INFO(I)
      END DO
      CALL MEMCPY ( JNAME, INFO(21) )
!
      DO I = 1, 16
         NABF(I) = INFO(I+4)
      END DO
!
! --- Process the baseline name
!
      IF ( ISNDX .EQ. 0 ) THEN ! Load first source name into array!
           DO J = 1, 4
              SOURCES(J,1) = SOURCE_NAME(J)
           END DO
           JNAMES(1) = JNAME
           CALL MEMCPY ( SOURCES(27,1), JNAME )
           IF ( IS_R4_NAN ( RABF(4) ) ) RABF(4) = 0.8*EPS_MAX
           IF ( IS_R4_NAN ( RABF(5) ) ) RABF(5) = 0.8*EPS_MAX
           IF ( IS_R4_NAN ( RABF(6) ) ) RABF(6) = 0.8*EPS_MAX
           IF ( IS_R4_NAN ( RABF(7) ) ) RABF(7) = 0.8*EPS_MAX
           OBSU_SUM(1) = DBLE(NABF(1))
           OBSU_SUM(2) = DBLE(NABF(2))
           OBSU_SUM(3) = DBLE(NABF(15))
           WRMS_SUM(1) = (RABF(5)**2)*NABF(1)
           WRMS_SUM(2) = (RABF(7)**2)*NABF(15)
           IF ( ABS(RABF(4)) .GE. EPS_MIN .AND. ABS(RABF(4)) < 0.5*EPS_MAX &
     &                                    .AND. ABS(RABF(5)) < 0.5*EPS_MAX ) THEN
                FACT_SUM(1) = (RABF(5)**2)*NABF(1)/(RABF(4)**2)
             ELSE
                FACT_SUM(1) = 0.D0
                WRMS_SUM(1) = 0.0D0
           ENDIF
           IF ( ABS(RABF(6)) .GE. EPS_MIN .AND. ABS(RABF(6)) < 0.5*EPS_MAX &
     &                                    .AND. ABS(RABF(7)) < 0.5*EPS_MAX ) THEN
                FACT_SUM(2) = (RABF(7)**2)*NABF(15)/(RABF(6)**2)
             ELSE
                FACT_SUM(2) = 0.D0
                WRMS_SUM(2) = 0.0D0
           ENDIF
           DO J = 1, 22
              SOURCES(4+J,1) = NTMP(J)
           END DO
           SOURCES(36:47,1) = INFO(26:37)
           ISNDX = 1
         ELSE IF ( ISNDX .GT. 0 ) THEN
           I = 0
           FOUND = .FALSE.
           DO WHILE ( .NOT. FOUND )
              I = I + 1
              IF ( I .GT. ISNDX ) THEN
                   DO J = 1, 4
                      SOURCES(J, I) = SOURCE_NAME(J)
                   END DO
                   JNAMES(I) = JNAME
                   CALL MEMCPY ( SOURCES(27,I), JNAME )
                   OBSU_SUM(1) = DBLE(NABF(1))
                   OBSU_SUM(2) = DBLE(NABF(2))
                   OBSU_SUM(3) = DBLE(NABF(15))
                   IF ( IS_R4_NAN ( RABF(4) ) ) RABF(4) = 0.8*EPS_MAX
                   IF ( IS_R4_NAN ( RABF(5) ) ) RABF(5) = 0.8*EPS_MAX
                   IF ( IS_R4_NAN ( RABF(6) ) ) RABF(6) = 0.8*EPS_MAX
                   IF ( IS_R4_NAN ( RABF(7) ) ) RABF(7) = 0.8*EPS_MAX
!
                   IF ( RABF(4) .LT. EPS_MAX .AND. &
     &                  RABF(5) .LT. EPS_MAX .AND. &
     &                  RABF(6) .LT. EPS_MAX .AND. &
     &                  RABF(7) .LT. EPS_MAX       ) THEN
!
                        WRMS_SUM(1) = (RABF(5)**2)*NABF(1)
                        WRMS_SUM(2) = (RABF(7)**2)*NABF(15)
                        IF ( ABS(RABF(4)) .GE. EPS_MIN .AND. ABS(RABF(4)) .LT. 0.5*EPS_MAX &
     &                                                 .AND. ABS(RABF(5)) .LT. 0.5*EPS_MAX ) THEN
                             FACT_SUM(1) = (RABF(5)**2)*NABF(1)/(RABF(4)**2)
                           ELSE
                             FACT_SUM(1) = 0.D0
                             WRMS_SUM(1) = 0.0D0
                        ENDIF
                        IF ( ABS(RABF(6)) .GE. EPS_MIN .AND. ABS(RABF(6)) .LT. 0.5*EPS_MAX &
     &                                                 .AND. ABS(RABF(7)) .LT. 0.5*EPS_MAX ) THEN
                             FACT_SUM(2) = (RABF(7)**2)*NABF(15)/(RABF(6)**2)
                          ELSE
                             FACT_SUM(2) = 0.D0
                             WRMS_SUM(2) = 0.0D0
                        ENDIF
                        DO J=1,22
                           SOURCES(4+J, I) = NTMP(J)
                        END DO
                        SOURCES(36:47,I) = INFO(26:37)
                      ELSE
                        WRITE (  6, '(A,I9,A,4A2/A,7(E15.7,1X)/A,1X,16(I7,1X) )' ) &
     &                        'WARNING: 1. sourc: Corrupted SARFIL record ', &
     &                         I, ' Source: ', SOURCE_NAME, &
     &                        'RABF: ', RABF, ' NABF: ', NABF
                        WRITE ( 23, '(A,I9,A,4A2/A,7(E15.7,1X)/A,1X,16(I7,1X) )' ) &
     &                        'WARNING: 1. sourc: Corrupted SARFIL record ', &
     &                         I, ' Source: ', SOURCE_NAME, &
     &                        'RABF: ', RABF, ' NABF: ', NABF
                   END IF
                   FOUND = .TRUE.
                   ISNDX = I
                 ELSE
                   DO J=1,4
                      SOURCE(J) = SOURCES(J,I)
                   END DO
                   IF ( SOU .EQ. SSL ) THEN
                        DO J=1,22
                           NTMP(J) = SOURCES(4+J,I)
                        END DO
                        IF ( IS_R4_NAN ( RABF(4) ) ) RABF(4) = 0.8*EPS_MAX
                        IF ( IS_R4_NAN ( RABF(5) ) ) RABF(5) = 0.8*EPS_MAX
                        IF ( IS_R4_NAN ( RABF(6) ) ) RABF(6) = 0.8*EPS_MAX
                        IF ( IS_R4_NAN ( RABF(7) ) ) RABF(7) = 0.8*EPS_MAX
!
                        IF ( RABF(4) .LT. EPS_MAX .AND. &
     &                       RABF(5) .LT. EPS_MAX .AND. &
     &                       RABF(6) .LT. EPS_MAX .AND. &
     &                       RABF(7) .LT. EPS_MAX       ) THEN
!
                             OBSU_SUM(1) = OBSU_SUM(1) + DBLE(NABF(1))
                             OBSU_SUM(2) = OBSU_SUM(2) + DBLE(NABF(2))
                             OBSU_SUM(3) = OBSU_SUM(3) + DBLE(NABF(15))
                             WRMS_SUM(1) = WRMS_SUM(1) + (RABF(5)**2)*NABF(1)
                             WRMS_SUM(2) = WRMS_SUM(2) + (RABF(7)**2)*NABF(15)
                             IF ( ABS(RABF(4)) .GE. EPS_MIN .AND. ABS(RABF(4)) .LT. 0.5*EPS_MAX &
     &                                                      .AND. ABS(RABF(5)) .LT. 0.5*EPS_MAX ) THEN
                                  FACT_SUM(1) = FACT_SUM(1) + &
     &                                        (RABF(5)**2)*NABF(1)/(RABF(4)**2)
                                ELSE 
                                  FACT_SUM(1) = 0.0D0
                                  WRMS_SUM(1) = 0.0D0
                             END IF
                             IF ( ABS(RABF(6)) .GE. EPS_MIN .AND. ABS(RABF(6)) .LT. 0.5*EPS_MAX &
     &                                                      .AND. ABS(RABF(7)) .LT. 0.5*EPS_MAX ) THEN
                                  FACT_SUM(2) = FACT_SUM(2) + &
     &                                         (RABF(7)**2)*NABF(15)/(RABF(6)**2)
                                ELSE 
                                  FACT_SUM(2) = 0.0D0
                                  WRMS_SUM(2) = 0.0D0
                             END IF
                             DO J=1,22
                                SOURCES(J+4,I) = NTMP(J)
                             END DO
!
! -------------------------- Update of scan counters
!
                             CALL MEMCPY ( NSCA_TOT,  INFO(26), %VAL(4) )
                             CALL MEMCPY ( NSCA_USED, INFO(28), %VAL(4) )
!
                             CALL MEMCPY ( NSCA_TOT_SUM,  SOURCES(36,I), %VAL(4) )
                             CALL MEMCPY ( NSCA_USED_SUM, SOURCES(38,I), %VAL(4) )
!
                             NSCA_TOT_SUM  = NSCA_TOT_SUM  + NSCA_TOT
                             NSCA_USED_SUM = NSCA_USED_SUM + NSCA_USED
!
                             CALL MEMCPY ( SOURCES(36,I), NSCA_TOT_SUM,  %VAL(4) )
                             CALL MEMCPY ( SOURCES(38,I), NSCA_USED_SUM, %VAL(4) )
!
! -------------------------- Update of weighed epoch counters
!
                             CALL MEMCPY ( WEI_EPO,  INFO(30), %VAL(8) )
                             CALL MEMCPY ( WEI_SUM,  INFO(34), %VAL(8) )
!
                             CALL MEMCPY ( WEI_EPO_ACC, SOURCES(40,I), %VAL(8) )
                             CALL MEMCPY ( WEI_SUM_ACC, SOURCES(44,I), %VAL(8) )
!
                             WEI_EPO_ACC = WEI_EPO_ACC + WEI_EPO
                             WEI_SUM_ACC = WEI_SUM_ACC + WEI_SUM
!
                             CALL MEMCPY ( SOURCES(40,I), WEI_EPO_ACC,  %VAL(8) )
                             CALL MEMCPY ( SOURCES(44,I), WEI_SUM_ACC,  %VAL(8) )
!
                           ELSE
                             WRITE (  6, '(A,I9,A,4A2/A,7(E15.7,1X)/A,1X,16(I7,1X) )' ) &
     &                         'WARNING: 2. sourc: Corrupted SARFIL record ', &
     &                          I, ' Source: ', SOURCE_NAME, &
     &                         'RABF: ', RABF, ' NABF: ', NABF
                             WRITE ( 23, '(A,I9,A,4A2/A,7(E15.7,1X)/A,1X,16(I7,1X) )' ) &
     &                         'WARNING: 2. sourc: Corrupted SARFIL record ', &
     &                          I, ' Source: ', SOURCE_NAME, &
     &                         'RABF: ', RABF, ' NABF: ', NABF
                        END IF
                        FOUND = .TRUE.
                    END IF
              END IF
          END DO
      END IF
!
      RETURN
      END  !#!  SOURC  #!#
