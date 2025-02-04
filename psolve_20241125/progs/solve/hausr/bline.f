      SUBROUTINE BLINE ( INFO )
      IMPLICIT NONE
!
! 1.  BLINE PROGRAM SPECIFICATION
!
! 1.1 Setup information arrays and process baseline name
!
! 1.2 REFERENCES:
!
! 2.  BLINE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2  INFO(25)
!
! INFO - Information array for baseline names
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
!
      INTEGER*4    NRECORDS
      INTEGER*2    BASELINE_NAME(8), I, J, NABF(12), NTMP(22), BASELINE(8)
      INTEGER*4    OBSU_SUM(3)
      REAL*8       WRMS_SUM(2), FACT_SUM(2)
      REAL*4       RABF(6)
      REAL*4       EPS_MIN, EPS_MAX
      PARAMETER  ( EPS_MAX = 2.E16  )
      PARAMETER  ( EPS_MIN = 1.0/EPS_MAX )
      LOGICAL*2    FOUND
      CHARACTER*8  STA1, STA2, BSL1, BSL2
      EQUIVALENCE  (BASELINE_NAME(1), STA1), &
     &             (BASELINE_NAME(5), STA2), &
     &             (BASELINE(1), BSL1), &
     &             (BASELINE(5), BSL2), &
     &             (NTMP(1), OBSU_SUM(1)), &
     &             (NTMP(7), WRMS_SUM(1)), &
     &             (NTMP(15), FACT_SUM(1)), &
     &             (NABF(1), RABF(1))
      LOGICAL*4, EXTERNAL :: IS_R8_NAN, IS_R4_NAN
!
! 4.  HISTORY
!  WHO  WHEN        WHAT
!  pet  2003.08.28  Added sanity check in order to prevent abnormal termination 
!                   when SARFxx is corrupted
!
! 5.  BLINE PROGRAM STRUCTURE
!
!   set up information arrays
!
      DO I = 1, 8
         BASELINE_NAME(I) = INFO(I)
      END DO
!
      DO I = 1, 12
         NABF(I) = INFO(I+8)
      END DO
!
! --- Alphabetize the stations within baseline name
!
      IF ( STA1 .GT. STA2 ) THEN
           DO I = 1, 4  ! Switch leading stations
              BASELINE_NAME(I)   = INFO(I+4)
              BASELINE_NAME(I+4) = INFO(I)
           END DO
      END IF
!
! --- Process the baseline name
!
      IF ( IBNDX .EQ. 0 ) THEN ! Load first station name into array!
           DO J = 1, 8
              BASELINES(J,1) = BASELINE_NAME(J)
           END DO
           IF ( IS_R4_NAN ( RABF(1) ) ) RABF(1) = 0.8*EPS_MAX
           IF ( IS_R4_NAN ( RABF(2) ) ) RABF(2) = 0.8*EPS_MAX
           IF ( IS_R4_NAN ( RABF(3) ) ) RABF(3) = 0.8*EPS_MAX
           IF ( IS_R4_NAN ( RABF(4) ) ) RABF(4) = 0.8*EPS_MAX
           IF ( IS_R4_NAN ( RABF(5) ) ) RABF(5) = 0.8*EPS_MAX
!
           OBSU_SUM(1) = DBLE(NABF(1))
           OBSU_SUM(2) = DBLE(NABF(2))
           OBSU_SUM(3) = DBLE(NABF(11))
           WRMS_SUM(1) = (RABF(3)**2)*NABF(1)
           WRMS_SUM(2) = (RABF(5)**2)*NABF(11)
           IF ( ABS(RABF(2)) .GE. EPS_MIN  .AND.  ABS(RABF(2)) .LT. 0.5*EPS_MAX )THEN
                FACT_SUM(1) = (RABF(3)**2)*NABF(1)/(RABF(2)**2)
              ELSE
                FACT_SUM(1) = 0.0D0
                WRMS_SUM(1) = 0.0D0
           ENDIF
           IF ( ABS(RABF(4)) .GE. EPS_MIN .AND.  ABS(RABF(4)) .LT. 0.5*EPS_MAX ) THEN
                FACT_SUM(2) = (RABF(5)**2)*NABF(11)/(RABF(4)**2)
              ELSE
                FACT_SUM(2) = 0.0D0
                WRMS_SUM(2) = 0.0D0
           ENDIF
           DO J=1,22
              BASELINES(8+J,1) = NTMP(J)
           END DO
           IBNDX = 1
         ELSE IF ( IBNDX .GT. 0 ) THEN
           I = 0
           FOUND = .FALSE.
           DO WHILE ( .NOT. FOUND .AND. I .LT. MAX_ARC_BSL + 16 )
              I = I + 1
              IF ( I .GT. IBNDX ) THEN
                   DO J=1,8
                      BASELINES(J, I) = BASELINE_NAME(J)
                   END DO
!
! ---------------- Check sanity of values in SARFIL. Sometimes it can have 
! ---------------- a  garbage which would case abnormal termination
!
                   IF ( IS_R4_NAN ( RABF(1) ) ) RABF(1) = 0.8*EPS_MAX
                   IF ( IS_R4_NAN ( RABF(2) ) ) RABF(2) = 0.8*EPS_MAX
                   IF ( IS_R4_NAN ( RABF(3) ) ) RABF(3) = 0.8*EPS_MAX
                   IF ( IS_R4_NAN ( RABF(4) ) ) RABF(4) = 0.8*EPS_MAX
                   IF ( IS_R4_NAN ( RABF(5) ) ) RABF(5) = 0.8*EPS_MAX
                   IF ( ABS(RABF(1)) .LT. EPS_MAX  .AND.  &
     &                  ABS(RABF(2)) .LT. EPS_MAX  .AND.  & 
     &                  ABS(RABF(3)) .LT. EPS_MAX  .AND.  &
     &                  ABS(RABF(4)) .LT. EPS_MAX  .AND.  &
     &                  ABS(RABF(5)) .LT. EPS_MAX         ) THEN
!
                        OBSU_SUM(1) = DBLE(NABF(1))
                        OBSU_SUM(2) = DBLE(NABF(2))
                        OBSU_SUM(3) = DBLE(NABF(11))
                        WRMS_SUM(1) = (RABF(3)**2)*NABF(1)
                        WRMS_SUM(2) = (RABF(5)**2)*NABF(11)
                        IF ( ABS(RABF(2)) .GE. EPS_MIN .AND. ABS(RABF(2)) .LT. 0.5*EPS_MAX ) THEN
                             FACT_SUM(1) = (RABF(3)**2)*NABF(1)/(RABF(2)**2)
                           ELSE
                             FACT_SUM(1) = 0.D0
                             WRMS_SUM(1) = 0.0D0
                        ENDIF
                        IF ( ABS(RABF(4)) .GE. EPS_MIN .AND. ABS(RABF(4)) .LT. 0.5*EPS_MAX ) THEN
                             FACT_SUM(2) = (RABF(5)**2)*NABF(11)/(RABF(4)**2)
                           ELSE
                             FACT_SUM(2) = 0.D0
                             WRMS_SUM(2) = 0.0D0
                        ENDIF
                        DO J = 1, 22
                           BASELINES(8+J,I) = NTMP(J)
                        END DO
                      ELSE 
                        WRITE (  6, '(A,I9,A,8A2/A,6(E15.7,1X)/A,1X,12(I7,1X) )' ) &
     &                        'WARNING: 1. bline: Corrupted SARFIL record ', &
     &                         I, ' baseline: ', BASELINE_NAME, &
     &                         'RABF: ', RABF, ' NABF: ', NABF
                        WRITE ( 23, '(A,I9,A,8A2/A,6(E15.7,1X)/A,1X,12(I7,1X) )' ) &
     &                        'WARNING: 1. bline: Corrupted SARFIL record ', &
     &                         I, ' baseline: ', BASELINE_NAME, &
     &                         'RABF: ', RABF, ' NABF: ', NABF
                   END IF
                   FOUND = .TRUE.
                   IBNDX = I
                 ELSE
                   DO J = 1, 8
                      BASELINE(J) = BASELINES(J, I)
                   END DO
                   IF ( (STA1 .EQ. BSL1 .AND. STA2 .EQ. BSL2 ) .OR. &
     &                  (STA2 .EQ. BSL1 .AND. STA1 .EQ. BSL2 )      ) THEN
                        DO J = 1, 22
                           NTMP(J) = BASELINES(8+J,I)
                        END DO
                        IF ( IS_R4_NAN ( RABF(1) ) ) RABF(1) = 0.8*EPS_MAX
                        IF ( IS_R4_NAN ( RABF(2) ) ) RABF(2) = 0.8*EPS_MAX
                        IF ( IS_R4_NAN ( RABF(3) ) ) RABF(3) = 0.8*EPS_MAX
                        IF ( IS_R4_NAN ( RABF(4) ) ) RABF(4) = 0.8*EPS_MAX
                        IF ( IS_R4_NAN ( RABF(5) ) ) RABF(5) = 0.8*EPS_MAX
!
! --------------------- Check sanity of values in SARFIL. Sometimes it can have 
! --------------------- a  garbage which would case abnormal termination
!
                        IF ( ABS(RABF(2)) .LT. EPS_MAX .AND. &
     &                       ABS(RABF(3)) .LT. EPS_MAX .AND. &
     &                       ABS(RABF(4)) .LT. EPS_MAX .AND. &
     &                       ABS(RABF(5)) .LT. EPS_MAX       ) THEN
!
                             OBSU_SUM(1) = OBSU_SUM(1) + DBLE(NABF(1))
                             OBSU_SUM(2) = OBSU_SUM(2) + DBLE(NABF(2))
                             OBSU_SUM(3) = OBSU_SUM(3) + DBLE(NABF(11))
                             WRMS_SUM(1) = WRMS_SUM(1) + (RABF(3)**2)*NABF(1)
                             WRMS_SUM(2) = WRMS_SUM(2) + (RABF(5)**2)*NABF(11)
                             IF ( ABS(RABF(2)) .GE. EPS_MIN .AND. ABS(RABF(2)) .LT. 0.5*EPS_MAX ) THEN
                                  FACT_SUM(1) = FACT_SUM(1) + &
     &                                        (RABF(3)**2)*NABF(1)/(RABF(2)**2)
                                ELSE
                                  FACT_SUM(1) = 0.0D0
                                  OBSU_SUM(1) = 0.0D0
                                  WRMS_SUM(1) = 0.0D0
                             END IF
                             IF ( ABS(RABF(4)) .GE. EPS_MIN .AND. ABS(RABF(4)) .LT. 0.5*EPS_MAX ) THEN
                                  FACT_SUM(2) = FACT_SUM(2) + &
     &                                        (RABF(5)**2)*NABF(11)/(RABF(4)**2)
                                ELSE
                                  FACT_SUM(2) = 0.0D0
                                  WRMS_SUM(2) = 0.0D0
                             END IF
                             DO J = 1, 22
                                BASELINES(J+8, I) = NTMP(J)
                             END DO
                           ELSE
                             WRITE (  6, '(A,I9,A,8A2/A,6(E15.7,1X)/A,1X,12(I7,1X) )' ) &
     &                          'WARNING: 2. bline: Corrupted SARFIL record ', &
     &                           I, ' baseline: ', BASELINE_NAME, &
     &                          'RABF: ', RABF, ' NABF: ', NABF
                             WRITE ( 23, '(A,I9,A,8A2/A,6(E15.7,1X)/A,1X,12(I7,1X) )' ) &
     &                          'WARNING: 2. bline: Corrupted SARFIL record ', &
     &                           I, ' baseline: ', BASELINE_NAME, &
     &                          'RABF: ', RABF, ' NABF: ', NABF
                        END IF
                        FOUND = .TRUE.
                   END IF
               END IF
 910           CONTINUE 
          END DO
      END IF
!
      RETURN
      END  SUBROUTINE  BLINE  !#!#
