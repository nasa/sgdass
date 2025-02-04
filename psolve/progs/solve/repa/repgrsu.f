      FUNCTION REPGRSU ( SUPKEY, DIAGI_S, M, GMB, T, Y, &
     &                   TG, XG, EG, &
     &                   TM, XM, EM, &
     &                   INFO_CHR_G, INFO_CHR_M, &
     &                   REC_OBS, REC_OBS_G, REC_OBS_M, &
     &                   FILE_NAME_1, FILE_NAME_2, &
     &                   FUNC_N, FUNC_B, FUNC_KEY, IUER )
!
! ************************************************************************
! *                                                                      *
! *  function REPGRSU deletes groups of points outside the               *
! *  cursor position in a DiaGi plot                                     *
! *  (G-good points, M-suppressed points, B-bad points                   *
! *                                                                      *
! *  called subroutines:                                                 *
! *  ERR_PASS, REPEXPA, ERR_LOG, ERR_PASS, REPCOUT, LIB$MOVC3,           *
! *  DIAGI_DRAW, REPGRRD, REPSTAT                                        *
! *                                                                      *
! *  calling routines:                                                   *
! *  DIAGI (via REPA and MULTI_DIAGI)                                    *
! *                                                                      *
! *  ### 10-SEP-2002             REPGRSU    V.Thorandt  10-SEP-2002 ###  *
! *  02-12-05 VT - array for record #s added                             *
! *  02-12-12 VT - filename parameters added                             *
! *  02-12-17 VT - real*8 arrays added, array expand mode changed        *
! *  02-12-20 VT - REPHEAD call                                          *
! *  03-01-17 VT - added REPSTAT (reset statistics)                      *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
!
      INTEGER*4  REPGRSU
!
      INCLUDE    'diagi.i'                   ! DiaGi include
!
      CHARACTER  FILE_NAME_1*100             ! file name variable
      CHARACTER  FILE_NAME_2*100             ! file name variable
!
      INTEGER*4  FUNC_N                      ! current # of function keys
      CHARACTER  FUNC_KEY(FUNC_N)*1          ! current keybord keys of user functions (s. repa.i)
      CHARACTER  FUNC_B(FUNC_N)*8            ! current button names of user functions (s. repa.i)
      CHARACTER  NEW_BUTT*8                  ! chosen button
!
      CHARACTER  SUPKEY*1                    ! suppression key
      TYPE ( DIAGI_STRU ) ::  DIAGI_S   ! record of DiaGi structure
      INTEGER*4  M                           ! total # of points (G+M+B)
      REAL*8     TG(M), XG(M), EG(M)         ! time, value, error (G)
      REAL*8     TM(M), XM(M), EM(M)         ! time, value, error (M)
      CHARACTER  GMB(M)*1                    ! flag for good(G), man.down(M) or bad(B) observation
      INTEGER*4  REC_OBS(M)                  ! record #s of baseline observations
      INTEGER*4  REC_OBS_G(M)                ! record #s of "good" baseline observations
      INTEGER*4  REC_OBS_M(M)                ! record #s of "recoverable" baseline observations
      REAL*8     T(M)                        ! extended list (arguments)
      REAL*8     Y(M)                        ! extended list (values)
      CHARACTER  INFO_CHR_G(M)*87            ! information for bottom line (good)
      CHARACTER  INFO_CHR_M(M)*87            ! information for bottom line (man.down)
      INTEGER*4  NIPQ                        ! # of indices in IPQ(M)
      INTEGER*4  IPQ(M)                      ! array of point indices (G-->M or M-->G) in array 1
      INTEGER*4  ILST_SAVED_1, ILST_SAVED_2  ! temporary line styles
      REAL*4     XC_ARG, YC_VAL, EC_SIG      ! temporary values
      INTEGER*4  IUER                        ! universal error handler
      INTEGER*4  IER                         ! error handler
      INTEGER*4  J1                          ! loop variable
!**********************************************************************************************
!
! --- purge bottom line
!
      CALL DIAGI_PURGE_BOT ( DIAGI_S )
!
! --- return if curser not in plotting area
!
      IF ( DIAGI_S%XC .LT. DIAGI_S%XMIN .OR. DIAGI_S%XC .GT. DIAGI_S%XMAX .OR. &
     &     DIAGI_S%YC .LT. DIAGI_S%YMIN .OR. DIAGI_S%YC .GT. DIAGI_S%YMAX ) GOTO 410
!
! --- search for points with |Y| > |YC| in G-array (# NIPQ1)
!
      NIPQ = 0
      IF ( SUPKEY .EQ. '0' ) THEN                       ! suppress points (G-->M)
!
! ------ find points which are "outside" curser area and write changed records
!
         CALL REPCOUT ( SUPKEY, M, NIPQ, IPQ, DIAGI_S%YC, DIAGI_S%NPOI(1), &
     &                  %VAL(DIAGI_S%ADR_Y4(1)), GMB, T, Y, REC_OBS, &
     &                  FILE_NAME_1, FILE_NAME_2 )
!
! ------ points in array 1 (G) found
!C       write(6,*) 'REPGRSU: NIPQ=',NIPQ
!
         IF ( NIPQ .GT. 0 ) THEN
!
! ------------- expand array of suppressed points (array 2)
!
                CALL ERR_PASS ( IUER, IER )
                CALL REPEXPA ( DIAGI_S%NPOI(2)+NIPQ, DIAGI_S%NPOI(2), &
     &                           DIAGI_S%ADR_X4(2), DIAGI_S%ADR_Y4(2), &
     &                           DIAGI_S%ADR_E4(2), IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7511, IUER, 'SUPP_TOGGLE', 'Error in an '// &
     &                   'attempt to grab more memory for internal arrays' )
                     RETURN
                END IF
!
! --------- save styles
!
            ILST_SAVED_1 = DIAGI_S%ILST(1)    ! line style 1
            ILST_SAVED_2 = DIAGI_S%ILST(2)    ! line style 2
            DIAGI_S%ILST(1) = 1               ! set temporarily point-by-point style
!
! --------- loop over indicec in IPQ1 for operation G-->M
!
            DO J1 = 1, NIPQ
!
! ------------ copy coordinates of current point IPQ(J1)
!
               CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_X4(1)+(IPQ(J1)-1)*4), XC_ARG )
               CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_Y4(1)+(IPQ(J1)-1)*4), YC_VAL )
               CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_E4(1)+(IPQ(J1)-1)*4), EC_SIG)
!
! ------------ redraw the point with opposite color
!
               CALL DIAGI_DRAW ( DIAGI_S, 2, 0, 1, XC_ARG, YC_VAL, EC_SIG, 0.0D0, 0.0D0 )
!
            END DO
!
! --------- restore original line style
!
            DIAGI_S%ILST(1) = ILST_SAVED_1    ! line style 1
            DIAGI_S%ILST(2) = ILST_SAVED_2    ! line style 2
!
! --------- remove the points from the arrays of good points (G),
! --------- add them to the arrays of recoverable points (M)
!
            CALL REPGRRD ( SUPKEY, &
     &           DIAGI_S%NPOI(1), DIAGI_S%NPOI(2), DIAGI_S%YC, %VAL(DIAGI_S%ADR_X4(1)), &
     &           %VAL(DIAGI_S%ADR_Y4(1)), %VAL(DIAGI_S%ADR_E4(1)), INFO_CHR_G, &
     &           %VAL(DIAGI_S%ADR_X4(2)), %VAL(DIAGI_S%ADR_Y4(2)), &
     &           %VAL(DIAGI_S%ADR_E4(2)), INFO_CHR_M, &
     &           TG, XG, EG, TM, XM, EM, REC_OBS_G, REC_OBS_M )
!
         END IF
!
      ELSE IF ( SUPKEY .EQ. '1' ) THEN                       ! recover points (M-->G)
!
! ------ find points which are "inside" curser area and write changed records
!
         CALL REPCOUT ( SUPKEY, M, NIPQ, IPQ, DIAGI_S%YC, DIAGI_S%NPOI(2), &
     &                  %VAL(DIAGI_S%ADR_Y4(2)), GMB, T, Y, REC_OBS, &
     &                  FILE_NAME_1, FILE_NAME_2 )
!
! ------ points in array 1 (M) found
!C       write(6,*) 'REPGRSU: NIPQ=',NIPQ
!
         IF ( NIPQ .GT. 0 ) THEN
!
                CALL ERR_PASS ( IUER, IER )
                CALL REPEXPA ( DIAGI_S%NPOI(1)+NIPQ, DIAGI_S%NPOI(1), &
     &                           DIAGI_S%ADR_X4(1), DIAGI_S%ADR_Y4(1), &
     &                           DIAGI_S%ADR_E4(1), IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7512, IUER, 'SUPP_TOGGLE', 'Error in an '// &
     &                   'attempt to grab more memory for internal arrays' )
                     RETURN
                END IF
!
! --------- save styles
!
            ILST_SAVED_1 = DIAGI_S%ILST(1)    ! line style 1
            ILST_SAVED_2 = DIAGI_S%ILST(2)    ! line style 2
            DIAGI_S%ILST(1) = 1               ! set temporarily point-by-point style
!
! --------- loop over indicec in IPQ for operation G-->M
!
            DO J1 = 1, NIPQ
!
! ------------ copy coordinates of current point IPQ(J1)
!
               CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_X4(2)+(IPQ(J1)-1)*4), XC_ARG )
               CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_Y4(2)+(IPQ(J1)-1)*4), YC_VAL )
               CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_E4(2)+(IPQ(J1)-1)*4), EC_SIG)
!
! ------------ redraw the point with opposite color
!
               CALL DIAGI_DRAW ( DIAGI_S, 1, 0, 1, XC_ARG, YC_VAL, EC_SIG, 0.0D0, 0.0D0 )
!
            END DO
!
! --------- restore original line style
!
            DIAGI_S%ILST(1) = ILST_SAVED_1    ! line style 1
            DIAGI_S%ILST(2) = ILST_SAVED_2    ! line style 2
!
! --------- remove the points from the arrays of suppressed points (M),
! --------- add them to the arrays of goodpoints (G)
!
            CALL REPGRRD ( SUPKEY, &
     &           DIAGI_S%NPOI(1), DIAGI_S%NPOI(2), DIAGI_S%YC, %VAL(DIAGI_S%ADR_X4(1)), &
     &           %VAL(DIAGI_S%ADR_Y4(1)), %VAL(DIAGI_S%ADR_E4(1)), INFO_CHR_G, &
     &           %VAL(DIAGI_S%ADR_X4(2)), %VAL(DIAGI_S%ADR_Y4(2)), &
     &           %VAL(DIAGI_S%ADR_E4(2)), INFO_CHR_M, &
     &           TG, XG, EG, TM, XM, EM, REC_OBS_G, REC_OBS_M )
!
         END IF
!
      END IF
!
! --- recalculate statistic and redraw the headline of the plot
!
      CALL REPSTAT ( DIAGI_S, XG, EG )
!
! --- redraw the button headline
!
 410  CALL REPHEAD ( DIAGI_S, FUNC_N, FUNC_B, FUNC_KEY, NEW_BUTT )
!
! --- set DiaGi return code
! --- if not =1 --> DiaGI error message & return to calling routine
!
      REPGRSU = 1
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  REPGRSU  #!#
