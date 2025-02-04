      FUNCTION REPPTSU ( DIAGI_S, M, GMB, T, X, E, &
     &                   TG, XG, EG, &
     &                   TM, XM, EM, &
     &                   INFO_CHR_G, INFO_CHR_M, &
     &                   REC_OBS, REC_OBS_G, REC_OBS_M, &
     &                   FILE_NAME_1, FILE_NAME_2, &
     &                   FUNC_N, FUNC_B, FUNC_KEY, IUER )
! ************************************************************************
! *                                                                      *
! * function REPPTSU suppresses/recovers single observations (points)    *
! *                                                                      *
! *  called subroutines:                                                 *
! *  ERR_PASS, REPEXPA, ERR_LOG, REPCCLO, LIB$MOVC3, DIAGI_DRAW,         *
! *  REPINSE, REPDELE, PGSCI, DIAGI_PURGE_BOT, REPBOTT                   *
! *  REPRERD, REPOBRD, SUPR_OBS, REPREWT, REPOBWT, REPSTAT               *
! *                                                                      *
! *  calling routines:                                                   *
! *  DIAGI (via REPA and MULTI_DIAGI)                                    *
! *                                                                      *
! *  terms: G-good point, M-manually suppressed, B-bad point             *
! *                                                                      *
! *  2002-08-28               REPPTSU                  Volkmar Thorandt  *
! *  2002-09-09 VT - added info array                                    *
! *  2002-12-05 VT - added record array                                  *
! *  2002-12-11 GE - added rewrite of RESFxx and OBSFxx                  *
! *  2002-12-20 VT - REPHEAD call                                        *
! *  2003-01-13 VT - added record arrays REC_OBS_G, REC_OBS_M            *
! *  2003-01-16 VT - added REPSTAT (reset statistics)                    *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
!
      INTEGER*4  REPPTSU
!
      INCLUDE    'diagi.i'                    ! DIAGI      - include
      INCLUDE    'solve.i'                    ! CALC/SOLVE - include
      INCLUDE    'oborg.i'                    ! OBSFxx     - include
      INCLUDE    'obors.i'                    ! OBSFxx     - include
      INCLUDE    'resfl.i'                    ! RESFxx     - include
      INCLUDE    'socom.i'                    ! COMMxx     - include
!
      TYPE ( DIAGI_STRU ) ::  DIAGI_S    ! record of DIAGI structure (see diagi.i)
      INTEGER*4  M                            ! total # of points (G+M+B)
      REAL*8     T(M), X(M), E(M)             ! extended lists ( total sets of points)
      REAL*8     TG(M), XG(M), EG(M)          ! time, value, error (G)
      REAL*8     TM(M), XM(M), EM(M)          ! time, value, error (M)
!
      CHARACTER  FILE_NAME_1*100              ! file name
      CHARACTER  FILE_NAME_2*100              ! file name
!
      INTEGER*4  FUNC_N                       ! current # of function keys
      CHARACTER  FUNC_KEY(FUNC_N)*1           ! current keybord keys of user functions (s. repa.i)
      CHARACTER  FUNC_B(FUNC_N)*8             ! current button names of user functions (s. repa.i)
      CHARACTER  NEW_BUTT*8                   ! chosen button
!
      CHARACTER  INFO_LINE*87                 ! info line for found point
      CHARACTER  INFO_CHR_G(M)*87             ! information for bottom line (good)
      CHARACTER  INFO_CHR_M(M)*87             ! information for bottom line (recoverable)
!
      INTEGER*2  REC_OBSF( JOBSREC_WORDS )    ! OBSFxx record (JOBSREC_WORDS s. solve.i)
      EQUIVALENCE ( REC_OBSF, FJD )
      LOGICAL*4  RECV_OBS                     ! Function recover obs.
      LOGICAL*4  DO_RECV_OBS                  ! Function value
      CHARACTER  GMB(M)*1                     ! flag for good(G), man.down(M) or bad(B) observation
      INTEGER*4  REC_OBS(M)                   ! record #s of baseline observations
      INTEGER*4  REC_OBS_G(M)                 ! record #s of "good" baseline observations
      INTEGER*4  REC_OBS_M(M)                 ! record #s of "recoverable" baseline observations
      INTEGER*4  IPQ                          ! REPCCLO return index of found point in field ICLR (color)
      INTEGER*4  IUER                         ! universal error handler
      REAL*4     XC_ARG, YC_VAL, EC_SIG       ! copy of point coordinates
      REAL*8     XC_ARG8, YC_VAL8, EC_SIG8    ! copy of point coordinates
!
      REAL*4     ARG_DIST,  DIST_X, DIST_Y    ! distance variables
      REAL*4     DIST_MM                      ! max. distance point-curser (mm)
      PARAMETER  ( DIST_MM  = 10.0 )          ! If there is no point closer to the
!                                             ! the current cursor position than
!                                             ! DIST_MM mm, then SHIFT_POINT will
!                                             ! do nothing.
      INTEGER*4  ICLR
      INTEGER*4  ILST_SAVED_1                 ! saved the line style
      INTEGER*4  ILST_SAVED_2                 ! saved the line style
      INTEGER*4  IPM                          ! index in extended lists
      INTEGER*4  IER                          ! error handler
      CHARACTER  MESS_BOT_SAV*128             ! copy of DIAGI_S.MESS_BOT
      CHARACTER  STRING*128                   ! REPBOTT parameter (empty)
      INTEGER*4  J1                           ! loop variable
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
      ICLR = 1
      IPQ = 0
      MESS_BOT_SAV = DIAGI_S%MESS_BOT
!
! --- Set maximal distance: DIST_MM mm for each coordinates. If the point is
! --- located at more than sqrt(2)*10.0 mm, then it will be ignored.
!
      DIST_X = 10.0/(XRIGHTS(1)-XLEFTS(1))*(DIAGI_S%XMAX - DIAGI_S%XMIN)
      DIST_Y = 10.0/(YTOPS(1)-YBOTS(1))*(DIAGI_S%YMAX - DIAGI_S%YMIN)
!
! --- Search for the nearest point within sqrt(dist_2** + dist_y**2) area.
! --- Points beyond the plotting area are ignored
!
      CALL REPCCLO ( &
     &     DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), %VAL(DIAGI_S%ADR_Y4(1)), &
     &     DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), %VAL(DIAGI_S%ADR_Y4(2)), &
     &     DIAGI_S%NPOI(3), %VAL(DIAGI_S%ADR_X4(3)), %VAL(DIAGI_S%ADR_Y4(3)), &
     &     DIAGI_S%XMIN, DIAGI_S%XMAX, DIAGI_S%YMIN, DIAGI_S%YMAX, &
     &     DIAGI_S%XC, DIAGI_S%YC, DIST_X, DIST_Y, ICLR, IPQ )
!
      IF ( IPQ .GT. 0 .AND. ICLR .LT. 3) THEN
!
! -------- the point was found -> copy its coordinates
!
           CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_X4(ICLR)+(IPQ-1)*4), XC_ARG )
           CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_Y4(ICLR)+(IPQ-1)*4), YC_VAL )
           CALL LIB$MOVC3 ( 4, %VAL(DIAGI_S%ADR_E4(ICLR)+(IPQ-1)*4), EC_SIG)
!
! -------- save styles
!
           ILST_SAVED_1 = DIAGI_S%ILST(1)  ! Save the line style
           ILST_SAVED_2 = DIAGI_S%ILST(2)  ! Save the line style
           DIAGI_S%ILST(1) = 1             ! set temporarily point-by-point style
!
           IF ( ICLR .EQ. 1 ) THEN         ! good point
!
                XC_ARG8 = TG(IPQ)
                YC_VAL8 = XG(IPQ)
                EC_SIG8 = EG(IPQ)
!
! ------------- Redraw the point with opposite color
!
                CALL DIAGI_DRAW ( DIAGI_S, 2, 0, 1, XC_ARG, YC_VAL, EC_SIG, &
     &                            0.0D0, 0.0D0 )
!
! ------------- expand array of suppressed points (array 2)
!
                CALL ERR_PASS ( IUER, IER )
                CALL REPEXPA ( DIAGI_S%NPOI(2)+1, DIAGI_S%NPOI(2), &
     &                           DIAGI_S%ADR_X4(2), DIAGI_S%ADR_Y4(2), &
     &                           DIAGI_S%ADR_E4(2), IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7511, IUER, 'SUPP_TOGGLE', 'Error in an '// &
     &                   'attempt to grab more memory for internal arrays' )
                     RETURN
                END IF
!
! ------------- insert the info line and values to the arrays of recoverable points
!
                INFO_LINE = INFO_CHR_G(IPQ)(1:85)//'MD'
                INFO_LINE(7:9) = '  1'
!
                CALL REPINSE ( DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), &
     &                         %VAL(DIAGI_S%ADR_Y4(2)), %VAL(DIAGI_S%ADR_E4(2)), &
     &                         INFO_CHR_M, XC_ARG, YC_VAL, EC_SIG, INFO_LINE, &
     &                         TM, XM, EM, XC_ARG8, YC_VAL8, EC_SIG8, REC_OBS_M, REC_OBS_G(IPQ) )
!
! ------------- remove the point from the arrays of good points
!
                CALL REPDELE ( IPQ, DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), &
     &                         %VAL(DIAGI_S%ADR_Y4(1)), %VAL(DIAGI_S%ADR_E4(1)), INFO_CHR_G, &
     &                         TG, XG, EG, REC_OBS_G )
           END IF
!
           IF ( ICLR .EQ. 2 ) THEN
!
                XC_ARG8 = TM(IPQ)
                YC_VAL8 = XM(IPQ)
                EC_SIG8 = EM(IPQ)
!
! ------------- re-draw the point with opposite color
!
                CALL DIAGI_DRAW ( DIAGI_S, 1, 0, &
     &                            1, XC_ARG, YC_VAL, EC_SIG, 0.0D0, 0.0D0 )
!
! ------------- expand array of good points (array 1)
!
                CALL ERR_PASS ( IUER, IER )
                CALL REPEXPA ( DIAGI_S%NPOI(1)+1, DIAGI_S%NPOI(1), &
     &                           DIAGI_S%ADR_X4(1), DIAGI_S%ADR_Y4(1), &
     &                           DIAGI_S%ADR_E4(1), IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7512, IUER, 'SUPP_TOGGLE', 'Error in an '// &
     &                   'attempt to grab more memory for internal arrays' )
                     RETURN
                END IF
!
! ------------- insert the point to the array of good points
!
                INFO_LINE = INFO_CHR_M(IPQ)(1:85)//'GP'
                INFO_LINE(7:9) = '  0'
                CALL REPINSE ( DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), &
     &                         %VAL(DIAGI_S%ADR_Y4(1)), %VAL(DIAGI_S%ADR_E4(1)), &
     &                         INFO_CHR_G, XC_ARG, YC_VAL, EC_SIG, INFO_LINE, &
     &                         TG, XG, EG, XC_ARG8, YC_VAL8, EC_SIG8, REC_OBS_G, REC_OBS_M(IPQ)  )
!
! ------------- remove the point from the arrays of recoverable points
!
                CALL REPDELE ( IPQ, DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), &
     &                         %VAL(DIAGI_S%ADR_Y4(2)), %VAL(DIAGI_S%ADR_E4(2)), INFO_CHR_M, &
     &                         TM, XM, EM, REC_OBS_M )
           END IF
!
           DIAGI_S%ILST(1) = ILST_SAVED_1  ! Restore original
           DIAGI_S%ILST(2) = ILST_SAVED_2  ! line style
!
! -------- Now we should find  it  in the extended list in order to report
! -------- a user about the work to have been done
!
           ARG_DIST = (DIAGI_S%XMAX - DIAGI_S%XMIN)
           DO J1=1,M
              IF ( ABS ( T(J1) - XC_ARG ) .LT. ARG_DIST ) THEN
                   ARG_DIST = ABS ( T(J1) - XC_ARG )
                   IPM = J1
              END IF
           END DO
!
!--------- write RESFxx and OBSFxx records for changed observations
!
           IF ( GMB(IPM) .EQ. 'G' ) THEN
              CALL REPRERD ( FILE_NAME_1, REC_OBS(IPM) )
              CALL REPOBRD ( FILE_NAME_2, REC_OBS(IPM), REC_OBSF, JOBSREC_WORDS )
!             write(6,*) 'REPPTSU: IDATYP= ',IDATYP
              SUPSTAT(1) = SUPSTAT_RES(1)
              SUPSTAT(2) = SUPSTAT_RES(2)
              UACSUP = UACSUP_RES
              CALL SUPR_OBS ( IDATYP, SUPSTAT, UACSUP )
              SUPSTAT_RES(1) = SUPSTAT(1)
              SUPSTAT_RES(2) = SUPSTAT(2)
              UACSUP_RES = UACSUP
              CALL REPREWT ( FILE_NAME_1, REC_OBS(IPM) )
              CALL REPOBWT ( FILE_NAME_2, REC_OBS(IPM), REC_OBSF, JOBSREC_WORDS )
              GMB(IPM) = 'M'
           ELSE IF ( GMB(IPM) .EQ. 'M' ) THEN
              CALL REPRERD ( FILE_NAME_1, REC_OBS(IPM) )
              CALL REPOBRD ( FILE_NAME_2, REC_OBS(IPM), REC_OBSF, JOBSREC_WORDS )
              SUPSTAT(1) = SUPSTAT_RES(1)
              SUPSTAT(2) = SUPSTAT_RES(2)
              UACSUP = UACSUP_RES
!             write(6,*) 'REPPTSU: IDATYP= ',IDATYP
              DO_RECV_OBS= RECV_OBS ( IDATYP, SUPSTAT, UACSUP )
!             write(6,*) 'REPPTSU: DO_RECV_OBS= ',DO_RECV_OBS
              SUPSTAT_RES(1) = SUPSTAT(1)
              SUPSTAT_RES(2) = SUPSTAT(2)
              UACSUP_RES = UACSUP
              CALL REPREWT ( FILE_NAME_1, REC_OBS(IPM) )
              CALL REPOBWT ( FILE_NAME_2, REC_OBS(IPM), REC_OBSF, JOBSREC_WORDS )
              GMB(IPM) = 'G'
           END IF
           DIAGI_S%MESS_BOT = INFO_LINE
!
!C         write(6,*) 'REPPTSU: REC_OBS(',IPM,')=',REC_OBS(IPM)
!
! --- no recoverable point was found
      ELSE IF ( IPQ .GT. 0 .AND. ICLR .EQ. 3 ) THEN
         DIAGI_S%MESS_BOT = 'Observation is not recoverable!'
      ELSE
         DIAGI_S%MESS_BOT = 'Please move the cursor closer to '// &
     &                      'the point to suppress or to recover it!'
!
      END IF
!
! --- recalculates the statistics and redraws the headline of the plot
!
      CALL REPSTAT ( DIAGI_S, XG, EG )
!
! --- redraw the button headline
!
      CALL REPHEAD ( DIAGI_S, FUNC_N, FUNC_B, FUNC_KEY, NEW_BUTT )
!
! --- print new bottom message
!
      CALL PGSCI  ( 1 )
!
! --- purge the current bottom message
      CALL DIAGI_PURGE_BOT ( DIAGI_S )
      CALL CLRCH ( STRING )
      CALL REPBOTT ( DIAGI_S, STRING, 7 )         ! bottom message
      DIAGI_S%MESS_BOT = MESS_BOT_SAV
!
410   CONTINUE
      REPPTSU = 1 ! Important: DiaGI user function should always return 1.
!                         ! If the function returns not 1, then DiaGI will print
!                         ! the error message and return to the main program!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  REPPTSU  #!#
