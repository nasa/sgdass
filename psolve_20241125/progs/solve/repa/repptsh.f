      FUNCTION   REPPTSH ( DIAGI_S, M, T, Y, YG, EG, YM, YB, &
     &                     NUM_AMB, AMB_SP, IWAY, REC_OBS, &
     &                     FUNC_N, FUNC_B, FUNC_KEY, &
     &                     FILE_NAME_1, FILE_NAME_2, PS_NS, &
     &                     INFO_CHR_G, INFO_CHR_M, INFO_CHR_B, &
     &                     IDA_DTP, IUER )
!
! ************************************************************************
! *                                                                      *
! *  DiaGI user function REPPTSH shifts the point which                  *
! *  the cursor is close to at AMP_SP up (if IWAY=1) or                  *
! *  AMB_SP down (if IWAY=-1). Array NUM_AMB keeps tracks of shifting.   *
! *  If the K-th point is shfted UP then NUM_AMB(K) := NUM_AMB(K) + 1.   *
! *  IF it is shifted down then NUM_AMB(K) := NUM_AMB(K) - 1.            *
! *                                                                      *
! *  called subroutines:                                                 *
! *  REPHEAD, PGCOL_RGB, REPCCLO, LIB$MOVC3, DIAGI_DRAW, ERR_LOG,        *
! *  REPRERD, REPOBRD, REPREWT, REPOBWT, REPSTAT                         *
! *                                                                      *
! *  calling routines:                                                   *
! *  DIAGI (via REPA and MULTI_DIAGI)                                    *
! *                                                                      *
! *  TERMS: G - good, M - manually suppressed, B - bad                   *
! *                                                                      *
! *  02-08-28              REPPTSH                     Volkmar Thorandt  *
! *  02-12-05 VT - added record array                                    *
! *  02-12-17 VT - added real*8 arrays                                   *
! *  02-12-19 VT - REPHEAD call                                          *
! *  03-01-09 GE - added rewrite of RESFxx and OBSFxx                    *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
!
      INTEGER*4  REPPTSH
!
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'oborg.i'
      INCLUDE    'obors.i'
      INCLUDE    'resfl.i'
!
      CHARACTER  FILE_NAME_1*100             ! residual file name variable RESFxx
      CHARACTER  FILE_NAME_2*100             ! obs. file name variable OBSFxx
!
      CHARACTER  PS_NS*2                     ! unit (ps/ns)
      INTEGER*2  REC_OBSF( JOBSREC_WORDS )   ! OBSFxx record (JOBSREC_WORDS s. solve.i)
      EQUIVALENCE ( REC_OBSF, FJD )
!
      TYPE ( DIAGI_STRU ) ::  DIAGI_S   ! DIAGI_STRU record
      INTEGER*4  M                           ! total # of points (observations)
      INTEGER*4  IWAY                        ! default # of ambiguity steps
      INTEGER*4  FUNC_N                      ! current # of function keys
      CHARACTER  FUNC_KEY(FUNC_N)*1          ! current keybord keys of user functions (s. repa.i)
      CHARACTER  FUNC_B(FUNC_N)*8            ! current button names of user functions (s. repa.i)
      CHARACTER  NEW_BUTT*8                  ! chosen button
      CHARACTER  IDA_DTP*2                   ! datatype group flag ('GX', 'GS', 'PX', 'PS')
      INTEGER*4  IUER                        ! error handler
      REAL*8     T(M)                        ! arguments (all) (time)
      REAL*8     Y(M)                        ! values (all)
      REAL*8     YG(M), YM(M), YB(M)         ! values (G,M,B)
      REAL*8     EG(M)                       ! errors (G)
      REAL*8     AMB_SP                      ! ambiguity step
      REAL*4     AMB_4                       ! ambiguity step (real*4)
      INTEGER*4  NUM_AMB(M)                  ! array with # of amb. steps
      INTEGER*4  REC_OBS(M)                  ! record #s of baseline observations
      CHARACTER  INFO_CHR_G(M)*87            ! information for bottom line (good)
      CHARACTER  INFO_CHR_M(M)*87            ! information for bottom line (recoverable)
      CHARACTER  INFO_CHR_B(M)*87            ! information for bottom line (bad)
      INTEGER*4  I_TMP                       ! temporary value
      REAL*4     XC_ARG                      ! argument of current point
      REAL*4     YC_VAL                      ! value of current point
      REAL*4     EC_SIG                      ! error of current point
      REAL*4     YC_VAL_GR                   ! copy of YC_VAL for grey display
      REAL*4     DIST_X                      ! distance variable (X-component)
      REAL*4     DIST_Y                      ! distance variable (Y-component)
      REAL*4     ARG_DIST                    ! distance variable
      REAL*4     DIST_MM                     ! distance variable (in mm)
      INTEGER*4  J1                          ! loop variable
      INTEGER*4  IPM                         ! index of found point in extended list
      INTEGER*4  ICLR                        ! current colour
      INTEGER*4  IPQ                         ! index of fount point
      INTEGER*4  ILST_SAVED                  ! saved line style
      INTEGER*4  ICLR_GRY                    ! colour code (grey)
      PARAMETER  ( ICLR_GRY = 16 )
      PARAMETER  ( DIST_MM  = 10.0 )         ! If there is no point closer to the
!                                            ! the current cursor position than
!                                            ! DIST_MM (in mm), then REPPTSH
!                                            ! will do nothing.
! *******************************************************************************
!
! --- purge bottom line
!
      CALL DIAGI_PURGE_BOT ( DIAGI_S )
!
! --- return if curser not in plotting area
!
      IF ( DIAGI_S%XC < DIAGI_S%XMIN .OR. DIAGI_S%XC > DIAGI_S%XMAX .OR. &
     &     DIAGI_S%YC < DIAGI_S%YMIN .OR. DIAGI_S%YC > DIAGI_S%YMAX ) GOTO 420
!
! --- convert to real*4
!
      AMB_4 = AMB_SP
!
! --- Define light grey color. In fact DiaGI does not define all possible
! --- colors, but only colors for the functions which are used.
!
      CALL PGCOL_RGB ( ITAB_CLR(ICLR_GRY,1),   IRGB_DEF(ICLR_GRY,1,1), &
     &                 IRGB_DEF(ICLR_GRY,1,2), IRGB_DEF(ICLR_GRY,1,3)  )
      CALL PGCOL_RGB ( ITAB_CLR(ICLR_GRY,2),   IRGB_DEF(ICLR_GRY,1,1), &
     &                 IRGB_DEF(ICLR_GRY,1,2), IRGB_DEF(ICLR_GRY,1,3)  )
!
      DIST_X = 10.0/(XRIGHTS(1)-XLEFTS(1))*(DIAGI_S%XMAX - DIAGI_S%XMIN)
      DIST_Y = 10.0/(YTOPS(1)-YBOTS(1))*(DIAGI_S%YMAX - DIAGI_S%YMIN)
!
! --- Search for the point the closest to the current cursor position
! --- within the sqrt(dist_2** + dist_y**2) area
! --- Only the points within 10.0 mm at each coordintes will be taken into
! --- consideration
!
      CALL REPCCLO ( &
     &     DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), %VAL(DIAGI_S%ADR_Y4(1)), &
     &     DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), %VAL(DIAGI_S%ADR_Y4(2)), &
     &     DIAGI_S%NPOI(3), %VAL(DIAGI_S%ADR_X4(3)), %VAL(DIAGI_S%ADR_Y4(3)), &
     &     DIAGI_S%XMIN, DIAGI_S%XMAX, DIAGI_S%YMIN, DIAGI_S%YMAX, &
     &     DIAGI_S%XC, DIAGI_S%YC, DIST_X, DIST_Y, ICLR, IPQ )
      IF ( IPQ > 0 ) THEN
!
! -------- The point was found. Let's copy its coordinates
! -------- Found colour is ICLR
           CALL LIB$MOVC3( 4, %VAL(DIAGI_S%ADR_X4(ICLR)+(IPQ-1)*4), XC_ARG )
           CALL LIB$MOVC3( 4, %VAL(DIAGI_S%ADR_Y4(ICLR)+(IPQ-1)*4), YC_VAL )
           CALL LIB$MOVC3( 4, %VAL(DIAGI_S%ADR_E4(ICLR)+(IPQ-1)*4), EC_SIG )
!
! -------- Redraw the point
!
           ILST_SAVED = DIAGI_S%ILST(ICLR)
           DIAGI_S%ILST(1) = 1 ! enter point-by-point mode. It is done as
!                              ! a precaution. If the current line mode is
!                              ! "spline" than DIAGI may abnormally terminated
!
! -------- First draw it by background color (extinguish the current point)
!
           CALL DIAGI_DRAW ( DIAGI_S, ICLR, 0, 1, XC_ARG, YC_VAL, EC_SIG, &
     &                       0.0D0, 0.0D0 )
!
! -------- copy of value for grey display
!
           YC_VAL_GR = YC_VAL
!
! -------- draw the current point by light grey color --> now after REPSTAT
!
!C         CALL DIAGI_DRAW ( DIAGI_S, ICLR, ICLR_GRY, 1, XC_ARG, YC_VAL,
!C   #                       EC_SIG, 0.0D0, 0.0D0 )
!
! -------- Change the value of the point
!
!%%%
           IF ( PS_NS == 'ps' .OR. PS_NS == 'ns' ) THEN
              YC_VAL = YC_VAL + IWAY*AMB_4             ! group delay data types
           ELSE
              YC_VAL = YC_VAL + IWAY                   ! phase delay data types
           END IF
!%%%
!
! -------- ... change this value in the internal plotting array
!
           CALL LIB$MOVC3( 4, YC_VAL, %VAL(DIAGI_S%ADR_Y4(ICLR)+(IPQ-1)*4) )
!
! -------- change values in real*8 arrays and info arrays
!
!%%%
           IF ( ICLR == 1 ) THEN
              IF ( PS_NS == 'ps' .OR. PS_NS == 'ns' ) THEN
                 YG(IPQ) = YG(IPQ) + IWAY*AMB_SP             ! value gr.del.
              ELSE
                 YG(IPQ) = YG(IPQ) + IWAY                    ! value phase del.
              END IF
              I_TMP = IDNINT( YG(IPQ) )
              WRITE( INFO_CHR_G(IPQ)(51:58), '(I8)' ) I_TMP  ! info array (value)
           ELSE IF (ICLR == 2 ) THEN
              IF ( PS_NS == 'ps' .OR. PS_NS == 'ns' ) THEN
                 YM(IPQ) = YM(IPQ) + IWAY*AMB_SP             ! value gr.del.
              ELSE
                 YM(IPQ) = YM(IPQ) + IWAY                    ! value phase del.
              END IF
              I_TMP = IDNINT( YM(IPQ) )
              WRITE( INFO_CHR_M(IPQ)(51:58), '(I8)' ) I_TMP  ! info array (value)
           ELSE IF (ICLR == 3 ) THEN
              IF ( PS_NS == 'ps' .OR. PS_NS == 'ns' ) THEN
                 YB(IPQ) = YB(IPQ) + IWAY*AMB_SP             ! value gr.del.
              ELSE
                 YB(IPQ) = YB(IPQ) + IWAY                    ! value phase del.
              END IF
              I_TMP = IDNINT( YB(IPQ) )
              WRITE( INFO_CHR_B(IPQ)(51:58), '(I8)' ) I_TMP  ! info array (value)
           END IF
!%%%
!
! -------- recalculate statistics and redraw the headline of the plot
!
           CALL REPSTAT ( DIAGI_S, YG, EG )
!
! -------- draw the current point by light grey color at old place
!
           CALL DIAGI_DRAW ( DIAGI_S, ICLR, ICLR_GRY, 1, XC_ARG, YC_VAL_GR, &
     &                       EC_SIG, 0.0D0, 0.0D0 )
!
! -------- redraw the point with the current color
!
           CALL DIAGI_DRAW ( DIAGI_S, ICLR, ICLR, 1, XC_ARG, YC_VAL, EC_SIG, &
     &                       0.0D0, 0.0D0 )
           DIAGI_S%ILST(1) = ILST_SAVED
!
! -------- Now we should find the point in the extended list. Internal plotting
! -------- array will be destroyed after leaving DiaGI, but a user wants
! -------- to save information about point shifts
!
           ARG_DIST = (DIAGI_S%XMAX - DIAGI_S%XMIN)
           DO 410 J1=1,M
              IF ( ABS ( T(J1) - XC_ARG ) < ARG_DIST ) THEN
                   ARG_DIST = ABS ( T(J1) - XC_ARG )
                   IPM = J1
              END IF
 410       CONTINUE
!
! -------- ... in order to save the changes in the array NUM_AMB which a user
! -------- really wants
!
           CALL REPRERD ( FILE_NAME_1, REC_OBS(IPM) )
           CALL REPOBRD ( FILE_NAME_2, REC_OBS(IPM), REC_OBSF, JOBSREC_WORDS )
!
!C         write(6,*) 'REPPTSH: NAMB=  ',NAMB
!C         write(6,*) 'REPPTSH: RDOC= ',RDOC
!C         write(6,*) 'REPPTSH: Y(',IPM,')= ',Y(IPM)
!C         write(6,*) 'REPPTSH: PS_NS= ',PS_NS
!C         write(6,*) 'REPPTSH: IPM=  ',IPM
!C         write(6,*) 'REPPTSH: NUM_AMB(',IPM,')=  ', NUM_AMB(IPM)
!C         write(6,*) 'REPPTSH: IWAY=  ',IWAY
!C         write(6,*) 'REPPTSH: AMB_SP=  ',AMB_SP
!
           NUM_AMB(IPM) = NUM_AMB(IPM) + IWAY
           IF ( PS_NS == 'ps' .OR. PS_NS == 'ns' ) THEN
              Y(IPM) = Y(IPM) + IWAY * AMB_SP              ! group delays
!%%%
           ELSE
              Y(IPM) = Y(IPM) + IWAY                       ! phase delays
           END IF
!%%%
!
           NAMB = NAMB + IWAY
           IF ( PS_NS == 'ps' ) THEN
              RDOC = Y(IPM) / 1000.D0                      ! group delays (ps)
           ELSE IF ( PS_NS == 'ns' ) THEN
              RDOC = Y(IPM)                                ! group delays (ns)
!%%%
           ELSE IF ( PS_NS == 'tp' ) THEN
              RDOC = Y(IPM) * AMB_SP * 1E3                 ! phase delays
           END IF
!%%%
!
!C         write(6,*) 'REPPTSH: Y(',IPM,')= ',Y(IPM)
!C         write(6,*) 'REPPTSH: NAMB= ',NAMB
!C         write(6,*) 'REPPTSH: RDOC= ',RDOC
!C         write(6,*) 'REPPTSH: FILE_NAME_1= ',FILE_NAME_1
!C         write(6,*) 'REPPTSH: FILE_NAME_2= ',FILE_NAME_2
!
! -------- new values (the same variables for X- and S-Band)
!          
!%%%           write(6,*) 'REPPTSH: IDA_DTP=  ',IDA_DTP
!%%%           write(6,*) 'REPPTSH: PS_NS=  ',PS_NS
!%%%           write(6,*) 'REPPTSH: NUMAMB=  ',NUMAMB
!%%%           write(6,*) 'REPPTSH: NUMAMB_S=  ',NUMAMB_S
!%%%           write(6,*) 'REPPTSH: NPHAM4=  ',NPHAM4
!%%%           write(6,*) 'REPPTSH: NPHAM4_S=  ',NPHAM4_S
!%%%           write(6,*) 'REPPTSH: AMB_SP=  ',AMB_SP
!%%%           write(6,*) 'REPPTSH: DOBS=  ',DOBS
!%%%           write(6,*) 'REPPTSH: DOBS_S=  ',DOBS_S
!%%%           write(6,*) 'REPPTSH: DPH=  ',DPH
!%%%           write(6,*) 'REPPTSH: DPH_S=  ',DPH_S
!%%%
           IF ( IDA_DTP == 'GX' ) THEN
              NUMAMB = NUMAMB + IWAY
           ELSE IF ( IDA_DTP == 'GS' ) THEN
              NUMAMB_S = NUMAMB_S + IWAY
           ELSE IF ( IDA_DTP == 'PX' ) THEN
              NPHAM4 = NPHAM4 + IWAY
           ELSE IF ( IDA_DTP == 'PS' ) THEN
              NPHAM4_S = NPHAM4_S + IWAY
           END IF
           IF ( PS_NS == 'ps' ) THEN
              DOBS = DOBS + ((IWAY * AMB_SP) / 1000000.D0)  ! group delays (ps)
           ELSE IF ( PS_NS == 'ns' ) THEN
              DOBS = DOBS + ((IWAY * AMB_SP) / 1000.D0)     ! group delays (ns)
           ELSE IF ( PS_NS == 'tp' ) THEN                 ! phase delays (turns of phase)
              IF ( IDA_DTP == 'PX' ) THEN                 ! X-phase
                 DPH = DPH + ((IWAY * AMB_SP))
              ELSE IF ( IDA_DTP == 'PS' ) THEN            ! S-phase
                 DPH_S = DPH_S + ((IWAY * AMB_SP))
              END IF
!%%%
           ENDIF
!
!%%%           write(6,*) 'REPPTSH: REC_OBS(',IPM,')= ',REC_OBS(IPM)
!%%%           write(6,*) 'REPPTSH: NUM_AMB(',IPM,')=  ', NUM_AMB(IPM)
!%%%           write(6,*) 'REPPTSH: NUMAMB=  ',NUMAMB
!%%%           write(6,*) 'REPPTSH: NUMAMB_S=  ',NUMAMB_S
!%%%           write(6,*) 'REPPTSH: NPHAM4=  ',NPHAM4
!%%%           write(6,*) 'REPPTSH: NPHAM4_S=  ',NPHAM4_S
!%%%          write(6,*) 'REPPTSH: DOBS=  ',DOBS
!%%%           write(6,*) 'REPPTSH: DOBS_S=  ',DOBS_S
!%%%           write(6,*) 'REPPTSH: DPH=  ',DPH
!%%%           write(6,*) 'REPPTSH: DPH_S=  ',DPH_S
!
           CALL REPREWT ( FILE_NAME_1, REC_OBS(IPM) )
           CALL REPOBWT ( FILE_NAME_2, REC_OBS(IPM), REC_OBSF, JOBSREC_WORDS )
      END IF
!
! --- redraw the button headline
!
  420 CALL REPHEAD ( DIAGI_S, FUNC_N, FUNC_B, FUNC_KEY, NEW_BUTT )
!
      REPPTSH = 1 ! Important: function should always return 1.
!                         ! if the function returns not 1, then DiaGI will print
!                         !  the error mesage and return to the main program
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  REPPTSH  #!#
