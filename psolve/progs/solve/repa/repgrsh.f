      FUNCTION   REPGRSH ( DIAGI_S, M, N_COL, T, Y, YG, EG, YM, YB, &
     &                     NUM_AMB, AMB_SP, REC_OBS, &
     &                     FUNC_N, FUNC_B, FUNC_KEY, &
     &                     FILE_NAME_1, FILE_NAME_2, PS_NS, &
     &                     INFO_CHR_G, INFO_CHR_M, INFO_CHR_B, &
     &                     IUER )
!
! ************************************************************************
! *                                                                      *
! *  DiaGI user function REPGRSH shifts all points with                  *
! *  | Y curser position - Y point value | > AMB_SP/2                    *
! *  Array NAMB keeps tracks of shifting.                                *
! *  If the K-th point is shifted UP then NAMB(K) := NAMB(K) + 1. If     *
! *  it is shifted DOWN then NAMB(K) := NAMB(K) - 1.                     *
! *                                                                      *
! *  called subroutines:                                                 *
! *  PGCOL_RGB, REPAMBI, LIB$MOVC3, DIAGI_DRAW, REPSTAT, ERR_LOG         *
! *                                                                      *
! *  calling routines:                                                   *
! *  DIAGI (via REPA and MULTI_DIAGI)                                    *
! *                                                                      *
! *  TERMS: G - good, M - manually suppressed, B - bad                   *
! *                                                                      *
! *  02-08-28             REPGRSH                      Volkmar Thorandt  *
! *  02-10-21 VT - REPGRSH derivate for observation groups               *
! *  02-12-05 VT - added record array                                    *
! *  02-12-17 VT - added real*8 arrays                                   *
! *  02-12-19 VT - REPHEAD call                                          *
! *  03-01-10 GE - added rewrite of RESFxx and OBSFxx                    *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
!
      INTEGER*4  REPGRSH
!
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'                   ! DiaGi include
      INCLUDE    'oborg.i'
      INCLUDE    'obors.i'
      INCLUDE    'resfl.i'
      CHARACTER  FILE_NAME_1*100             ! residual file name variable RESFxx
      CHARACTER  FILE_NAME_2*100             ! obs. file name variable OBSFxx
      CHARACTER  PS_NS*2                     ! unit (ps/ns)
      INTEGER*2  REC_OBSF( JOBSREC_WORDS )   ! OBSFxx record (JOBSREC_WORDS s. solve.i)
      EQUIVALENCE ( REC_OBSF, FJD )
!
      TYPE ( DIAGI_STRU ) ::  DIAGI_S   ! DiaGi_STRU record
      INTEGER*4  N_COL                       ! # of active colours (obs. functions in DIAGI_S)
      INTEGER*4  M                           ! total # of observations
      INTEGER*4  FUNC_N                      ! current # of function keys
      CHARACTER  FUNC_KEY(FUNC_N)*1          ! current keybord keys of user functions (s. repa.i)
      CHARACTER  FUNC_B(FUNC_N)*8            ! current button names of user functions (s. repa.i)
      CHARACTER  NEW_BUTT*8                  ! chosen button
      INTEGER*4  IUER                        ! error handler
      REAL*8     T(M)                        ! all arguments (G+M+B)
      REAL*8     Y(M)                        ! all values (G+M+B)
      REAL*8     YG(M), YM(M), YB(M)         ! values (G,M,B)
      REAL*8     EG(M)                       ! errors (G)
      REAL*8     AMB_SP                      ! ambiguity step (real*8)
      REAL*4     AMB_4                       ! ambiguity step (real*4)
      INTEGER*4  NUM_AMB(M)                  ! array with # of amb. steps for each observ.
      INTEGER*4  REC_OBS(M)                  ! record #s of baseline observations
      CHARACTER  INFO_CHR_G(M)*87            ! information for bottom line (good)
      CHARACTER  INFO_CHR_M(M)*87            ! information for bottom line (recoverable)
      CHARACTER  INFO_CHR_B(M)*87            ! information for bottom line (bad)
      INTEGER*4  I_TMP                       ! temporary value
      REAL*4     XC_ARG                      ! argument of observation
      REAL*4     YC_VAL                      ! value of observation
      REAL*4     EC_SIG                      ! error of observation
      REAL*4     ARG_DIST                    ! misc. value
      INTEGER*4  J1, J2, J3                  ! loop variables
      INTEGER*4  IPM                         ! index in full observation sets
      INTEGER*4  ILST_SAVED                  ! saved line style
      INTEGER*4  ICLR_GRY                    ! index of grey color
      PARAMETER  ( ICLR_GRY = 16 )
      INTEGER*4  IWAY                        ! # of ambiguity steps (local variable!)
      REAL*4     AGREY(M,3)                  ! array with values for grey coloured points
      INTEGER*4  IGREY                       ! # of points in AGRAY (= # of shifted points)
! *****************************************************************************************
      IGREY = 0
!
      CALL DIAGI_PURGE_BOT ( DIAGI_S )
!
! --- write button headline
!
      CALL REPHEAD ( DIAGI_S, FUNC_N, FUNC_B, FUNC_KEY, NEW_BUTT )
!
! --- convert to real*4
!
      AMB_4 = AMB_SP
!
! --- define light grey color. In fact DiaGI does not define all possible
! --- colors, but only colors for the functions which are used.
      CALL PGCOL_RGB ( ITAB_CLR(ICLR_GRY,1),   IRGB_DEF(ICLR_GRY,1,1), &
     &                 IRGB_DEF(ICLR_GRY,1,2), IRGB_DEF(ICLR_GRY,1,3)  )
      CALL PGCOL_RGB ( ITAB_CLR(ICLR_GRY,2),   IRGB_DEF(ICLR_GRY,1,1), &
     &                 IRGB_DEF(ICLR_GRY,1,2), IRGB_DEF(ICLR_GRY,1,3)  )
!
! --- search for points with |DIAGI_S.YC - Y| > AMB_SP
!
      DO J1=1,N_COL                          ! loop over colors (good,recoverable,bad)
!
         DO J2=1,DIAGI_S%NPOI(J1)            ! loop over observations of current colour
!
! --------- check distance of current point from curser postion
!
!C          write(6,*) 'REPGRSH: DIAGI_S.NPOI(',J1,')=',DIAGI_S.NPOI(J1)
!C          write(6,*) 'REPGRSH: AMB_4=',AMB_4
!
! --------- search for point to be shifted
!
            IWAY = 0
!
            IF ( DIAGI_S%YC .LE. DIAGI_S%YMAX .AND. DIAGI_S%YC .GT. DIAGI_S%YMIN ) THEN
               CALL REPAMBI ( DIAGI_S%NPOI(J1), J2, %VAL(DIAGI_S%ADR_Y4(J1)), &
     &                                               DIAGI_S%YC, AMB_4, IWAY )
            END IF
!
! --------- distance of current point greater then half of ambiguity step
!
            IF ( IWAY .NE. 0  ) THEN
!
!C             write(6,*) 'REPGRSH: IWAY=',IWAY
!
! ------------ copy values to real*4 arrays
!
               CALL LIB$MOVC3( 4, %VAL(DIAGI_S%ADR_X4(J1)+(J2-1)*4), XC_ARG )
               CALL LIB$MOVC3( 4, %VAL(DIAGI_S%ADR_Y4(J1)+(J2-1)*4), YC_VAL )
               CALL LIB$MOVC3( 4, %VAL(DIAGI_S%ADR_E4(J1)+(J2-1)*4), EC_SIG )
!
! ------------ redraw the point
!
               ILST_SAVED = DIAGI_S%ILST(J1)
               DIAGI_S%ILST(1) = 1 ! enter point-by-point mode. It is done as
! a precaution. If the current line mode is
! "spline" than DIAGI may abnormally terminated
!
! ------------ draw it by background color (extinguish the current point)
!
               CALL DIAGI_DRAW ( DIAGI_S, J1, 0, 1, XC_ARG, YC_VAL, EC_SIG, &
     &                           0.0D0, 0.0D0 )
!
! ------------ fill the array for points which have to be shifted after end of loops
!
               IGREY = IGREY + 1
               AGREY(IGREY,1) = XC_ARG
               AGREY(IGREY,2) = YC_VAL
               AGREY(IGREY,3) = EC_SIG
!
! ------------ change the value of the point (real*4)
!
               YC_VAL = YC_VAL + IWAY*AMB_4
!
!C             write(6,*)'REPGRSH: COLOUR, NUM, YC_VAL=',J1,' ',J2,' ',YC_VAL
!
! ------------ change this value in the internal plotting array
!
               CALL LIB$MOVC3( 4, YC_VAL, %VAL(DIAGI_S%ADR_Y4(J1)+(J2-1)*4) )
!
! ------------ change values in real*8 arrays
!
               IF ( J1 .EQ. 1 ) THEN
                  YG(J2) = YG(J2) + IWAY*AMB_SP                  ! value
                  I_TMP = IDNINT( YG(J2) )
                  WRITE( INFO_CHR_G(J2)(51:58), '(I8)' ) I_TMP   ! info array (value)
               ELSE IF ( J1 .EQ. 2 ) THEN
                  YM(J2) = YM(J2) + IWAY*AMB_SP                  ! value
                  I_TMP = IDNINT( YM(J2) )
                  WRITE( INFO_CHR_M(J2)(51:58), '(I8)' ) I_TMP   ! info array (value)
               ELSE IF ( J1 .EQ. 3 ) THEN
                  YB(J2) = YB(J2) + IWAY*AMB_SP                  ! value
                  I_TMP = IDNINT( YB(J2) )
                  WRITE( INFO_CHR_B(J2)(51:58), '(I8)' ) I_TMP   ! info array (value)
               END IF
!
! ------------ redraw the point with the current color
!
               CALL DIAGI_DRAW ( DIAGI_S, J1, J1, 1, XC_ARG, YC_VAL, EC_SIG, &
     &                           0.0D0, 0.0D0 )
               DIAGI_S%ILST(1) = ILST_SAVED
!
! ------------ Now we should find the point in the extended list. Internal plotting
! ------------ array will be destroyed after leaving DiaGI, but a user wants
! ------------ to save information about point shifts.
!
               ARG_DIST = (DIAGI_S%XMAX - DIAGI_S%XMIN)
               DO J3=1,M
                  IF ( ABS ( T(J3) - XC_ARG ) .LT. ARG_DIST ) THEN
                       ARG_DIST = ABS ( T(J3) - XC_ARG )
                       IPM = J3
                  END IF
               END DO
!
! -------- reset residual file and observation file
!
           CALL REPRERD ( FILE_NAME_1, REC_OBS(IPM) )
           CALL REPOBRD ( FILE_NAME_2, REC_OBS(IPM), REC_OBSF, JOBSREC_WORDS )
!
!C         write(6,*) 'REPGRSH: NAMB=  ',NAMB
!C         write(6,*) 'REPGRSH: RDOC= ',RDOC
!C         write(6,*) 'REPGRSH: Y(',IPM,')= ',Y(IPM)
!C         write(6,*) 'REPGRSH: PS_NS= ',PS_NS
!C         write(6,*) 'REPGRSH: IPM=  ',IPM
!C         write(6,*) 'REPGRSH: NUM_AMB(',IPM,')=  ', NUM_AMB(IPM)
!C         write(6,*) 'REPGRSH: IWAY=  ',IWAY
!C         write(6,*) 'REPGRSH: AMB_SP=  ',AMB_SP
!
           NUM_AMB(IPM) = NUM_AMB(IPM) + IWAY
           Y(IPM) = Y(IPM) + IWAY * AMB_SP
!
           NAMB = NAMB + IWAY
           IF (PS_NS.EQ.'ps') THEN
              RDOC = Y(IPM) / 1000.D0
           ELSE IF (PS_NS.EQ.'ns') THEN
              RDOC = Y(IPM)
           END IF
!
!C         write(6,*) 'REPGRSH: Y(',IPM,')= ',Y(IPM)
!C         write(6,*) 'REPGRSH: NAMB= ',NAMB
!C         write(6,*) 'REPGRSH: RDOC= ',RDOC
!C         write(6,*) 'REPGRSH: FILE_NAME_1= ',FILE_NAME_1
!C         write(6,*) 'REPGRSH: FILE_NAME_2= ',FILE_NAME_2
!C         write(6,*) 'REPGRSH: BANDKIND= ',BANDKIND
!
! -------- new values (the same variables for X- and S-Band)
!
           NUMAMB = NUMAMB + IWAY
           IF (PS_NS.EQ.'ps') THEN
              DOBS = DOBS + ((IWAY * AMB_SP) / 1000000.D0)
           ELSE IF (PS_NS.EQ.'ns') THEN
              DOBS = DOBS + ((IWAY * AMB_SP) / 1000.D0)
           ENDIF
!
!C         write(6,*) 'REPGRSH: REC_OBS(',IPM,')= ',REC_OBS(IPM)
!C         write(6,*) 'REPGRSH: NUM_AMB(',IPM,')=  ', NUM_AMB(IPM)
!C         write(6,*) 'REPGRSH: NUMAMB=  ',NUMAMB
!C         write(6,*) 'REPGRSH: NUMAMB_S=  ',NUMAMB_S
!C         write(6,*) 'REPGRSH: DOBS=  ',DOBS
!C         write(6,*) 'REPGRSH: DOBS_S=  ',DOBS_S
!
           CALL REPREWT ( FILE_NAME_1, REC_OBS(IPM) )
           CALL REPOBWT ( FILE_NAME_2, REC_OBS(IPM), REC_OBSF, JOBSREC_WORDS )
!
            ENDIF
         END DO
      END DO
!
! --- recalculate statistics and redraw the headline of the plot
!
      CALL REPSTAT ( DIAGI_S, YG, EG )
!
! --- redraw the button headline
!
      CALL REPHEAD ( DIAGI_S, FUNC_N, FUNC_B, FUNC_KEY, NEW_BUTT )
!
! --- draw grey points at old places
!
      DO J1=1,IGREY
         CALL DIAGI_DRAW ( DIAGI_S, 1, ICLR_GRY, 1, AGREY(J1,1), AGREY(J1,2), &
     &                           AGREY(J1,3), 0.0D0, 0.0D0 )
      END DO
!
      REPGRSH = 1 ! important: function should always return 1.
!                 ! if the function returns not 1, then DiaGI will print
!                 !  the error mesage and return to the main program
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  REPGRSH  #!#
