      FUNCTION   REPA_DF_INIT ( DIAGI_S, REP ) 
! ************************************************************************
! *                                                                      *
! *   Function REPA_DF_INIT works in the context of REPA. It is executed *
! *   just after DiaGi draws the plot and before DiaGi started to wait   *
! *   for user input. It makes several things:                           *
! *   1) Draws the boxes at the upper part of the plotting window        *
! *      with information about the modes and the current mode of mouse  *
! *      button bindings.                                                *
! *   2) Prints the ambiguity spacing for the first observation of the   *
! *      baseline if applicatble;                                        *
! *   3) Binds keys and mouse bittons according to the current REPA      *
! *      mode.                                                           *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  DIAGI_S ( RECORD    ) -- Object which keeps internal parameters for *
! *                           plotting the current window.               *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
! *                                                                      *
! *  ### 06-DEC-2004  REPA_DF_INIT  v1.0 (c) L. Petrov  06-DEC-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  REPA_DF_INIT 
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'repa.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      TYPE     ( REP__TYPE  ) :: REP
      REAL*4     BOX_XLB, BOX_XTU, BOX_YLB, BOX_YTU 
      INTEGER*4  COL_OPR(3), COL_LET(3), COL_CUR(3), &
     &           ICOL_OPR,  ICOL_LET,   ICOL_CUR, N1$ARG
      DATA       (  COL_OPR(N1$ARG), N1$ARG=1,3 ), ICOL_OPR, &
     &           (  COL_LET(N1$ARG), N1$ARG=1,3 ), ICOL_LET, &
     &           (  COL_CUR(N1$ARG), N1$ARG=1,3 ), ICOL_CUR  &
     &           /  220, 208, 185,  37,  & ! color for operation BUTTON
     &              180,  36,  66,  42,  & ! color for letter code
     &              220, 183, 114,  41   & ! color for plot foreground (PS,GIF)
     &           /
      REAL*4     XLB_B1, XTU_B1, X_INC, XLB, YLB, YTU, XLB_ADJ1, XLB_ADJ2, &
     &           YLB_ADJ, XMS
      REAL*8     SPAMB
      CHARACTER  STR*32
      INTEGER*4  J1, J2, J3, J4, J5, IND_BAS, IND_OBS, IND_CLR_OLD 
      REAL*8,    EXTERNAL :: REPA_GET_AMBSP
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- We should extinguish DiaGi label at the bottom line
!
      CALL DIAGI_PURGE_BOT ( DIAGI_S )
!
! --- Redefine the size for vertical coordinate for the label
!
      IF ( DIAGI_S%IDEV == 1 ) THEN
           DIAGI_S%YSH_LAB = -0.070
        ELSE IF ( DIAGI_S%IDEV == 2 ) THEN
           DIAGI_S%YSH_LAB = -0.075
        ELSE IF ( DIAGI_S%IDEV == 3 ) THEN
           DIAGI_S%YSH_LAB = -0.095
      END IF
!
! --- Set sizes for putting boxes. 
!
      XLB_B1   = 0.075
      XTU_B1   = 0.175
      X_INC    = 0.140
      XMS      = XLB_B1 + X_INC*4
      XLB      = 0.984
      YLB      = 0.979 
      YTU      = 0.999
      XLB_ADJ1 = 0.005
      XLB_ADJ2 = 0.040
      YLB_ADJ  = 0.004
!
! --- Set Multi_Diagi colors
!
      CALL PGCOL_RGB ( ICOL_OPR, COL_OPR(1), COL_OPR(2), COL_OPR(3) )
      CALL PGCOL_RGB ( ICOL_LET, COL_LET(1), COL_LET(2), COL_LET(3) )
      CALL PGCOL_RGB ( ICOL_CUR, COL_CUR(1), COL_CUR(2), COL_CUR(3) )
!
! --- Set the size of the view port and the plotting area: the whole screen
!
      CALL PGVSIZ  ( 0.0, (DIAGI_S%XLEFT+DIAGI_S%XRIGHT)/25.4, &
     &               0.0, (DIAGI_S%YBOT+DIAGI_S%YTOP)/25.4     )
!
! --- Set world coordinates running from 0 to 1
!
      CALL PGSWIN ( 0.0, 1.0, 0.0, 1.0 )
!
! --- Draw boxes with mode names
!
      DO 410 J1=1,REP__M_MOD 
!
! ------ Set coordinate of the angles of the J1-th box
!
         BOX_XLB = XLB_B1 + X_INC*(J1-1)
         BOX_XTU = XTU_B1 + X_INC*(J1-1)
         BOX_YLB = YLB
         BOX_YTU = YTU
!
! ------ Set color of the box
!
         IF ( J1 .EQ. REP%CNF%MOD_IND ) THEN
!
! ----------- This mode is the current one
!
              CALL PGSCI ( ICOL_CUR )
            ELSE 
              CALL PGSCI ( ICOL_OPR )
         END IF
!
! ------ Draw the J1-th box
!
         CALL PGSFS  ( 1 )
         CALL PGRECT ( BOX_XLB, BOX_XTU, BOX_YLB, BOX_YTU )
!
! ------ Fraw the perimter of the box
!
         CALL PGSFS  ( 2 )
         CALL PGSCI  ( 1 )
         CALL PGRECT ( BOX_XLB, BOX_XTU, BOX_YLB, BOX_YTU )
!
! ------ Draw the key code associated with the J1-th box
!
         CALL PGSCI  ( ICOL_LET )
         CALL PGSCH  ( DIAGI_S%SCH_FRM  )
         CALL PGPTXT ( BOX_XLB+XLB_ADJ1, BOX_YLB+YLB_ADJ, 0.0, 0.0, &
     &                 REP__CH_KEY(J1) )
!
! ------ Draw the function name associated with the J1-th box
!
         CALL PGSCI  ( 1 )
         CALL PGPTXT ( BOX_XLB+XLB_ADJ2, BOX_YLB+YLB_ADJ, 0.0, 0.0, &
     &                 REP__CH_MOD(J1) )
 410  CONTINUE 
!
! --- Find the baseline index
!
      IND_BAS = 0
      DO 420 J2=1,REP%N_BAS
         IF ( LOC(REP%DIAGI(J2)) .EQ. LOC(DIAGI_S) ) IND_BAS = J2
 420  CONTINUE 
!
      IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'Delay   ' ) THEN
!
! -------- If the value is delay, we need to put ambiguity spacing value
! -------- Learn the index of the current baseline first
!
!
! -------- Find the first observation at this baseline
!
           IND_OBS = 0
           IF ( REP%PLT(IND_BAS)%N_GOO .GT. 0 ) THEN
                IND_OBS = REP%PLT(IND_BAS)%IND_GOO(1)
              ELSE IF ( REP%PLT(IND_BAS)%N_BAD .GT. 0 ) THEN
                IND_OBS = REP%PLT(IND_BAS)%IND_BAD(1)
              ELSE IF ( REP%PLT(IND_BAS)%N_UNR .GT. 0 ) THEN
                IND_OBS = REP%PLT(IND_BAS)%IND_UNR(1)
           END IF
!
           IF ( IND_OBS .GT. 0 ) THEN
!
! ------------- Learn the ambiguity spacing
!
                SPAMB = REPA_GET_AMBSP ( REP, IND_OBS )
                IF ( SPAMB .GT. REPA__M_AMBSP ) THEN
!
! ------------------ Non-null value means that ambiguity spacing is applicatble
! ------------------ for this combination of observables
!
                     IF ( SPAMB .GT. 1.D-9 ) THEN
                          WRITE ( UNIT=STR, FMT='(F8.3," ns")' ) SPAMB*1.D9
                        ELSE 
                          WRITE ( UNIT=STR, FMT='(F8.0," ps")' ) SPAMB*1.D12
                     END IF
                     STR = 'Amb. spacing: '//STR
!
! ------------------ Set color, font size and print the spacing
!
                     CALL PGSCI  ( 1 )
                     CALL PGSCH  ( DIAGI_S%SCH_FRM  )
                     CALL PGPTXT ( XLB, YLB, 0.0, 1.0, STR(1:I_LEN(STR)) )
                END IF
           END IF
      END IF
!
      IF ( REP%CNF%MARKED_SOURCE .NE. 'NO      '  .AND. &
     &     REP%CNF%MARKED_SOURCE .NE. 'No      '        ) THEN
!
! -------- Print the name of the marked source
!
! -------- Set the color
!
           CALL PGCOL_RGB ( ITAB_CLR(REP%CNF%MARKED_CLR,1),   &
     &                      IRGB_DEF(REP%CNF%MARKED_CLR,1,1), &
     &                      IRGB_DEF(REP%CNF%MARKED_CLR,1,2), &
     &                      IRGB_DEF(REP%CNF%MARKED_CLR,1,3)  )
           CALL PGSCI  ( ITAB_CLR(REP%CNF%MARKED_CLR,1) )
           IF ( DIAGI_S%IDEV == 2 ) THEN
                CALL PGSCH  ( 0.9*DIAGI_S%SCH_FRM  )
              ELSE 
                CALL PGSCH  ( DIAGI_S%SCH_FRM  )
           END IF
           CALL PGSLW  ( 1 )
!
! -------- Print the text
!
           CALL PGPTXT ( XMS, YLB, 0.0, 0.0, 'Marked source: '// &
     &                   REP%CNF%MARKED_SOURCE )
      END IF
      CALL PGSCI  ( 1 )
!
! --- Restore the viewport
!
      CALL PGVSIZ ( DIAGI_S%XLEFT/25.4, &
     &              DIAGI_S%XRIGHT/25.4 + (1-DIAGI_XRM)*DIAGI_S%XLEFT/25.4, &
     &              DIAGI_S%YBOT/25.4,  DIAGI_S%YTOP/25.4   )
      CALL PGSWIN ( DIAGI_S%XMIN, DIAGI_S%XMAX, DIAGI_S%YMIN, DIAGI_S%YMAX )
!
! --- Change the font size for labels and apply it
!
      DIAGI_S%SCH_LAB = SCH_LABS(DIAGI_S%IDEV)*REPA__LAB_RSC
      CALL PGSCH   ( DIAGI_S%SCH_LAB )
!
! --- Draw the bottom message
!
      CALL PGPTXT  ( DIAGI_S%XMIN, DIAGI_S%YMIN + &
     &               (DIAGI_S%YMAX-DIAGI_S%YMIN)*DIAGI_S%YSH_LAB, 0.0, 0.0, &
     &               DIAGI_S%MESS_BOT(1:I_LEN(DIAGI_S%MESS_BOT)) )
!
! --- Draw units
!
      IF ( ILEN(DIAGI_S%ARG_UNITS) .GT. 0 ) THEN
           CALL PGPTXT ( DIAGI_S%XMAX, DIAGI_S%YMIN + &
     &          (DIAGI_S%YMAX-DIAGI_S%YMIN)*DIAGI_S%YSH_ARU*DIAGI_S%SCH_FRM, &
     &          0.0, 1.0, DIAGI_S%ARG_UNITS(1:I_LEN(DIAGI_S%ARG_UNITS)) )
      END IF
!
! --- Now set bindings for user functions for all baselines
!
      DO 430 J3=1,REP%N_BAS
!
! ------ Initialization
!
         REP%DIAGI(J3)%USER_CHR(14) = CHAR(0)
         REP%DIAGI(J3)%USER_CHR(15) = CHAR(0)
         REP%DIAGI(J3)%USER_CHR(16) = CHAR(0)
         REP%DIAGI(J3)%USER_CHR(21) = CHAR(0)
         REP%DIAGI(J3)%USER_CHR(22) = CHAR(0)
         REP%DIAGI(J3)%USER_CHR(23) = CHAR(0)
         REP%DIAGI(J3)%USER_CHR(24) = CHAR(0)
         REP%DIAGI(J3)%USER_CHR(25) = CHAR(0)
         REP%DIAGI(J3)%USER_CHR(26) = CHAR(0)
         REP%DIAGI(J3)%USER_CHR(27) = CHAR(0)
!!         REP%DIAGI(J3)%USER_CHR(28) = CHAR(24)  ! I think this
!!         REP%DIAGI(J3)%USER_CHR(29) = CHAR(216) ! is wrong
!
         IF ( REP__CH_MOD(REP%CNF%MOD_IND) == 'DiaGi ' )  THEN
!
              REP%DIAGI(J3)%USER_CHR( 7) = 'X'
              REP%DIAGI(J3)%USER_CHR( 8) = 'x'
              REP%DIAGI(J3)%USER_CHR( 9) = 'Q'
              REP%DIAGI(J3)%USER_CHR(10) = 'q'
            ELSE IF ( REP__CH_MOD(REP%CNF%MOD_IND) == 'SngAmb' )  THEN
!
              REP%DIAGI(J3)%USER_CHR( 7) = CHAR(0)
              REP%DIAGI(J3)%USER_CHR( 8) = CHAR(0)
              REP%DIAGI(J3)%USER_CHR( 9) = 'Q'
              REP%DIAGI(J3)%USER_CHR(10) = 'q'
              IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'Delay   ' ) THEN
                   REP%DIAGI(J3)%USER_CHR(14) = 'A'
                   REP%DIAGI(J3)%USER_CHR(15) = 'X'
                 ELSE 
                   REP%DIAGI(J3)%USER_CHR(26) = 'A'
                   REP%DIAGI(J3)%USER_CHR(27) = 'X'
              END IF
              REP%DIAGI(J3)%USER_CHR(16) = 'D'
              REP%DIAGI(J3)%USER_CHR(24) = CHAR(8)
            ELSE IF ( REP__CH_MOD(REP%CNF%MOD_IND) == 'GrpAmb' )  THEN
!
              REP%DIAGI(J3)%USER_CHR( 7) = CHAR(0)
              REP%DIAGI(J3)%USER_CHR( 8) = CHAR(0)
              REP%DIAGI(J3)%USER_CHR( 9) = 'Q'
              REP%DIAGI(J3)%USER_CHR(10) = 'q'
              REP%DIAGI(J3)%USER_CHR(21) = 'D'
              REP%DIAGI(J3)%USER_CHR(24) = CHAR(8)
              IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'Delay   ' ) THEN
                   REP%DIAGI(J3)%USER_CHR(25) = 'A'
                   REP%DIAGI(J3)%USER_CHR(27) = 'X'
                 ELSE 
                   REP%DIAGI(J3)%USER_CHR(26) = 'A'
                   REP%DIAGI(J3)%USER_CHR(27) = 'X'
              END IF
            ELSE IF ( REP__CH_MOD(REP%CNF%MOD_IND) == 'GrpTgl' )  THEN
!
              REP%DIAGI(J3)%USER_CHR( 7) = CHAR(0)
              REP%DIAGI(J3)%USER_CHR( 8) = CHAR(0)
              REP%DIAGI(J3)%USER_CHR( 9) = 'Q'
              REP%DIAGI(J3)%USER_CHR(10) = 'q'
              REP%DIAGI(J3)%USER_CHR(22) = 'A'
              REP%DIAGI(J3)%USER_CHR(23) = 'X'
              REP%DIAGI(J3)%USER_CHR(24) = CHAR(8)
              REP%DIAGI(J3)%USER_CHR(26) = 'D'
         END IF
 430  CONTINUE 
!
      CALL REPA_SHOW_MS ( DIAGI_S, REP, IND_BAS, DIAGI_S%NCLR, &
     &                    REP%CNF%MARKED_CLR, &
     &                    DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)),         &
     &                    %VAL(DIAGI_S%ADR_Y4(1)), %VAL(DIAGI_S%ADR_E4(1)), &
     &                    DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)),         &
     &                    %VAL(DIAGI_S%ADR_Y4(2)), %VAL(DIAGI_S%ADR_E4(2)), &
     &                    DIAGI_S%NPOI(3), %VAL(DIAGI_S%ADR_X4(3)),         &
     &                    %VAL(DIAGI_S%ADR_Y4(3)), %VAL(DIAGI_S%ADR_E4(3))  )
!
      REPA_DF_INIT = DIAGI__CONT   ! Important: DiaGI user function should 
!                                  ! always return 1.
!                   ! If the function returns not 1, then DiaGI will print
!                   ! the error mesage and return to the main program!
      RETURN
      END  FUNCTION  REPA_DF_INIT
