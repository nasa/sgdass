      FUNCTION  REPA_DF_UNDO ( DIAGI_S, REP )
! ************************************************************************
! *                                                                      *
! *   Function  REPA_DF_UNDO  undoes the latest operation of toggling    *
! *   suppression status or ambiguity change.                            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  DIAGI_S ( RECORD    ) -- Object which keeps internal parameters for *
! *                           plotting the current window.               *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
! *                                                                      *
! * ### 13-DEC-2004d  REPA_DF_UNDO   v1.0 (c) L. Petrov 13-DEC-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  REPA_DF_UNDO
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'repa.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      TYPE     ( REP__TYPE  ) :: REP
      REAL*4     XPT_R4, YPT_R4, EPT_R4, XOLD_R4, YOLD_R4, EOLD_R4, YNEW_R4
      REAL*8     XPT_R8, YPT_R8, XOLD_R8, YOLD_R8, YNEW_R8, AMBSP
      INTEGER*4  J1, IND_SAVE, IND_CLR, IND_PT,  NEW_CLR, ICLR_OLD
      PARAMETER  ( ICLR_OLD = 16 )
      INTEGER*4, EXTERNAL :: IFIND_PL
!
! --- Define light grey color. In fact DiaGI does not define all possible
! --- colors, but only colors for the functions which are used.
!
      CALL PGCOL_RGB ( ITAB_CLR(ICLR_OLD,1),   IRGB_DEF(ICLR_OLD,1,1), &
     &                 IRGB_DEF(ICLR_OLD,1,2), IRGB_DEF(ICLR_OLD,1,3)  )
      CALL PGCOL_RGB ( ITAB_CLR(ICLR_OLD,2),   IRGB_DEF(ICLR_OLD,1,1), &
     &                 IRGB_DEF(ICLR_OLD,1,2), IRGB_DEF(ICLR_OLD,1,3)  )
!
      IF ( REP%N_COM .EQ. 0 ) THEN
           REPA_DF_UNDO = DIAGI__CONT
           RETURN 
      END IF
      REPA_DF_UNDO = 0
!
! --- Scan the command stack from the tail to the head
!
      DO 410 J1=REP%N_COM,1,-1
         IF ( REP%COM(J1)%IND_SBC == REP__COM_SNGTGL ) THEN
!
! ----------- Aga, the latest subcommand was the status toggle
!
              IND_CLR = 0
              IND_PT  = 0
              IF ( REP%PLT(REP%COM(J1)%IND_BAS)%N_GOO .GT. 0 ) THEN
!
! ---------------- It was the change from bad to good
!
                   IND_PT = IFIND_PL ( REP%PLT(REP%COM(J1)%IND_BAS)%N_GOO,   &
     &                                 REP%PLT(REP%COM(J1)%IND_BAS)%IND_GOO, &
     &                                 REP%COM(J1)%IND_OBS )
                   IF ( IND_PT .GT. 0 ) IND_CLR = REPA__I_GOO 
              END IF
              IF ( REP%PLT(REP%COM(J1)%IND_BAS)%N_BAD .GT. 0  .AND. &
     &             IND_PT .LE. 0 ) THEN
!
! ---------------- It was the change from good to bad 
!
                   IND_PT = IFIND_PL ( REP%PLT(REP%COM(J1)%IND_BAS)%N_BAD,   &
     &                                 REP%PLT(REP%COM(J1)%IND_BAS)%IND_BAD, &
     &                                 REP%COM(J1)%IND_OBS )
                   IF ( IND_PT .GT. 0 ) IND_CLR = REPA__I_BAD 
              END IF
              IF ( IND_CLR .EQ. 0 ) RETURN ! It should never happen
!
! ----------- Well, let's toggle suppression status once again
!
              CALL REPA_SNGTGL ( REP, REP%COM(J1)%IND_BAS, IND_CLR, IND_PT, &
                                 DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)),  &
     &                                            %VAL(DIAGI_S%ADR_Y4(1)),  &
     &                                            %VAL(DIAGI_S%ADR_E4(1)),  &
     &                           DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)),  &
     &                                            %VAL(DIAGI_S%ADR_Y4(2)),  &
     &                                            %VAL(DIAGI_S%ADR_E4(2)),  &
     &                           XPT_R4, YPT_R4, EPT_R4, XPT_R8, YPT_R8,    &
     &                           IND_SAVE )
!
! ----------- Decrement the command stack
!
              REP%N_COM = REP%N_COM - 1
              IF ( REP%COM(J1)%IND_COM == REP%COM(J1)%IND_SBC ) THEN
                   IF ( IND_CLR .EQ. 1 ) NEW_CLR = 2
                   IF ( IND_CLR .EQ. 2 ) NEW_CLR = 1
!
! ---------------- This was a subcommand.
! ---------------- Re-draw the point
!
                   CALL DIAGI_DRAW ( DIAGI_S, NEW_CLR, NEW_CLR, 1, XPT_R4, &
     &                               YPT_R4, EPT_R4, XPT_R8, YPT_R8 )
                   REPA_DF_UNDO = DIAGI__CONT
                   RETURN
              END IF
            ELSE IF ( REP%COM(J1)%IND_SBC == REP__COM_SNGAMB ) THEN
!
! ----------- Ooo! the latest subcommand was ambiguity shift
!
              IND_CLR = 0
              IND_PT  = 0
              IF ( REP%PLT(REP%COM(J1)%IND_BAS)%N_GOO .GT. 0 ) THEN
!
! ---------------- It was ambiguity shoft of a good point
!
                   IND_PT = IFIND_PL ( REP%PLT(REP%COM(J1)%IND_BAS)%N_GOO,   &
     &                                 REP%PLT(REP%COM(J1)%IND_BAS)%IND_GOO, &
     &                                 REP%COM(J1)%IND_OBS )
                   IF ( IND_PT .GT. 0 ) IND_CLR = REPA__I_GOO 
              END IF
!
              IF ( REP%PLT(REP%COM(J1)%IND_BAS)%N_BAD .GT. 0  .AND. &
     &             IND_PT .LE. 0 ) THEN
!
! ---------------- It was ambiguity shoft of a bad point
!
                   IND_PT = IFIND_PL ( REP%PLT(REP%COM(J1)%IND_BAS)%N_BAD,   &
     &                                 REP%PLT(REP%COM(J1)%IND_BAS)%IND_BAD, &
     &                                 REP%COM(J1)%IND_OBS )
                   IF ( IND_PT .GT. 0 ) IND_CLR = REPA__I_BAD 
              END IF
!
              IF ( REP%PLT(REP%COM(J1)%IND_BAS)%N_UNR .GT. 0  .AND. &
     &             IND_PT .LE. 0 ) THEN
!
! ---------------- It was ambiguity shift of an unrecoverable point
!
                   IND_PT = IFIND_PL ( REP%PLT(REP%COM(J1)%IND_BAS)%N_UNR,   &
     &                                 REP%PLT(REP%COM(J1)%IND_BAS)%IND_UNR, &
     &                                 REP%COM(J1)%IND_OBS )
                   IF ( IND_PT .GT. 0 ) IND_CLR = REPA__I_UNR
              END IF
!
! ----------- Shift the ambiguity in the opposite direction
!
              CALL REPA_SNGAMB ( REP, -REP%COM(REP%N_COM)%IVAL,               &
     &                           REP%COM(REP%N_COM)%IND_BAS, IND_CLR, IND_PT, &
     &                           REP%COM(REP%N_COM)%IND_OBS,                  &
                                 DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)),    &
     &                                            %VAL(DIAGI_S%ADR_Y4(1)),    &
     &                                            %VAL(DIAGI_S%ADR_E4(1)),    &
     &                           DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)),    &
     &                                            %VAL(DIAGI_S%ADR_Y4(2)),    &
     &                                            %VAL(DIAGI_S%ADR_E4(2)),    &
     &                           DIAGI_S%NPOI(3), %VAL(DIAGI_S%ADR_X4(3)),    &
     &                                            %VAL(DIAGI_S%ADR_Y4(3)),    &
     &                                            %VAL(DIAGI_S%ADR_E4(3)),    &
     &                           XOLD_R4, YOLD_R4, EOLD_R4, XOLD_R8, YOLD_R8, &
     &                           YNEW_R4, YNEW_R8, AMBSP )
!
! ----------- Decrement the command stack
!
              REP%N_COM = REP%N_COM - 1
              IF ( REP%COM(J1)%IND_COM == REP%COM(J1)%IND_SBC ) THEN
!
! ---------------- This was a subcommand.
! ---------------- Re-draw the point
!
                   CALL DIAGI_DRAW ( DIAGI_S, IND_CLR, ICLR_OLD, 1, &
     &                             XOLD_R4, YOLD_R4, EOLD_R4, XOLD_R8, YOLD_R8 )
!
                   CALL DIAGI_DRAW ( DIAGI_S, IND_CLR, IND_CLR, 1, &
     &                             XOLD_R4, YNEW_R4, EOLD_R4, XOLD_R8, YNEW_R8 )
                   REPA_DF_UNDO = DIAGI__CONT
                   RETURN
              END IF
         END IF
!
         IF ( REP%COM(J1)%IND_COM == REP__COM_GRPAMB  .OR.  &
              REP%COM(J1)%IND_COM == REP__COM_GRPTGL        ) THEN
!
! ----------- This command was group ambiiguity shift or group suppression
! ----------- status toggle. WE have to redraw eniter plot
!
              CALL REPA_DF_SETBOX ( DIAGI_S, REP, &
     &                              REP%PLT(REP%COM(J1)%IND_BAS)%BOU_IND ) 
              REPA_DF_UNDO = DIAGI__CONT
              RETURN
         END IF
 410  CONTINUE 
!
      REPA_DF_UNDO = DIAGI__CONT
      RETURN
      END  FUNCTION  REPA_DF_UNDO
