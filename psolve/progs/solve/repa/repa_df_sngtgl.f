      FUNCTION   REPA_DF_SNGTGL ( DIAGI_S, REP ) 
! ************************************************************************
! *                                                                      *
! *   Function REPA_DF_SNGTGL toggles the status suppressed/resurrected  *
! *   of a single point.
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  DIAGI_S ( RECORD    ) -- Object which keeps internal parameters for *
! *                           plotting the current window.               *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
! *                                                                      *
! *  ### 07-DEC-2004  REPA_DF_SNGTGL v1.0 (c) L. Petrov  07-DEC-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  REPA_DF_SNGTGL
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'repa.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      TYPE     ( REP__TYPE  ) :: REP
      CHARACTER  STR*32
      REAL*4     XPT_R4, YPT_R4, EPT_R4 
      REAL*8     XPT_R8, YPT_R8
      INTEGER*4  IND_BAS, J1, IND_CLR, IND_PT, IND_SAVE, NEW_CLR
!
! --- Determine the index of the baseline which corresponds to the current
! --- window
!
      IND_BAS = 0
      DO 410 J1=1,REP%N_BAS
         IF ( LOC(REP%DIAGI(J1)) .EQ. LOC(DIAGI_S) ) IND_BAS = J1
 410  CONTINUE 
!
! --- Find the point which is the closest to the cursor
!
      CALL REPA_SEARCH_CLOSEST ( DIAGI_S%XC, DIAGI_S%YC, DIAGI_S%NCLR,        &
           DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), %VAL(DIAGI_S%ADR_Y4(1)), &
           DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), %VAL(DIAGI_S%ADR_Y4(2)), &
     &                   0, %VAL(DIAGI_S%ADR_X4(3)), %VAL(DIAGI_S%ADR_Y4(3)), &
     &     DIAGI_S%XMIN, DIAGI_S%XMAX, DIAGI_S%YMIN, DIAGI_S%YMAX,            &
     &     IND_CLR, IND_PT )
!
      IF ( IND_PT .GT. 0 ) THEN
!
! -------- Very well. We have found such a point. Now let's toggle suppression
! -------- status
!
           CALL REPA_SNGTGL ( REP, IND_BAS, IND_CLR, IND_PT,   &
                              DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), &
     &                                         %VAL(DIAGI_S%ADR_Y4(1)), &
     &                                         %VAL(DIAGI_S%ADR_E4(1)), &
     &                        DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), &
     &                                         %VAL(DIAGI_S%ADR_Y4(2)), &
     &                                         %VAL(DIAGI_S%ADR_E4(2)), &
     &                        XPT_R4, YPT_R4, EPT_R4, XPT_R8, YPT_R8, IND_SAVE )
!
           IF ( IND_CLR .EQ. REPA__I_GOO ) NEW_CLR = REPA__I_BAD 
           IF ( IND_CLR .EQ. REPA__I_BAD ) NEW_CLR = REPA__I_GOO 
!
! -------- Re-draw the point
!
           CALL DIAGI_DRAW ( DIAGI_S, NEW_CLR, NEW_CLR, 1, XPT_R4, YPT_R4, &
     &                       EPT_R4, XPT_R8, YPT_R8 )

!
! -------- Increment the command stack
!
           REP%N_COM = REP%N_COM + 1
           IF ( REP%N_COM .GT. REPA__M_COM ) THEN
                CALL CLRCH   ( STR ) 
                CALL INCH    ( REPA__M_COM, STR ) 
                CALL ERR_LOG ( 7801, -1, 'REPA_DF_SNGTGL', 'Trap of '// &
     &              'internal control: Command stack is too small: '// &
     &              'REPA__M_COM: '// STR )
                CALL EXIT ( 111 ) 
           END IF
!
! -------- Put the command into the stack
!
           REP%COM(REP%N_COM)%IND_COM = REP__COM_SNGTGL 
           REP%COM(REP%N_COM)%IND_SBC = REP__COM_SNGTGL
           REP%COM(REP%N_COM)%IND_BAS = IND_BAS
           REP%COM(REP%N_COM)%IND_OBS = IND_SAVE
           REP%COM(REP%N_COM)%IVAL    = IND_CLR
      END IF
!
      REPA_DF_SNGTGL = DIAGI__CONT
      RETURN
      END  FUNCTION  REPA_DF_SNGTGL 
