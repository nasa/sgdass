      FUNCTION   REPA_DF_GRPTGL ( DIAGI_S, REP, MODE )
! ************************************************************************
! *                                                                      *
! *   Function REPA_DF_GRPTGL toggles suppression status for a group     *
! *   of points at the current plot. If a user hit the Left Mouse        *
! *   button, all good points with residual by modulo greater than the   *
! *   current cursor position will be suppressed. If a user hit the      *
! *   Right Mouse button, all bad points with residual by modulo less    *
! *   than the current cursor position will be restored.                 *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  DIAGI_S ( RECORD    ) -- Object which keeps internal parameters for *
! *                           plotting the current window.               *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
! *                                                                      *
! *  ### 10-DEC-2004  REPA_DF_GRPTGL v1.0 (c) L. Petrov  10-DEC-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  REPA_DF_GRPTGL 
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'repa.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      TYPE     ( REP__TYPE  ) :: REP
      INTEGER*4  MODE
      REAL*4     XPT_R4, YPT_R4, EPT_R4
      REAL*8     XPT_R8, YPT_R8
      REAL*8     ARG_RANGE, VAL_RANGE 
      CHARACTER  STR*32
      INTEGER*4  IAMB, IND_BAS, IND_CLR, IND_PT, IND_OBS, IND_SAVE, &
     &           NCOM_SAVED, IND_ARR(REPA__M_COM), N_GOO, N_BAD, &
     &           J1, J2, J3, J4, J5 
      INTEGER*4, EXTERNAL :: IFIND_PL
      REAL*8,    EXTERNAL :: REPA_GET_AMBSP 
!
      IF ( DIAGI_S%YC .LT. DIAGI_S%YMIN .OR. &
     &     DIAGI_S%YC .GT. DIAGI_S%YMAX      ) THEN
!
! -------- If the point is either heigher than the upper limit or
! -------- below the low limit, -- do nothing.
!
           REPA_DF_GRPTGL = DIAGI__CONT
           RETURN 
      END IF
!
! --- Determine the index of the baseline which corresponds to the current
! --- window
!
      IND_BAS = 0
      DO 410 J1=1,REP%N_BAS
         IF ( LOC(REP%DIAGI(J1)) .EQ. LOC(DIAGI_S) ) IND_BAS = J1
 410  CONTINUE 
!
      NCOM_SAVED = REP%N_COM 
      IF ( REP%PLT(IND_BAS)%N_GOO .GT. 0 ) THEN
!
! -------- We have to save N_GOO list since it will be changed during 
! -------- inside the cycle
!
           CALL COPY_I4 ( REP%PLT(IND_BAS)%N_GOO, REP%PLT(IND_BAS)%IND_GOO, &
     &                    IND_ARR )
           N_GOO = REP%PLT(IND_BAS)%N_GOO 
           DO 420 J2=1,N_GOO
              IND_OBS = IND_ARR(J2)
!
! ----------- Find the new index in the new GOO-list
!
              IND_PT = IFIND_PL ( REP%PLT(IND_BAS)%N_GOO, &
     &                            REP%PLT(IND_BAS)%IND_GOO, IND_OBS )
              IF ( ABS(REP%PLT(IND_BAS)%VAL_GOO(IND_PT)) > ABS(DIAGI_S%YC) &
     &             .AND.  MODE .EQ. -1     ) THEN
!
! ---------------- Suppression of a good point which is beyond |DIAGI_S%YC|
!
                   CALL REPA_SNGTGL ( REP, IND_BAS, REPA__I_GOO, IND_PT,      &
                                    DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), &
     &                                               %VAL(DIAGI_S%ADR_Y4(1)), &
     &                                               %VAL(DIAGI_S%ADR_E4(1)), &
     &                              DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), &
     &                                               %VAL(DIAGI_S%ADR_Y4(2)), &
     &                                               %VAL(DIAGI_S%ADR_E4(2)), &
     &                              XPT_R4, YPT_R4, EPT_R4, XPT_R8, YPT_R8,   &
     &                              IND_SAVE )
!
! ---------------- Put the command into the stack
!
                   REP%N_COM = REP%N_COM + 1
                   IF ( REP%N_COM .GT. REPA__M_COM ) THEN
                        CALL CLRCH   ( STR ) 
                        CALL INCH    ( REPA__M_COM, STR ) 
                        CALL ERR_LOG ( 7806, -1, 'REPA_GRPTGL', 'Trap of '//   &
     &                      'internal control. Command stack is too small: '// &
     &                      'REPA__M_COM: '// STR )
                        CALL EXIT ( 111 ) 
                   END IF
!
                   IF ( REP%N_COM .EQ. NCOM_SAVED+1 ) THEN
!
! --------------------- If the point is the first for REP__COM_GRPAMB,
! --------------------- then put there the command name, 
!
                        REP%COM(REP%N_COM)%IND_COM = REP__COM_GRPTGL 
                      ELSE 
!
! --------------------- otherwise put zero. THis trick will allow "UNDO"
! --------------------- operation when it should stop undoing
!
                        REP%COM(REP%N_COM)%IND_COM = 0
                   END IF
!
! ---------------- Put the sub-command index into the stack as well as
! ---------------- information about the baseline, observation and the 
! ---------------- ambiguity shift
!
                   REP%COM(REP%N_COM)%IND_SBC = REP__COM_SNGTGL
                   REP%COM(REP%N_COM)%IND_BAS = IND_BAS
                   REP%COM(REP%N_COM)%IND_OBS = IND_SAVE
                   REP%COM(REP%N_COM)%IVAL    = REPA__I_GOO
              END IF
 420       CONTINUE 
      END IF
!
      IF ( REP%PLT(IND_BAS)%N_BAD .GT. 0 ) THEN
!
! -------- We have to save N_BAD list since it will be changed during 
! -------- inside the cycle
!
           CALL COPY_I4 ( REP%PLT(IND_BAS)%N_BAD, REP%PLT(IND_BAS)%IND_BAD, &
     &                    IND_ARR )
           N_BAD = REP%PLT(IND_BAS)%N_BAD 
           DO 430 J3=1,N_BAD 
              IND_OBS = IND_ARR(J3)
!
! ----------- Find the new index in the new BAD-list
!
              IND_PT = IFIND_PL ( REP%PLT(IND_BAS)%N_BAD, &
     &                            REP%PLT(IND_BAS)%IND_BAD, IND_OBS )
              IF ( ABS(REP%PLT(IND_BAS)%VAL_BAD(IND_PT)) < ABS(DIAGI_S%YC) &
     &             .AND.  MODE .EQ.  1 ) THEN
!
! ---------------- Resurrection of a bad point which is inside |DIAGI_S%YC|
!
                   CALL REPA_SNGTGL ( REP, IND_BAS, REPA__I_BAD, IND_PT,      &
                                    DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), &
     &                                               %VAL(DIAGI_S%ADR_Y4(1)), &
     &                                               %VAL(DIAGI_S%ADR_E4(1)), &
     &                              DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), &
     &                                               %VAL(DIAGI_S%ADR_Y4(2)), &
     &                                               %VAL(DIAGI_S%ADR_E4(2)), &
     &                              XPT_R4, YPT_R4, EPT_R4, XPT_R8, YPT_R8,   &
     &                              IND_SAVE )
!
! ---------------- Put the command into the stack
!
                   REP%N_COM = REP%N_COM + 1
                   IF ( REP%N_COM .GT. REPA__M_COM ) THEN
                        CALL CLRCH   ( STR ) 
                        CALL INCH    ( REPA__M_COM, STR ) 
                        CALL ERR_LOG ( 7807, -1, 'REPA_GRPTGL', 'Trap of '//   &
     &                      'internal control. Command stack is too small: '// &
     &                      'REPA__M_COM: '// STR )
                        CALL EXIT ( 111 ) 
                   END IF
!
                   IF ( REP%N_COM .EQ. NCOM_SAVED+1 ) THEN
!
! --------------------- If the point is the first for REP__COM_GRPAMB,
! --------------------- then put there the command name, 
!
                        REP%COM(REP%N_COM)%IND_COM = REP__COM_GRPTGL 
                      ELSE 
!
! --------------------- otherwise put zero. THis trick will allow "UNDO"
! --------------------- operation when it should stop undoing
!
                        REP%COM(REP%N_COM)%IND_COM = 0
                   END IF
!
! ---------------- Put the sub-command index into the stack as well as
! ---------------- information about the baseline, observation and the 
! ---------------- ambiguity shift
!
                   REP%COM(REP%N_COM)%IND_SBC = REP__COM_SNGTGL
                   REP%COM(REP%N_COM)%IND_BAS = IND_BAS
                   REP%COM(REP%N_COM)%IND_OBS = IND_SAVE
                   REP%COM(REP%N_COM)%IVAL    = REPA__I_BAD
              END IF
 430       CONTINUE 
      END IF
!
      IF ( REP%N_COM .GT. NCOM_SAVED ) THEN
!
! -------- Aga. It turned out that at least one point was changed. Let's
! -------- re-draw the plot
!
           CALL REPA_DF_SETBOX ( DIAGI_S, REP, REP%PLT(IND_BAS)%BOU_IND ) 
      END IF
!
      REPA_DF_GRPTGL = DIAGI__CONT
      RETURN
      END  FUNCTION  REPA_DF_GRPTGL 
