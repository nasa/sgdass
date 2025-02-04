      FUNCTION   REPA_DF_GRPAMB ( DIAGI_S, REP )
! ************************************************************************
! *                                                                      *
! *   Function REPA_DF_SNGAMB resolves ambiguities of all points at      *
! *   the current baseline with repsect to the point which the cursor    *
! *   points at.                                                         *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  DIAGI_S ( RECORD    ) -- Object which keeps internal parameters for *
! *                           plotting the current window.               *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
! *                                                                      *
! *  ### 10-DEC-2004  REPA_DF_GRPAMB v1.0 (c) L. Petrov  10-DEC-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  REPA_DF_GRPAMB 
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'repa.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      TYPE     ( REP__TYPE  ) :: REP
      REAL*4     XOLD_R4, YOLD_R4, EOLD_R4, YNEW_R4
      REAL*8     XOLD_R8, YOLD_R8, YNEW_R8, AMBSP
      REAL*8     ARG_RANGE, VAL_RANGE 
      CHARACTER  STR*32
      INTEGER*4  IAMB, IND_BAS, IND_CLR, IND_PT, IND_OBS, &
     &           J1, J2, J3, J4, J5, NCOM_SAVED
      REAL*8     REPA_GET_AMBSP 
!
      IF ( DIAGI_S%YC .LT. DIAGI_S%YMIN .OR. &
     &     DIAGI_S%YC .GT. DIAGI_S%YMAX      ) THEN
!
! -------- If the point is either heigher than the upper limit or
! -------- below the low limit, -- do nothing.
!
           REPA_DF_GRPAMB = DIAGI__CONT
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
      NCOM_SAVED = REP%N_COM  ! Save the stack counter
      IF ( REP%PLT(IND_BAS)%N_GOO .GT. 0 ) THEN
!
! -------- Examine good points
!
           DO 420 J2=1,REP%PLT(IND_BAS)%N_GOO 
              IND_OBS = REP%PLT(IND_BAS)%IND_GOO(J2)
!
! ----------- Determine the ambiguity spacing which corresponds to the 
! ----------- current point
!
              AMBSP = REPA_GET_AMBSP ( REP, IND_OBS )
!
! ----------- Zero ambiguity spacing means that the operation of 
! ----------- ambiguity shift for this observalbe is not defined
!
              IF ( AMBSP .LT. REPA__M_AMBSP ) GOTO 420
!
! ----------- Resolve ambiguity with respect to the current cursor position
!
              IAMB = NINT( (DIAGI_S%YC - REP%PLT(IND_BAS)%VAL_GOO(J2))/ &
     &                     (AMBSP*REP%PLT(IND_BAS)%VAL_SCL) )
              IF ( IAMB .NE. 0  .AND.  ABS(IAMB) .LT. REPA__M_AMB ) THEN
!
! ---------------- If the ambiguity is not zero and in the allowed range,
! ---------------- the value of the point in internal DIAGI_S data structure
! ---------------- whcih is used for plotting
!
                   CALL REPA_SNGAMB ( REP, IAMB, IND_BAS, REPA__I_GOO, J2, IND_OBS, &
                                      DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), &
     &                                                 %VAL(DIAGI_S%ADR_Y4(1)), &
     &                                                 %VAL(DIAGI_S%ADR_E4(1)), &
     &                                DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), &
     &                                                 %VAL(DIAGI_S%ADR_Y4(2)), &
     &                                                 %VAL(DIAGI_S%ADR_E4(2)), &
     &                                DIAGI_S%NPOI(3), %VAL(DIAGI_S%ADR_X4(3)), &
     &                                                 %VAL(DIAGI_S%ADR_Y4(3)), &
     &                                                 %VAL(DIAGI_S%ADR_E4(3)), &
     &                                XOLD_R4, YOLD_R4, EOLD_R4, &
     &                                XOLD_R8, YOLD_R8,          &
     &                                YNEW_R4, YNEW_R8, AMBSP )
!
! ---------------- Put the command into the stack
!
                   REP%N_COM = REP%N_COM + 1
                   IF ( REP%N_COM .GT. REPA__M_COM ) THEN
                        CALL CLRCH   ( STR ) 
                        CALL INCH    ( REPA__M_COM, STR ) 
                        CALL ERR_LOG ( 7803, -1, 'REPA_GRPAMB', 'Trap of '// &
     &                      'internal control Command stack is too small: '// &
     &                      'REPA__M_COM: '// STR )
                        CALL EXIT ( 111 ) 
                   END IF
!
                   IF ( REP%N_COM .EQ. NCOM_SAVED+1 ) THEN
!
! --------------------- If the point is the first for REP__COM_GRPAMB,
! --------------------- then put there the command name, 
!
                        REP%COM(REP%N_COM)%IND_COM = REP__COM_GRPAMB 
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
                   REP%COM(REP%N_COM)%IND_SBC = REP__COM_SNGAMB
                   REP%COM(REP%N_COM)%IND_BAS = IND_BAS
                   REP%COM(REP%N_COM)%IND_OBS = IND_OBS
                   REP%COM(REP%N_COM)%IVAL    = IAMB
              END IF
 420       CONTINUE 
      END IF
!
      IF ( REP%PLT(IND_BAS)%N_BAD .GT. 0 ) THEN
!
! -------- Examine bad points
!
           DO 430 J3=1,REP%PLT(IND_BAS)%N_BAD 
              IND_OBS = REP%PLT(IND_BAS)%IND_BAD(J3)
!
! ----------- Determine the ambiguity spacing which corresponds to the 
! ----------- current point
!
              AMBSP = REPA_GET_AMBSP ( REP, IND_OBS )
!
! ----------- Zero amobioguity spacing means that the operation of 
! ----------- ambiguity shift for this pbservalbe is not defined
!
              IF ( AMBSP .LT. REPA__M_AMBSP ) GOTO 430
!
! ----------- Resolve ambiguity with respect to the current cursor position
!
              IAMB = NINT( (DIAGI_S%YC - REP%PLT(IND_BAS)%VAL_BAD(J3))/ &
     &                     (AMBSP*REP%PLT(IND_BAS)%VAL_SCL) )
              IF ( IAMB .NE. 0  .AND.  ABS(IAMB) .LT. REPA__M_AMB ) THEN
                   CALL REPA_SNGAMB ( REP, IAMB, IND_BAS, REPA__I_BAD, J3, IND_OBS, &
                                      DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), &
     &                                                 %VAL(DIAGI_S%ADR_Y4(1)), &
     &                                                 %VAL(DIAGI_S%ADR_E4(1)), &
     &                                DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), &
     &                                                 %VAL(DIAGI_S%ADR_Y4(2)), &
     &                                                 %VAL(DIAGI_S%ADR_E4(2)), &
     &                                DIAGI_S%NPOI(3), %VAL(DIAGI_S%ADR_X4(3)), &
     &                                                 %VAL(DIAGI_S%ADR_Y4(3)), &
     &                                                 %VAL(DIAGI_S%ADR_E4(3)), &
     &                                XOLD_R4, YOLD_R4, EOLD_R4, &
     &                                XOLD_R8, YOLD_R8,          &
     &                                YNEW_R4, YNEW_R8, AMBSP )
!
! ---------------- Put the command into the stack
!
                   REP%N_COM = REP%N_COM + 1
                   IF ( REP%N_COM .GT. REPA__M_COM ) THEN
                        CALL CLRCH   ( STR ) 
                        CALL INCH    ( REPA__M_COM, STR ) 
                        CALL ERR_LOG ( 7804, -1, 'REPA_GRPAMB', 'Trap of '// &
     &                      'internal control Command stack is too small: '// &
     &                      'REPA__M_COM: '// STR )
                        CALL EXIT ( 111 ) 
                   END IF
!
                   IF ( REP%N_COM .EQ. NCOM_SAVED ) THEN
!
! --------------------- If the point is the first for REP__COM_GRPAMB,
! --------------------- then put there the command name, 
!
                        REP%COM(REP%N_COM)%IND_COM = REP__COM_GRPAMB 
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
                   REP%COM(REP%N_COM)%IND_SBC = REP__COM_SNGAMB
                   REP%COM(REP%N_COM)%IND_BAS = IND_BAS
                   REP%COM(REP%N_COM)%IND_OBS = IND_OBS
                   REP%COM(REP%N_COM)%IVAL    = IAMB
              END IF
 430       CONTINUE 
      END IF
!
      IF ( REP%PLT(IND_BAS)%N_UNR .GT. 0  .AND.  DIAGI_S%NCLR .GE. 3 ) THEN
           DO 440 J4=1,REP%PLT(IND_BAS)%N_UNR
              IND_OBS = REP%PLT(IND_BAS)%IND_UNR(J4)
!
! ----------- Determine the ambiguity spacing which corresponds to the 
! ----------- current point
!
              AMBSP = REPA_GET_AMBSP ( REP, IND_OBS )
!
! ----------- Zero ambiguity spacing means that the operation of 
! ----------- ambiguity shift for this pbservalbe is not defined
!
              IF ( AMBSP .LT. REPA__M_AMBSP ) GOTO 440
              IAMB = NINT( (DIAGI_S%YC - REP%PLT(IND_BAS)%VAL_UNR(J4))/ &
     &                     (AMBSP*REP%PLT(IND_BAS)%VAL_SCL) )
              IF ( IAMB .NE. 0  .AND.  ABS(IAMB) .LT. REPA__M_AMB ) THEN
                   CALL REPA_SNGAMB ( REP, IAMB, IND_BAS, REPA__I_UNR, J4, IND_OBS, &
                                      DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), &
     &                                                 %VAL(DIAGI_S%ADR_Y4(1)), &
     &                                                 %VAL(DIAGI_S%ADR_E4(1)), &
     &                                DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), &
     &                                                 %VAL(DIAGI_S%ADR_Y4(2)), &
     &                                                 %VAL(DIAGI_S%ADR_E4(2)), &
     &                                DIAGI_S%NPOI(3), %VAL(DIAGI_S%ADR_X4(3)), &
     &                                                 %VAL(DIAGI_S%ADR_Y4(3)), &
     &                                                 %VAL(DIAGI_S%ADR_E4(3)), &
     &                                XOLD_R4, YOLD_R4, EOLD_R4, &
     &                                XOLD_R8, YOLD_R8,          &
     &                                YNEW_R4, YNEW_R8, AMBSP )
!
! ---------------- Put the command into the stack
!
                   REP%N_COM = REP%N_COM + 1
                   IF ( REP%N_COM .GT. REPA__M_COM ) THEN
                        CALL CLRCH   ( STR ) 
                        CALL INCH    ( REPA__M_COM, STR ) 
                        CALL ERR_LOG ( 7805, -1, 'REPA_GRPAMB', 'Trap of '// &
     &                      'internal control Command stack is too small: '// &
     &                      'REPA__M_COM: '// STR )
                        CALL EXIT ( 111 ) 
                   END IF
!
                   IF ( REP%N_COM .EQ. NCOM_SAVED ) THEN
!
! --------------------- If the point is the first for REP__COM_GRPAMB,
! --------------------- then put there the command name, 
!
                        REP%COM(REP%N_COM)%IND_COM = REP__COM_GRPAMB 
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
                   REP%COM(REP%N_COM)%IND_SBC = REP__COM_SNGAMB
                   REP%COM(REP%N_COM)%IND_BAS = IND_BAS
                   REP%COM(REP%N_COM)%IND_OBS = IND_OBS
                   REP%COM(REP%N_COM)%IVAL    = IAMB
              END IF
 440       CONTINUE 
      END IF
!
 850  CONTINUE 
      IF ( REP%N_COM .GT. NCOM_SAVED ) THEN
!
! -------- Aga. It turned out that at least one point was changed. Let's
! -------- re-draw the plot
!
           CALL REPA_DF_SETBOX ( DIAGI_S, REP, REP%PLT(IND_BAS)%BOU_IND ) 
      END IF
!
      REPA_DF_GRPAMB = DIAGI__CONT
      RETURN
      END  FUNCTION  REPA_DF_GRPAMB 
