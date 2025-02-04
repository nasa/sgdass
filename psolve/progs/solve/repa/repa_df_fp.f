      FUNCTION   REPA_DF_FP ( DIAGI_S, REP, MODE ) 
! ************************************************************************
! *                                                                      *
! *   Function REPA_DF_FP 
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     MODE ( INTEGER*4 ) -- Mode.
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  DIAGI_S ( RECORD    ) -- Object which keeps internal parameters for *
! *                           plotting the current window.               *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
! *                                                                      *
! * ### 28-JAN-2005    REPA_DF_FP    v1.1 (c) L. Petrov 03-SEP-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  REPA_DF_FP
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'repa.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      TYPE     ( REP__TYPE  ) :: REP
      INTEGER*4  MODE
      CHARACTER  FRINGE_FILE*128
      LOGICAL*4  LEX
      INTEGER*4  J1, IND_BAS, IND_PT, IND_CLR, IND_OBS, IER
      ADDRESS__TYPE :: DIR_DESC
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      ADDRESS__TYPE, EXTERNAL :: OPENDIR 
!
! --- Determine the index of the baseline which corresponds to the current
! --- window
!
      IND_BAS = 0
      DO 410 J1=1,REP%N_BAS
         IF ( LOC(REP%DIAGI(J1)) .EQ. LOC(DIAGI_S) ) IND_BAS = J1
 410  CONTINUE 
!
      IF ( ILEN(REP%FRINGE_ROOT_DIR) .EQ. 0 ) THEN
           WRITE ( 6, '(A)' ) 'Fringe root file is not defined. Use "?" in CRES'
           REPA_DF_FP = DIAGI__CONT   
           RETURN 
      END IF
!
! --- Check does the PostScript viewer exists
!
      INQUIRE ( FILE=REP%SOLVE_PS_VIEWER, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           WRITE ( 6, '(A)' ) 'PostScript viewer '// &
     &                  REP%SOLVE_PS_VIEWER(1:I_LEN(REP%SOLVE_PS_VIEWER))// &
     &                 ' was not found '
           REPA_DF_FP = DIAGI__CONT   
           RETURN 
      END IF
!
      FRINGE_FILE = REP%FRINGE_ROOT_DIR(1:I_LEN(REP%FRINGE_ROOT_DIR))// &
     &              REP%EXPSERNO_STR(1:I_LEN(REP%EXPSERNO_STR))//'/'
!
! --- Check: does work directory exist
!
      DIR_DESC = OPENDIR ( FRINGE_FILE(1:I_LEN(FRINGE_FILE))//CHAR(0) )
      IF ( DIR_DESC .EQ. 0 ) THEN
!
! -------- If directory does not exist we remove the subdirectory with 
! -------- the experiment serial number
!
           FRINGE_FILE = REP%FRINGE_ROOT_DIR(1:I_LEN(REP%FRINGE_ROOT_DIR))
         ELSE 
           CALL CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
! --- Make point inquiry
!
      CALL REPA_DOINQ ( DIAGI_S, REP, &
     &                  DIAGI_S%XC, DIAGI_S%YC, IND_BAS, DIAGI_S%NCLR, &
     &                  DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)),         &
     &                  %VAL(DIAGI_S%ADR_Y4(1)), %VAL(DIAGI_S%ADR_E4(1)), &
     &                  DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)),         &
     &                  %VAL(DIAGI_S%ADR_Y4(2)), %VAL(DIAGI_S%ADR_E4(2)), &
     &                  DIAGI_S%NPOI(3), %VAL(DIAGI_S%ADR_X4(3)),         &
     &                  %VAL(DIAGI_S%ADR_Y4(3)), %VAL(DIAGI_S%ADR_E4(3)), &
     &                  IND_PT, IND_CLR )
!
      IF ( IND_CLR == REPA__I_GOO ) THEN
           IND_OBS = REP%PLT(IND_BAS)%IND_GOO(IND_PT)
        ELSE IF ( IND_CLR == REPA__I_BAD ) THEN
           IND_OBS = REP%PLT(IND_BAS)%IND_BAD(IND_PT)
        ELSE IF ( IND_CLR == REPA__I_UNR ) THEN
           IND_OBS = REP%PLT(IND_BAS)%IND_UNR(IND_PT)
      END IF
!
      IF ( MODE .EQ. 1 ) THEN
           IF ( ILEN(REP%OBS(IND_OBS)%FRINGE_X_FINAM) .EQ. 0 ) THEN
                WRITE ( 6, * ) 'No Fringe file name is in the database for '// &
     &                         ' X-band observation #', IND_OBS
                REPA_DF_FP = DIAGI__CONT   
                RETURN 
           END IF
           FRINGE_FILE = FRINGE_FILE(1:I_LEN(FRINGE_FILE))// &
     &            REP%OBS(IND_OBS)%SCAN_NAME(1:I_LEN(REP%OBS(IND_OBS)%SCAN_NAME))//'/'// &
     &            REP%OBS(IND_OBS)%FRINGE_X_FINAM
         ELSE IF ( MODE .EQ. 2 ) THEN
           IF ( ILEN(REP%OBS(IND_OBS)%FRINGE_S_FINAM) .EQ. 0 ) THEN
                WRITE ( 6, * ) 'No Fringe file name is in the database for '// &
     &                         ' S-band observation #', IND_OBS
                REPA_DF_FP = DIAGI__CONT   
                RETURN 
           END IF
           FRINGE_FILE = FRINGE_FILE(1:I_LEN(FRINGE_FILE))// &
     &            REP%OBS(IND_OBS)%SCAN_NAME(1:I_LEN(REP%OBS(IND_OBS)%SCAN_NAME))//'/'// &
     &            REP%OBS(IND_OBS)%FRINGE_S_FINAM 
      END IF
!
      CALL FLUSH ( 6 ) 
      IER = -1
      CALL MK4_PLOT ( FRINGE_FILE, REP%SOLVE_PS_VIEWER, IER )
      CALL FLUSH ( 6 ) 
!
! --- Store the status of the inqury
!
      DIAGI_S%IPQ = IND_PT
      DIAGI_S%ICQ = IND_CLR
      REPA_DF_FP = DIAGI__CONT   
!
      RETURN
      END  FUNCTION  REPA_DF_FP
