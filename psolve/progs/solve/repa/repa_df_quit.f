      FUNCTION   REPA_DF_QUIT ( DIAGI_S, REP, MODE ) 
! ************************************************************************
! *                                                                      *
! *   Function REPA_DF_QUIT is the quit-function called by REPA. This    *
! *   function is called when a user hits the key which causes DiaGi     *
! *   to quit plotting mode.                                             *
! *                                                                      *
! * ### 07-DEC-2004    REPA_DF_QUIT   v1.0 (c) L. Petrov 07-DEC-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  REPA_DF_QUIT
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'repa.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      TYPE     ( REP__TYPE  ) :: REP
      INTEGER*4  MODE
      INTEGER*4  IND_BAS, J1
!
! --- Determine the index of the baseline which corresponds to the current
! --- window
!
      IND_BAS = 0
      DO 410 J1=1,REP%N_BAS
         IF ( LOC(REP%DIAGI(J1)) .EQ. LOC(DIAGI_S) ) IND_BAS = J1
 410  CONTINUE 
!
      IF ( MODE == DIAGI__CONT ) THEN 
!
! -------- [X] or [Q]
!
           REP%CNF%BASELINE = 'ALL             '
           DIAGI_S%MD_OUT = DIAGI__CONT
           CALL REPA_COM ( REP )
           REP%STATUS = REPA__QUIT 
         ELSE IF ( MODE == DIAGI__KEEP ) THEN 
!
! -------- [HOME]
!
           REP%CNF%BASELINE = REP%LIS%C_BAS(IND_BAS)
           DIAGI_S%MD_OUT = DIAGI__QUIT
           CALL REPA_COM ( REP )
           REP%STATUS = REPA__QUIT 
         ELSE IF ( MODE == DIAGI__QUIT ) THEN 
!
! -------- [F10]
!
           REP%CNF%BASELINE = REP%LIS%C_BAS(IND_BAS)
           DIAGI_S%MD_OUT = DIAGI__QUIT
           REP%N_COM = 0
           REP%STATUS = REPA__QUIT 
      END IF
!
      REPA_DF_QUIT = DIAGI__QUIT   
      RETURN
      END  FUNCTION  REPA_DF_QUIT
