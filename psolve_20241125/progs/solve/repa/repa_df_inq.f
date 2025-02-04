      FUNCTION   REPA_DF_INQ ( DIAGI_S, REP ) 
! ************************************************************************
! *                                                                      *
! *   Function REPA_DF_INQ inquires the information about the current    *
! *   point. It updates the bottom label and put there information       *
! *   about the argument and the value of the point which is the closest *
! *   to the cursor. It also indicates the point by pringin a small      *
! *   blue asterisk on the point.                                        *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  DIAGI_S ( RECORD    ) -- Object which keeps internal parameters for *
! *                           plotting the current window.               *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
! *                                                                      *
! * ### 07-DEC-2004    REPA_DF_INQ   v1.0 (c) L. Petrov 07-DEC-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  REPA_DF_INQ
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'repa.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      TYPE     ( REP__TYPE  ) :: REP
      INTEGER*4  J1, IND_BAS, IND_PT, IND_CLR
!
! --- Determine the index of the baseline which corresponds to the current
! --- window
!
      IND_BAS = 0
      DO 410 J1=1,REP%N_BAS
         IF ( LOC(REP%DIAGI(J1)) .EQ. LOC(DIAGI_S) ) IND_BAS = J1
 410  CONTINUE 
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
! --- Store the status of the inqury
!
      DIAGI_S%IPQ = IND_PT
      DIAGI_S%ICQ = IND_CLR
      REPA_DF_INQ = DIAGI__CONT   
!
      RETURN
      END  FUNCTION  REPA_DF_INQ
