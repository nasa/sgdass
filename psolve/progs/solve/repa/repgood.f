      FUNCTION REPGOOD ( DIAGI_S )
! ************************************************************************
! *                                                                      *
! *   Routine REAPGOOD changes maximal and minimal value of the first    *
! *   function and re-draws the plot.                                    *
! *                                                                      *
! *  ### 14-MAY-2003    REPGOOD   v1.0 (c)  L. Petrov  14-MAY-2003  ###  *
! *                                                                      *
! *  HISTORY:                                                            *
! *  2003.06.05   VT   increased the space at edges of plotting area     * 
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      INTEGER*4  REPGOOD
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      REAL*4     YMIN_V, YMAX_V
      INTEGER*4  J1
!
!!        call pause ( 'repgood' ) ! %%%
      IF ( DIAGI_S%NPOI(1) .LE. 1 ) THEN
           REPGOOD = 1
           RETURN
      END IF
      CALL DIAGI_MINMAX ( DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_Y4(1)), &
     &                    DIAGI_S%LER(1),  %VAL(DIAGI_S%ADR_E4(1)), &
     &                    YMIN_V, YMAX_V )
      DIAGI_S%YMIN = YMIN_V - ( YMAX_V - YMIN_V ) / 50.0
      DIAGI_S%YMAX = YMAX_V + ( YMAX_V - YMIN_V ) / 50.0
!
      CALL DIAGI_SET_FRAME ( DIAGI_S, DIAGI_S%MESS_BOT )
!
! --- drawing plots of the functions, colour by colour.
! --- The next colour overlap the previous one.
!
      DO 410 J1=1,DIAGI_S%NCLR
         CALL DIAGI_DRAW ( DIAGI_S, J1, 0, &
     &        DIAGI_S%NPOI(J1), %VAL(DIAGI_S%ADR_X4(J1)), &
     &        %VAL(DIAGI_S%ADR_Y4(J1)), %VAL(DIAGI_S%ADR_E4(J1)), &
     &        %VAL(DIAGI_S%ADR_X8(J1)), %VAL(DIAGI_S%ADR_Y8(J1)) )
 410  CONTINUE
!
      REPGOOD = 1
      RETURN
      END  !#!  REPGOOD  #!#
