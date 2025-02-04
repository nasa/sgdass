      FUNCTION   REPA_DF_SNGAMB ( DIAGI_S, REP, IAMB ) 
! ************************************************************************
! *                                                                      *
! *   Function REPA_DF_SNGAMB changes an ambiguity of a single point     *
! *   at IAMB ambiguity spacings.                                        *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     IAMB ( INTEGER*4 ) -- The number of ambiguity spcacings to be    *
! *                           shifted at.                                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  DIAGI_S ( RECORD    ) -- Object which keeps internal parameters for *
! *                           plotting the current window.               *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
! *                                                                      *
! *  ### 07-DEC-2004  REPA_DF_SNGAMB v1.0 (c) L. Petrov  07-DEC-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  REPA_DF_SNGAMB 
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'repa.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      TYPE     ( REP__TYPE  ) :: REP
      REAL*4     XOLD_R4, YOLD_R4, EOLD_R4, YNEW_R4
      REAL*8     XOLD_R8, YOLD_R8, YNEW_R8, AMBSP
      INTEGER*4  IAMB
      CHARACTER  STR*32
      INTEGER*4  IND_BAS, IND_CLR, IND_PT, IND_OBS, J1, ICLR_OLD 
      PARAMETER  ( ICLR_OLD = 16 )
!
! --- Define light grey color. In fact DiaGI does not define all possible
! --- colors, but only colors for the functions which are used.
!
      CALL PGCOL_RGB ( ITAB_CLR(ICLR_OLD,1),   IRGB_DEF(ICLR_OLD,1,1), &
     &                 IRGB_DEF(ICLR_OLD,1,2), IRGB_DEF(ICLR_OLD,1,3)  )
      CALL PGCOL_RGB ( ITAB_CLR(ICLR_OLD,2),   IRGB_DEF(ICLR_OLD,1,1), &
     &                 IRGB_DEF(ICLR_OLD,1,2), IRGB_DEF(ICLR_OLD,1,3)  )
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
     &     DIAGI_S%NPOI(3), %VAL(DIAGI_S%ADR_X4(3)), %VAL(DIAGI_S%ADR_Y4(3)), &
     &     DIAGI_S%XMIN, DIAGI_S%XMAX, DIAGI_S%YMIN, DIAGI_S%YMAX,            &
     &     IND_CLR, IND_PT )
!
      IF ( IND_PT .GT. 0 ) THEN
!
! -------- Very well. We have found such a point. Now let's move the point
!
           CALL REPA_SNGAMB ( REP, IAMB, IND_BAS, IND_CLR, IND_PT, IND_OBS,   &
                              DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), &
     &                                         %VAL(DIAGI_S%ADR_Y4(1)), &
     &                                         %VAL(DIAGI_S%ADR_E4(1)), &
     &                        DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), &
     &                                         %VAL(DIAGI_S%ADR_Y4(2)), &
     &                                         %VAL(DIAGI_S%ADR_E4(2)), &
     &                        DIAGI_S%NPOI(3), %VAL(DIAGI_S%ADR_X4(3)), &
     &                                         %VAL(DIAGI_S%ADR_Y4(3)), &
     &                                         %VAL(DIAGI_S%ADR_E4(3)), &
     &                        XOLD_R4, YOLD_R4, EOLD_R4, XOLD_R8, YOLD_R8, &
     &                        YNEW_R4, YNEW_R8, AMBSP )
!
! -------- Re-plot the point
!
           IF ( AMBSP .GT. REPA__M_AMBSP ) THEN
!
! ------------- Re-draw the current point with the "old" color (grey)
!
                CALL DIAGI_DRAW ( DIAGI_S, IND_CLR, ICLR_OLD, 1, &
     &                            XOLD_R4, YOLD_R4, EOLD_R4, XOLD_R8, YOLD_R8 )
!                
                CALL REPA_PT_REPLACE ( IND_PT, IND_CLR, &
     &                                 %VAL(DIAGI_S%ADR_Y4(1)),  &
     &                                 %VAL(DIAGI_S%ADR_Y4(2)),  &
     &                                 %VAL(DIAGI_S%ADR_Y4(3)),  &
     &                                 YOLD_R4 )
                CALL REPA_SHOW_MS ( DIAGI_S, REP, IND_BAS, DIAGI_S%NCLR, &
     &                    ICLR_OLD, &
     &                    DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)),         &
     &                    %VAL(DIAGI_S%ADR_Y4(1)), %VAL(DIAGI_S%ADR_E4(1)), &
     &                    DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)),         &
     &                    %VAL(DIAGI_S%ADR_Y4(2)), %VAL(DIAGI_S%ADR_E4(2)), &
     &                    DIAGI_S%NPOI(3), %VAL(DIAGI_S%ADR_X4(3)),         &
     &                    %VAL(DIAGI_S%ADR_Y4(3)), %VAL(DIAGI_S%ADR_E4(3))  )
                CALL REPA_PT_REPLACE ( IND_PT, IND_CLR, &
     &                                 %VAL(DIAGI_S%ADR_Y4(1)),  &
     &                                 %VAL(DIAGI_S%ADR_Y4(2)),  &
     &                                 %VAL(DIAGI_S%ADR_Y4(3)),  &
     &                                 YNEW_R4 )
!
! ------------- Draw the new point
!
                CALL DIAGI_DRAW ( DIAGI_S, IND_CLR, IND_CLR, 1, &
     &                            XOLD_R4, YNEW_R4, EOLD_R4, XOLD_R8, YNEW_R8 )
                CALL REPA_SHOW_MS ( DIAGI_S, REP, IND_BAS, DIAGI_S%NCLR, &
     &                    REP%CNF%MARKED_CLR, &
     &                    DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)),         &
     &                    %VAL(DIAGI_S%ADR_Y4(1)), %VAL(DIAGI_S%ADR_E4(1)), &
     &                    DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)),         &
     &                    %VAL(DIAGI_S%ADR_Y4(2)), %VAL(DIAGI_S%ADR_E4(2)), &
     &                    DIAGI_S%NPOI(3), %VAL(DIAGI_S%ADR_X4(3)),         &
     &                    %VAL(DIAGI_S%ADR_Y4(3)), %VAL(DIAGI_S%ADR_E4(3))  )
!
! ------------- Put the command into the stack
!
                REP%N_COM = REP%N_COM + 1
                IF ( REP%N_COM .GT. REPA__M_COM ) THEN
                     CALL CLRCH   ( STR ) 
                     CALL INCH    ( REPA__M_COM, STR ) 
                     CALL ERR_LOG ( 7802, -1, 'REPA_SNGAMB', 'Trap of '// &
     &                   'internal control Command stack is too small: '// &
     &                   'REPA__M_COM: '// STR )
                     CALL EXIT ( 111 ) 
                END IF
                REP%COM(REP%N_COM)%IND_COM = REP__COM_SNGAMB
                REP%COM(REP%N_COM)%IND_SBC = REP__COM_SNGAMB
                REP%COM(REP%N_COM)%IND_BAS = IND_BAS
                REP%COM(REP%N_COM)%IND_OBS = IND_OBS
                REP%COM(REP%N_COM)%IVAL    = IAMB
           END IF      
      END IF
!
      REPA_DF_SNGAMB = DIAGI__CONT
      RETURN
      END  FUNCTION  REPA_DF_SNGAMB 
!
! ------------------------------------------------------------------------
!
      SUBROUTINE REPA_PT_REPLACE ( IND_PT, IND_CLR, YARR1, YARR2, YARR3, YPT )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine  REPA_PT_REPLACE                                *
! *                                                                      *
! * ### 15-JUN-2006 REPA_PT_REPLACE  v1.0 (c) L. Petrov 15-JUN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'repa.i'
      INTEGER*4  IND_PT, IND_CLR, NPT
      REAL*4     YARR1(*), YARR2(*), YARR3(*), YPT 
      IF ( IND_CLR .EQ. REPA__I_GOO ) THEN
           YARR1(IND_PT) = YPT
         ELSE IF ( IND_CLR .EQ. REPA__I_BAD ) THEN
           YARR2(IND_PT) = YPT
         ELSE IF ( IND_CLR .EQ. REPA__I_UNR ) THEN
           YARR3(IND_PT) = YPT 
      END IF
      RETURN
      END  !#!  
