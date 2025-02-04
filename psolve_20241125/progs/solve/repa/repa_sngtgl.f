      SUBROUTINE REPA_SNGTGL ( REP, IND_BAS, IND_CLR, IND_PT, &
     &                         N1, XARR1, YARR1, EARR1,       &
     &                         N2, XARR2, YARR2, EARR2, &
     &                         XPT_R4, YPT_R4, EPT_R4, XPT_R8, YPT_R8, &
     &                         IND_OBS )
! ************************************************************************
! *                                                                      *
! *   Routine REPA_SNGTGL toggles suppression status of the point at     *
! *   baseline IND_BAS, category IND_CLR with the index within the       *
! *   category IND_PT. IF the point was good it removes it from the list *
! *   of good points and inserts in the list of bad points. Vise versa,  *
! *   if the point was bad, it removes it from the list of bad points    *
! *   and inserts it in the list of good points.                         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
! *  IND_BAS ( INTEGER*4 ) -- baseline index.                            *
! *  IND_CLR ( INTEGER*4 ) -- Caterory index. One of                     *
! *                           REPA__I_GOO -- Good point.                 *
! *                           REPA__I_BAD -- Bad, but recoverable point. *
! *                           REPA__I_UNR -- Bad, unrecoverable point.   *
! *   IND_PT ( INTEGER*4 ) -- point index **within** the categroy.       *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   XPT_R4 ( REAL*4    ) -- Argument of the point of interest.         *
! *   YPT_R4 ( REAL*4    ) -- Value of the point of interest.            *
! *   EPT_R4 ( REAL*4    ) -- Error of the point of interest.            *
! *   XPT_R8 ( REAL*8    ) -- Argument of the point of interest.         *
! *   YPT_R8 ( REAL*8    ) -- Error of the point of interest.            *
! *  IND_OBS ( INTEGER*4 ) -- Index of the observation in the            *
! *                           experiment.                                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       N1 ( INTEGER*4 ) -- The number of good points.                 *
! *    XARR1 ( REAL*4    ) -- Array of good point arguments.             *
! *                           Dimension: N1.                             *
! *    YARR1 ( REAL*4    ) -- Array of good point values.                *
! *                           Dimension: N1.                             *
! *    EARR1 ( REAL*4    ) -- Array of good point errors.                *
! *                           Dimension: N1.                             *
! *       N2 ( INTEGER*4 ) -- The number of bad points.                  *
! *    XARR2 ( REAL*4    ) -- Array of bad point arguments.              *
! *                           Dimension: N2.                             *
! *    YARR2 ( REAL*4    ) -- Array of bad point values.                 *
! *                           Dimension: N2.                             *
! *    EARR2 ( REAL*4    ) -- Array of bad point errors.                 *
! *                           Dimension: N2.                             *
! *                                                                      *
! *  ### 08-DEC-2004  REPA_SNGTGL  v1.0 (c)  L. Petrov  08-DEC-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  REPA_DF_SNGTGL
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'repa.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      TYPE     ( REP__TYPE  ) :: REP
      INTEGER*4  IND_BAS, IND_CLR, IND_PT, N1, N2, N3, IND_OBS
      REAL*4     XARR1(*), YARR1(*), EARR1(*), &
     &           XARR2(*), YARR2(*), EARR2(*), &
     &           XPT_R4, YPT_R4, EPT_R4  
      REAL*8     XPT_R8, YPT_R8
      REAL*4     XVAL_OLD, YVAL_OLD, EVAL_OLD
      REAL*8     ARG_OLD, VAL_OLD, ERR_OLD
      INTEGER*4  J1, J2, NP, KP
!
      IF ( IND_CLR .EQ. REPA__I_GOO ) THEN
!
! -------- It was suppression of the good point. Store arguemnt, value, 
! -------- error and index of the point
!
           NP = N1
           KP = N2
           XVAL_OLD = XARR1(IND_PT)
           YVAL_OLD = YARR1(IND_PT)
           EVAL_OLD = EARR1(IND_PT)
           ARG_OLD  = REP%PLT(IND_BAS)%ARG_GOO(IND_PT) 
           VAL_OLD  = REP%PLT(IND_BAS)%VAL_GOO(IND_PT) 
           ERR_OLD  = REP%PLT(IND_BAS)%ERR_GOO(IND_PT) 
           IND_OBS  = REP%PLT(IND_BAS)%IND_GOO(IND_PT) 
        ELSE IF ( IND_CLR .EQ. REPA__I_BAD ) THEN
!
! -------- It was resurrection of the bad point. Store arguemnt, value, 
! -------- error and index of the point
!
           NP = N2
           KP = N1
           XVAL_OLD = XARR2(IND_PT)
           YVAL_OLD = YARR2(IND_PT)
           EVAL_OLD = EARR2(IND_PT)
           ARG_OLD  = REP%PLT(IND_BAS)%ARG_BAD(IND_PT) 
           VAL_OLD  = REP%PLT(IND_BAS)%VAL_BAD(IND_PT) 
           ERR_OLD  = REP%PLT(IND_BAS)%ERR_BAD(IND_PT) 
           IND_OBS  = REP%PLT(IND_BAS)%IND_BAD(IND_PT) 
      END IF
!
      IF ( IND_PT .LT. NP ) THEN
!
! -------- Remove the point from the middle of the list. We move to the left
! -------- all points with the arguemnt greater than the point of interest
!
           DO 410 J1=IND_PT,NP-1
              IF ( IND_CLR .EQ. REPA__I_GOO ) THEN
                   XARR1(J1) = XARR1(J1+1)
                   YARR1(J1) = YARR1(J1+1)
                   EARR1(J1) = EARR1(J1+1)
                   REP%PLT(IND_BAS)%ARG_GOO(J1) = REP%PLT(IND_BAS)%ARG_GOO(J1+1) 
                   REP%PLT(IND_BAS)%VAL_GOO(J1) = REP%PLT(IND_BAS)%VAL_GOO(J1+1) 
                   REP%PLT(IND_BAS)%ERR_GOO(J1) = REP%PLT(IND_BAS)%ERR_GOO(J1+1) 
                   REP%PLT(IND_BAS)%IND_GOO(J1) = REP%PLT(IND_BAS)%IND_GOO(J1+1) 
                ELSE IF ( IND_CLR .EQ. REPA__I_BAD ) THEN
                   XARR2(J1) = XARR2(J1+1)
                   YARR2(J1) = YARR2(J1+1)
                   EARR2(J1) = EARR2(J1+1)
                   REP%PLT(IND_BAS)%ARG_BAD(J1) = REP%PLT(IND_BAS)%ARG_BAD(J1+1) 
                   REP%PLT(IND_BAS)%VAL_BAD(J1) = REP%PLT(IND_BAS)%VAL_BAD(J1+1) 
                   REP%PLT(IND_BAS)%ERR_BAD(J1) = REP%PLT(IND_BAS)%ERR_BAD(J1+1) 
                   REP%PLT(IND_BAS)%IND_BAD(J1) = REP%PLT(IND_BAS)%IND_BAD(J1+1) 
               END IF
 410       CONTINUE 
      END IF
!
! --- Insert the point from the middle of the list. We move to the right
! --- all points with the arguemnt greater than the point of interest
!
      IF ( KP .GT. 0 ) THEN
           DO 420 J2=KP,1,-1
              IF ( IND_CLR .EQ. REPA__I_GOO ) THEN
!
! ---------------- It is suppression of the former good point
!
                   IF ( XVAL_OLD .GT. XARR2(J2)  .AND.  J2 .NE. KP )  THEN
!
! --------------------- Move the point to the right
!
                        XARR2(J2+1) = XARR2(J2)
                        YARR2(J2+1) = YARR2(J2)
                        EARR2(J2+1) = EARR2(J2)
                        REP%PLT(IND_BAS)%ARG_BAD(J2+1) = REP%PLT(IND_BAS)%ARG_BAD(J2) 
                        REP%PLT(IND_BAS)%VAL_BAD(J2+1) = REP%PLT(IND_BAS)%VAL_BAD(J2) 
                        REP%PLT(IND_BAS)%ERR_BAD(J2+1) = REP%PLT(IND_BAS)%ERR_BAD(J2) 
                        REP%PLT(IND_BAS)%IND_BAD(J2+1) = REP%PLT(IND_BAS)%IND_BAD(J2) 
                      ELSE 
!
! --------------------- Insert the point
!
                        XARR2(J2+1) = XVAL_OLD
                        YARR2(J2+1) = YVAL_OLD
                        EARR2(J2+1) = EVAL_OLD
                        REP%PLT(IND_BAS)%ARG_BAD(J2+1) = ARG_OLD
                        REP%PLT(IND_BAS)%VAL_BAD(J2+1) = VAL_OLD
                        REP%PLT(IND_BAS)%ERR_BAD(J2+1) = ERR_OLD
                        REP%PLT(IND_BAS)%IND_BAD(J2+1) = IND_OBS
                        GOTO 820
                    END IF
                 ELSE IF ( IND_CLR .EQ. REPA__I_BAD ) THEN
!
! ----------------- It is resurrection of the former bad point
!
                   IF ( XVAL_OLD .GT. XARR1(J2)  .AND.  J2 .NE. KP ) THEN
!
! --------------------- Move the point to the right
!
                        XARR1(J2+1) = XARR1(J2)
                        YARR1(J2+1) = YARR1(J2)
                        EARR1(J2+1) = EARR1(J2)
                        REP%PLT(IND_BAS)%ARG_GOO(J2+1) = REP%PLT(IND_BAS)%ARG_GOO(J2) 
                        REP%PLT(IND_BAS)%VAL_GOO(J2+1) = REP%PLT(IND_BAS)%VAL_GOO(J2) 
                        REP%PLT(IND_BAS)%ERR_GOO(J2+1) = REP%PLT(IND_BAS)%ERR_GOO(J2) 
                        REP%PLT(IND_BAS)%IND_GOO(J2+1) = REP%PLT(IND_BAS)%IND_GOO(J2) 
                      ELSE 
!
! --------------------- Insert the point
!
                        XARR1(J2+1) = XVAL_OLD
                        YARR1(J2+1) = YVAL_OLD
                        EARR1(J2+1) = EVAL_OLD
                        REP%PLT(IND_BAS)%ARG_GOO(J2+1) = ARG_OLD
                        REP%PLT(IND_BAS)%VAL_GOO(J2+1) = VAL_OLD
                        REP%PLT(IND_BAS)%ERR_GOO(J2+1) = ERR_OLD
                        REP%PLT(IND_BAS)%IND_GOO(J2+1) = IND_OBS
                        GOTO 820
                   END IF
              END IF         
 420       CONTINUE 
 820       CONTINUE 
         ELSE ! i.e. KP == 0
           IF ( IND_CLR .EQ. REPA__I_GOO ) THEN
!
! ------------- This means N2 was 0
!
                XARR2(1) = XVAL_OLD
                YARR2(1) = YVAL_OLD
                EARR2(1) = EVAL_OLD
                REP%PLT(IND_BAS)%ARG_BAD(1) = ARG_OLD
                REP%PLT(IND_BAS)%VAL_BAD(1) = VAL_OLD
                REP%PLT(IND_BAS)%ERR_BAD(1) = ERR_OLD
                REP%PLT(IND_BAS)%IND_BAD(1) = IND_OBS
              ELSE IF ( IND_CLR .EQ. REPA__I_BAD ) THEN
!
! ------------- This means N1 was 0
!
                XARR1(1) = XVAL_OLD
                YARR1(1) = YVAL_OLD
                EARR1(1) = EVAL_OLD
                REP%PLT(IND_BAS)%ARG_GOO(1) = ARG_OLD
                REP%PLT(IND_BAS)%VAL_GOO(1) = VAL_OLD
                REP%PLT(IND_BAS)%ERR_GOO(1) = ERR_OLD
                REP%PLT(IND_BAS)%IND_GOO(1) = IND_OBS
           END IF
      END IF 
!
      IF ( IND_CLR .EQ. REPA__I_GOO ) THEN
!
! -------- It is suppression of the former good point.
! -------- Update counters
!
           N1 = N1 - 1
           N2 = N2 + 1
           REP%PLT(IND_BAS)%N_GOO  = REP%PLT(IND_BAS)%N_GOO  - 1
           REP%PLT(IND_BAS)%N_BAD  = REP%PLT(IND_BAS)%N_BAD  + 1
           REP%LIS%KG_BAS(IND_BAS) = REP%LIS%KG_BAS(IND_BAS) - 1
           REP%LIS%KB_BAS(IND_BAS) = REP%LIS%KB_BAS(IND_BAS) + 1
         ELSE IF ( IND_CLR .EQ. REPA__I_BAD ) THEN
!
! -------- It is resurrection of the former bad point.
! -------- Update counters
!
           N1 = N1 + 1
           N2 = N2 - 1
           REP%PLT(IND_BAS)%N_GOO  = REP%PLT(IND_BAS)%N_GOO  + 1
           REP%PLT(IND_BAS)%N_BAD  = REP%PLT(IND_BAS)%N_BAD  - 1
           REP%LIS%KG_BAS(IND_BAS) = REP%LIS%KG_BAS(IND_BAS) + 1
           REP%LIS%KB_BAS(IND_BAS) = REP%LIS%KB_BAS(IND_BAS) - 1
      END IF
!
! --- Save the old values
!
      XPT_R4 = XVAL_OLD
      YPT_R4 = YVAL_OLD
      EPT_R4 = EVAL_OLD
      XPT_R8 = ARG_OLD
      YPT_R8 = VAL_OLD
!
      RETURN
      END  SUBROUTINE  REPA_SNGTGL 
