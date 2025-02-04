      SUBROUTINE REPA_SNGAMB ( REP, IAMB, IND_BAS, IND_CLR, IND_PT, IND_OBS, &
     &                         N1, XARR1, YARR1, EARR1,       &
     &                         N2, XARR2, YARR2, EARR2,       &
     &                         N3, XARR3, YARR3, EARR3,       &
     &                         XOLD_R4, YOLD_R4, EOLD_R4, XOLD_R8, YOLD_R8, &
     &                         YNEW_R4, YNEW_R8, AMBSP  )
! ************************************************************************
! *                                                                      *
! *   Routine REPA_SNGAMB updates array YARR1 or YARR2, or YARR3 for     *
! *   the shift at IAMB ambiguity of the point at baseline IND_BAS,      *
! *   category IND_CLR with the index within the category IND_PT.        *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     IAMB ( RECORD    ) -- Object which keeps internal parameters for *
! *  IND_BAS ( INTEGER*4 ) -- baseline index.                            *
! *  IND_CLR ( INTEGER*4 ) -- Caterory index. One of                     *
! *                           REPA__I_GOO -- Good point.                 *
! *                           REPA__I_BAD -- Bad, but recoverable point. *
! *                           REPA__I_UNR -- Bad, unrecoverable point.   *
! *   IND_PT ( INTEGER*4 ) -- point index **within** the categroy.       *
! *  IND_OBS ( INTEGER*4 ) -- Index of the observation in the            *
! *                           experiment.                                *
! *       N1 ( INTEGER*4 ) -- The number of good points.                 *
! *    XARR1 ( REAL*4    ) -- Array of good point arguments.             *
! *                           Dimension: N1.                             *
! *    EARR1 ( REAL*4    ) -- Array of good point errors.                *
! *                           Dimension: N1.                             *
! *       N2 ( INTEGER*4 ) -- The number of bad points.                  *
! *    XARR2 ( REAL*4    ) -- Array of bad point arguments.              *
! *                           Dimension: N2.                             *
! *    EARR2 ( REAL*4    ) -- Array of bad point errors.                 *
! *                           Dimension: N2.                             *
! *       N3 ( INTEGER*4 ) -- The number of bad points.                  *
! *    XARR3 ( REAL*4    ) -- Array of unrecoverable point arguments.    *
! *                           Dimension: N3.                             *
! *    EARR3 ( REAL*4    ) -- Array of unrecoverable point errors.       *
! *                           Dimension: N3.                             *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  XOLD_R4 ( REAL*4    ) -- Argument of the point which ambiguity is   *
! *                           to be changed.                             *
! *  YOLD_R4 ( REAL*4    ) -- The old value of the point before          *
! *                           ambiguity change.                          *
! *  EOLD_R4 ( REAL*4    ) -- The error of the point which ambiguity is  *
! *                           to be changed.                             *
! *  XOLD_R8 ( REAL*8    ) -- Argument of the point which ambiguity is   *
! *                           to be changed.                             *
! *  YOLD_R8 ( REAL*8    ) -- The old value of the point before          *
! *                           ambiguity change.                          *
! *  YNEW_R4 ( REAL*4    ) -- The old value of the point after           *
! *                           ambiguity change.                          *
! *  YNEW_R8 ( REAL*8    ) -- The old value of the point after           *
! *                           ambiguity change.                          *
! *    AMBSP ( REAL*8    ) -- Ambiguity spacing.                         *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
! *    YARR1 ( REAL*4    ) -- Array of good point values.                *
! *                           Dimension: N1.                             *
! *    YARR2 ( REAL*4    ) -- Array of bad point values.                 *
! *                           Dimension: N2.                             *
! *    YARR3 ( REAL*4    ) -- Array of unrecoverable point values.       *
! *                           Dimension: N3.                             *
! *                                                                      *
! *  ### 08-DEC-2004  REPA_SNGAMB  v1.0 (c)  L. Petrov  08-DEC-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'repa.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      TYPE     ( REP__TYPE  ) :: REP
      INTEGER*4  IAMB, IND_BAS, IND_CLR, IND_PT, N1, N2, N3
      REAL*4     XARR1(*), YARR1(*), EARR1(*), &
     &           XARR2(*), YARR2(*), EARR2(*), &
     &           XARR3(*), YARR3(*), EARR3(*), &
     &           XOLD_R4, YOLD_R4, EOLD_R4, YNEW_R4
      CHARACTER  STR*32
      REAL*8     XOLD_R8, YOLD_R8, YNEW_R8, AMBSP
      INTEGER*4  IND_OBS
      LOGICAL*4, EXTERNAL :: PROBE_READ_ADDRESS
      REAL*8,    EXTERNAL :: REPA_GET_AMBSP 
!
! --- Get the index of the observation and store the artument, value and the
! --- error of the point before ambiguity shift
!
      IF ( IND_CLR .EQ. REPA__I_GOO ) THEN
           IND_OBS = REP%PLT(IND_BAS)%IND_GOO(IND_PT) 
           XOLD_R4 = XARR1(IND_PT)
           YOLD_R4 = YARR1(IND_PT)
           EOLD_R4 = EARR1(IND_PT)
           XOLD_R8 = REP%PLT(IND_BAS)%ARG_GOO(IND_PT) 
           YOLD_R8 = REP%PLT(IND_BAS)%VAL_GOO(IND_PT) 
         ELSE IF ( IND_CLR .EQ. REPA__I_BAD ) THEN
           IND_OBS = REP%PLT(IND_BAS)%IND_BAD(IND_PT) 
           XOLD_R4 = XARR2(IND_PT)
           YOLD_R4 = YARR2(IND_PT)
           EOLD_R4 = EARR2(IND_PT)
           XOLD_R8 = REP%PLT(IND_BAS)%ARG_BAD(IND_PT) 
           YOLD_R8 = REP%PLT(IND_BAS)%VAL_BAD(IND_PT) 
         ELSE IF ( IND_CLR .EQ. REPA__I_UNR ) THEN
           IND_OBS = REP%PLT(IND_BAS)%IND_UNR(IND_PT) 
           XOLD_R4 = XARR3(IND_PT)
           YOLD_R4 = YARR3(IND_PT)
           IF ( LOC(EARR3) .NE. 0 ) THEN
                IF ( PROBE_READ_ADDRESS(EARR3) ) THEN
                     EOLD_R4 = EARR3(IND_PT)
                  ELSE 
                     EOLD_R4 = 0.0
                END IF
              ELSE
                EOLD_R4 = 0.0
           END IF
           XOLD_R8 = REP%PLT(IND_BAS)%ARG_UNR(IND_PT) 
           YOLD_R8 = REP%PLT(IND_BAS)%VAL_UNR(IND_PT) 
      END IF
!
! --- Get the value of the ambiguity change
!
      AMBSP = REPA_GET_AMBSP ( REP, IND_OBS ) 
      IF ( AMBSP .GT. REPA__M_AMBSP ) THEN
!
! -------- Get the new value of the point after ambiguity change
!
           YNEW_R4 = YOLD_R4 + IAMB*AMBSP*REP%PLT(IND_BAS)%VAL_SCL 
           YNEW_R8 = YOLD_R8 + IAMB*AMBSP*REP%PLT(IND_BAS)%VAL_SCL 
      END IF
!
! --- Update YARR1 or YARR2 or YARR3 array
!
      IF ( IND_CLR .EQ. REPA__I_GOO ) THEN
           YARR1(IND_PT) = YNEW_R4 
           REP%PLT(IND_BAS)%VAL_GOO(IND_PT) = YNEW_R8 
         ELSE IF ( IND_CLR .EQ. REPA__I_BAD ) THEN
           YARR2(IND_PT) = YNEW_R4 
           REP%PLT(IND_BAS)%VAL_BAD(IND_PT) = YNEW_R8 
         ELSE IF ( IND_CLR .EQ. REPA__I_UNR ) THEN
           YARR3(IND_PT) = YNEW_R4 
           REP%PLT(IND_BAS)%VAL_UNR(IND_PT) = YNEW_R8 
      END IF
!
      RETURN
      END  SUBROUTINE  REPA_SNGAMB 
