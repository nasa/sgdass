      SUBROUTINE PLOT_EOP_FCS ( MODE, ICMP, MP, &
     &                          NC, TIM_C, EOP_C, &
     &                          NR, TIM_R, EOP_R, &
     &                          NA, TIM_A, EOP_A, &
     &                          NM, TIM_M, EOP_M, EOP_SPL_M, &
     &                          NK, ARG_SPL, L_PAR, BSPL_EOP, &
     &                          NF, TIM_F, EOP_F, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PLOT_EOP_FCS
! *                                                                      *
! *  ### 30-MAR-2016 PLOT_EOP_FCS   v2.0 (c)  L. Petrov 22-MAY-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'diagi.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      INTEGER*4  MODE, ICMP, MP, IUER
      INTEGER*4  NC, NR, NA, NM, NF, NK, L_PAR
      REAL*8     TIM_C(MP), EOP_C(MP,2), &
     &           TIM_R(NM), EOP_R(MP,2), &
     &           TIM_A(MP), EOP_A(MP,2), &
     &           TIM_M(NM), EOP_M(NM,2), EOP_SPL_M(NM,2), &
     &           ARG_SPL(NK), BSPL_EOP(L_PAR,2), &
     &           TIM_F(NF), EOP_F(NF)
      CHARACTER  ZAG*128, UNIT*128, STR*128
      REAL*8     TIM_EPS
      PARAMETER  ( TIM_EPS = 100.0D0 )
      INTEGER*4    DEG
      PARAMETER  ( DEG = 3 )
      REAL*8,    ALLOCATABLE :: TC(:), TR(:), TA(:), TF(:), TM(:)
      REAL*8,    ALLOCATABLE :: EC(:), ER(:), EA(:), EF(:), EM(:)
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, &
     &           ICL1, ICL2, ICL3, DIAGI_LEN, IER
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, IK
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_DER, BSPL_DR2, FSPL8, DSPL8, D2SPL8
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8 
!
! --- Clear DIAGI_S object
!
      DIAGI_LEN = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4
      CALL NOUT ( DIAGI_LEN, DIAGI_S )
!
! --- Setting defaults values of the plotting parameters
!
      CALL ERR_PASS ( IUER, IER )
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, UNIT, &
     &                  ICL1, ICL2, ICL3, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6631, IUER, 'PLOT_EOP_FCS', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN 
      END IF
!
      ALLOCATE ( EM(NM) )
      ALLOCATE ( EF(NF) )
      ALLOCATE ( EC(NC) )
      ALLOCATE ( ER(NR) )
      ALLOCATE ( EA(NA) )
!
      ALLOCATE ( TM(NM) )
      ALLOCATE ( TF(NF) )
      ALLOCATE ( TC(NC) )
      ALLOCATE ( TR(NR) )
      ALLOCATE ( TA(NA) )
!
      DO 410 J1=1,NM
         IF ( J1 == 1 ) THEN
              IK = IXMN8 ( NM, TIM_M, TIM_M(1) + TIM_EPS )
            ELSE IF ( J1 == NM ) THEN
              IK = IXMN8 ( NM, TIM_M, TIM_M(NM) - TIM_EPS )
            ELSE
              IK = IXMN8 ( NM, TIM_M, TIM_M(J1) )
         END IF
         IF ( ICMP == 1 ) THEN
              EM(J1) = FSPL8  ( TIM_M(J1), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
           ELSE IF ( ICMP == 2 ) THEN
              EM(J1) = FSPL8  ( TIM_M(J1), NM, TIM_M, EOP_M(1,2), IK, EOP_SPL_M(1,2) ) 
           ELSE IF ( ICMP == 3 ) THEN
              EM(J1) = FSPL8  ( TIM_M(J1), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
           ELSE IF ( ICMP == 4 ) THEN
              EM(J1) = DSPL8  ( TIM_M(J1), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
           ELSE IF ( ICMP == 5 ) THEN
              EM(J1) = DSPL8  ( TIM_M(J1), NM, TIM_M, EOP_M(1,2), IK, EOP_SPL_M(1,2) ) 
           ELSE IF ( ICMP == 6 ) THEN
              EM(J1) = DSPL8  ( TIM_M(J1), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
           ELSE IF ( ICMP == 7 ) THEN
              EM(J1) = D2SPL8 ( TIM_M(J1), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
           ELSE IF ( ICMP == 8 ) THEN
              EM(J1) = D2SPL8 ( TIM_M(J1), NM, TIM_M, EOP_M(1,2), IK, EOP_SPL_M(1,2) ) 
           ELSE IF ( ICMP == 9 ) THEN
              EM(J1) = D2SPL8 ( TIM_M(J1), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
         END IF
         TM(J1) = (TIM_M(J1) - TIM_A(NA))/86400.0D0
 410  CONTINUE 
      DO 420 J2=1,NF
         IF ( J2 == 1 ) THEN
              IK = IXMN8 ( NK, ARG_SPL, TIM_F(1) + TIM_EPS )
            ELSE IF ( J2 == NF ) THEN
              IK = IXMN8 ( NK, ARG_SPL, TIM_F(NF) - TIM_EPS )
            ELSE
              IK = IXMN8 ( NK, ARG_SPL, TIM_F(J2) )
         END IF
!
         EF(J2) = 0.0D0
         DO 430 J3=-DEG,0
            IF ( ICMP == 1 ) THEN
                 EF(J2) = EF(J2) + BSPL_EOP(IK+DEG+J3,1)* &
     &                             BSPL_VAL ( NK, ARG_SPL, DEG, IK+J3, TIM_F(J2) )
               ELSE IF ( ICMP == 2 ) THEN
                 EF(J2) = EF(J2) + BSPL_EOP(IK+DEG+J3,2)* &
     &                             BSPL_VAL ( NK, ARG_SPL, DEG, IK+J3, TIM_F(J2) )
               ELSE IF ( ICMP == 3 ) THEN
                 EF(J2) = EF(J2) + BSPL_EOP(IK+DEG+J3,1)* &
     &                             BSPL_VAL ( NK, ARG_SPL, DEG, IK+J3, TIM_F(J2) )
               ELSE IF ( ICMP == 4 ) THEN
                 EF(J2) = EF(J2) + BSPL_EOP(IK+DEG+J3,1)* &
     &                             BSPL_DER ( NK, ARG_SPL, DEG, IK+J3, TIM_F(J2) )
               ELSE IF ( ICMP == 5 ) THEN
                 EF(J2) = EF(J2) + BSPL_EOP(IK+DEG+J3,2)* &
     &                             BSPL_DER ( NK, ARG_SPL, DEG, IK+J3, TIM_F(J2) )
               ELSE IF ( ICMP == 6 ) THEN
                 EF(J2) = EF(J2) + BSPL_EOP(IK+DEG+J3,1)* &
     &                             BSPL_DER ( NK, ARG_SPL, DEG, IK+J3, TIM_F(J2) )
               ELSE IF ( ICMP == 7 ) THEN
                 EF(J2) = EF(J2) + BSPL_EOP(IK+DEG+J3,1)* &
     &                             BSPL_DR2 ( NK, ARG_SPL, DEG, IK+J3, TIM_F(J2) )
               ELSE IF ( ICMP == 8 ) THEN
                 EF(J2) = EF(J2) + BSPL_EOP(IK+DEG+J3,2)* &
     &                             BSPL_DR2 ( NK, ARG_SPL, DEG, IK+J3, TIM_F(J2) )
               ELSE IF ( ICMP == 9 ) THEN
                 EF(J2) = EF(J2) + BSPL_EOP(IK+DEG+J3,1)* &
     &                             BSPL_DR2 ( NK, ARG_SPL, DEG, IK+J3, TIM_F(J2) )
            END IF
            TF(J2) = (TIM_F(J2) - TIM_A(NA))/86400.0D0
 430     CONTINUE 
!!   write ( 6 , * ) ' j2= ', int2(j2), ' ik= ', ik, ' tim_f(j2) ', tim_f(j2), ' arg_spl(1) = ', arg_spl(1), ' ef= ', ef(j2) ! %%%%%%%%%%%%%%%%%
 420  CONTINUE 
!
      IF ( MODE == 2 ) THEN
           DO 440 J4=1,NC
              IF ( J4 == 1 ) THEN
                   IK = IXMN8 ( NM, TIM_M, TIM_C(1) + TIM_EPS )
                 ELSE IF ( J4 == NC ) THEN
                   IK = IXMN8 ( NM, TIM_M, TIM_C(NC) - TIM_EPS )
                 ELSE
                   IK = IXMN8 ( NM, TIM_M, TIM_C(J4) )
              END IF
              IF ( ICMP == 1 ) THEN
                   EC(J4) = EOP_C(J4,1) - FSPL8  ( TIM_C(J4), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 2 ) THEN
                   EC(J4) = EOP_C(J4,2) - FSPL8  ( TIM_C(J4), NM, TIM_M, EOP_M(1,2), IK, EOP_SPL_M(1,2) ) 
                 ELSE IF ( ICMP == 3 ) THEN
                   EC(J4) = EOP_C(J4,1) - FSPL8  ( TIM_C(J4), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 4 ) THEN
                   EC(J4) = EOP_C(J4,1) - DSPL8  ( TIM_C(J4), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 5 ) THEN
                   EC(J4) = EOP_C(J4,2) - DSPL8  ( TIM_C(J4), NM, TIM_M, EOP_M(1,2), IK, EOP_SPL_M(1,2) ) 
                 ELSE IF ( ICMP == 6 ) THEN
                   EC(J4) = EOP_C(J4,1) - DSPL8  ( TIM_C(J4), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 7 ) THEN
                   EC(J4) = EOP_C(J4,1) - D2SPL8 ( TIM_C(J4), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 8 ) THEN
                   EC(J4) = EOP_C(J4,2) - D2SPL8 ( TIM_C(J4), NM, TIM_M, EOP_M(1,2), IK, EOP_SPL_M(1,2) ) 
                 ELSE IF ( ICMP == 9 ) THEN
                   EC(J4) = EOP_C(J4,1) - D2SPL8 ( TIM_C(J4), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
              END IF
              TC(J4) = (TIM_C(J4) - TIM_A(NA))/86400.0D0
 440       CONTINUE 
!
           DO 450 J5=1,NR
              IF ( J5 == 1 ) THEN
                   IK = IXMN8 ( NM, TIM_M, TIM_R(1) + TIM_EPS )
                 ELSE IF ( J5 == NC ) THEN
                   IK = IXMN8 ( NM, TIM_M, TIM_R(NR) - TIM_EPS )
                 ELSE
                   IK = IXMN8 ( NM, TIM_M, TIM_R(J5) )
              END IF
              IF ( ICMP == 1 ) THEN
                   ER(J5) = EOP_R(J5,1) - FSPL8 ( TIM_R(J5), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 2 ) THEN
                   ER(J5) = EOP_R(J5,2) - FSPL8 ( TIM_R(J5), NM, TIM_M, EOP_M(1,2), IK, EOP_SPL_M(1,2) ) 
                 ELSE IF ( ICMP == 3 ) THEN
                   ER(J5) = EOP_R(J5,1) - FSPL8 ( TIM_R(J5), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 4 ) THEN
                   ER(J5) = EOP_R(J5,1) - DSPL8 ( TIM_R(J5), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 5 ) THEN
                   ER(J5) = EOP_R(J5,2) - DSPL8 ( TIM_R(J5), NM, TIM_M, EOP_M(1,2), IK, EOP_SPL_M(1,2) ) 
                 ELSE IF ( ICMP == 6 ) THEN
                   ER(J5) = EOP_R(J5,1) - DSPL8 ( TIM_R(J5), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 7 ) THEN
                   ER(J5) = EOP_R(J5,1) - D2SPL8 ( TIM_R(J5), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 8 ) THEN
                   ER(J5) = EOP_R(J5,2) - D2SPL8 ( TIM_R(J5), NM, TIM_M, EOP_M(1,2), IK, EOP_SPL_M(1,2) ) 
                 ELSE IF ( ICMP == 9 ) THEN
                   ER(J5) = EOP_R(J5,1) - D2SPL8 ( TIM_R(J5), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
              END IF
              TR(J5) = (TIM_R(J5) - TIM_A(NA))/86400.0D0
 450       CONTINUE 
!
           DO 460 J6=1,NF
              IF ( J6 == 1 ) THEN
                   IK = IXMN8 ( NM, TIM_M, TIM_F(1) + TIM_EPS )
                 ELSE IF ( J6 == NF ) THEN
                   IK = IXMN8 ( NM, TIM_M, TIM_F(NF) - TIM_EPS )
                 ELSE
                   IK = IXMN8 ( NM, TIM_M, TIM_F(J6) )
              END IF
              IF ( ICMP == 1 ) THEN
                   EF(J6) = EF(J6) - FSPL8 ( TIM_F(J6), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 2 ) THEN
                   EF(J6) = EF(J6) - FSPL8 ( TIM_F(J6), NM, TIM_M, EOP_M(1,2), IK, EOP_SPL_M(1,2) ) 
                 ELSE IF ( ICMP == 3 ) THEN
                   EF(J6) = EF(J6) - FSPL8 ( TIM_F(J6), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 4 ) THEN
                   EF(J6) = EF(J6) - DSPL8 ( TIM_F(J6), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 5 ) THEN
                   EF(J6) = EF(J6) - DSPL8 ( TIM_F(J6), NM, TIM_M, EOP_M(1,2), IK, EOP_SPL_M(1,2) ) 
                 ELSE IF ( ICMP == 6 ) THEN
                   EF(J6) = EF(J6) - DSPL8 ( TIM_F(J6), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 7 ) THEN
                   EF(J6) = EF(J6) - D2SPL8 ( TIM_F(J6), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 8 ) THEN
                   EF(J6) = EF(J6) - D2SPL8 ( TIM_F(J6), NM, TIM_M, EOP_M(1,2), IK, EOP_SPL_M(1,2) ) 
                 ELSE IF ( ICMP == 9 ) THEN
                   EF(J6) = EF(J6) - D2SPL8 ( TIM_F(J6), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
              END IF
              TF(J6) = (TIM_F(J6) - TIM_A(NA))/86400.0D0
!!   write ( 6 , * ) ' j2= ', int2(j2), ' ik= ', ik, ' tim_f(j6) ', tim_f(j6), ' tim_m(1) = ', tim_m(1), ' eop_m(1,1) = ', eop_m(1,1) ! %%%%%
 460       CONTINUE 
!
           DO 470 J7=1,NA
              IF ( J7 == 1 ) THEN
                   IK = IXMN8 ( NM, TIM_M, TIM_A(1) + TIM_EPS )
                 ELSE IF ( J7 == NA ) THEN
                   IK = IXMN8 ( NM, TIM_M, TIM_A(NA) - TIM_EPS )
                 ELSE
                   IK = IXMN8 ( NM, TIM_M, TIM_A(J7) )
              END IF
              IF ( ICMP == 1 ) THEN
                   EA(J7) = EOP_A(J7,1) - FSPL8  ( TIM_A(J7), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 2 ) THEN
                   EA(J7) = EOP_A(J7,2) - FSPL8  ( TIM_A(J7), NM, TIM_M, EOP_M(1,2), IK, EOP_SPL_M(1,2) ) 
                 ELSE IF ( ICMP == 3 ) THEN
                   EA(J7) = EOP_A(J7,1) - FSPL8  ( TIM_A(J7), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 4 ) THEN
                   EA(J7) = EOP_A(J7,1) - DSPL8  ( TIM_A(J7), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 5 ) THEN
                   EA(J7) = EOP_A(J7,2) - DSPL8  ( TIM_A(J7), NM, TIM_M, EOP_M(1,2), IK, EOP_SPL_M(1,2) ) 
                 ELSE IF ( ICMP == 6 ) THEN
                   EA(J7) = EOP_A(J7,1) - DSPL8  ( TIM_A(J7), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 7 ) THEN
                   EA(J7) = EOP_A(J7,1) - D2SPL8 ( TIM_A(J7), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
                 ELSE IF ( ICMP == 8 ) THEN
                   EA(J7) = EOP_A(J7,2) - D2SPL8 ( TIM_A(J7), NM, TIM_M, EOP_M(1,2), IK, EOP_SPL_M(1,2) ) 
                 ELSE IF ( ICMP == 9 ) THEN
                   EA(J7) = EOP_A(J7,1) - D2SPL8 ( TIM_A(J7), NM, TIM_M, EOP_M(1,1), IK, EOP_SPL_M(1,1) ) 
              END IF
              TA(J7) = (TIM_A(J7) - TIM_A(NA))/86400.0D0
 470       CONTINUE 
      END IF
!
      IF ( MODE == 1 ) THEN
           DIAGI_S%NCLR      = 3
           DIAGI_S%NPOI(1)   = NC
           DIAGI_S%NPOI(2)   = NR
           DIAGI_S%NPOI(3)   = NF
           DIAGI_S%ADR_X8(1) = LOC(TIM_C)
           DIAGI_S%ADR_X8(2) = LOC(TIM_R)
           DIAGI_S%ADR_X8(3) = LOC(TIM_F)
           IF ( ICMP == 2 .OR. ICMP == 5 ) THEN
                DIAGI_S%ADR_Y8(1) = LOC(EOP_C(1,2))
                DIAGI_S%ADR_Y8(2) = LOC(EOP_R(1,2))
              ELSE 
                DIAGI_S%ADR_Y8(1) = LOC(EOP_C)
                DIAGI_S%ADR_Y8(2) = LOC(EOP_R)
           END IF
           DIAGI_S%ADR_Y8(3) = LOC(EF)
           DIAGI_S%LER(1)    = .FALSE.
           DIAGI_S%LER(2)    = .FALSE.
           DIAGI_S%LER(3)    = .FALSE.
           DIAGI_S%ICOL(1)   = ICL1
           DIAGI_S%ICOL(2)   = ICL2
           DIAGI_S%ICOL(3)   = ICL3
           DIAGI_S%IBST(1)   = 0
           DIAGI_S%IBST(2)   = 0
           DIAGI_S%IBST(3)   = 0
           DIAGI_S%ILST(1)   = ILST
           DIAGI_S%ILST(2)   = ILST
           DIAGI_S%ILST(3)   = ILST
           DIAGI_S%IOST(1)   = IOST
           DIAGI_S%IOST(2)   = IOST
           DIAGI_S%IOST(3)   = IOST
           DIAGI_S%IPST(1)   = 3
           DIAGI_S%IPST(2)   = 5
           DIAGI_S%IPST(3)   = 4
           DIAGI_S%IWST(1)   = 1
           DIAGI_S%IWST(2)   = 1
           DIAGI_S%IWST(3)   = 1
           DIAGI_S%ICLR      = 1
        ELSE IF ( MODE == 2 .AND. ( ICMP .EQ. 1 .OR. ICMP .EQ. 2 .OR. ICMP .EQ. 3 ) ) THEN
           DIAGI_S%NCLR      = 3
           DIAGI_S%NPOI(1)   = NC
           DIAGI_S%NPOI(2)   = NR
           DIAGI_S%NPOI(3)   = NF
           DIAGI_S%ADR_X8(1) = LOC(TC)
           DIAGI_S%ADR_X8(2) = LOC(TR)
           DIAGI_S%ADR_X8(3) = LOC(TF)
           DIAGI_S%ADR_Y8(1) = LOC(EC)
           DIAGI_S%ADR_Y8(2) = LOC(ER)
           DIAGI_S%ADR_Y8(3) = LOC(EF)
           DIAGI_S%LER(1)    = .FALSE.
           DIAGI_S%LER(2)    = .FALSE.
           DIAGI_S%LER(3)    = .FALSE.
           DIAGI_S%ICOL(1)   = ICL1
           DIAGI_S%ICOL(2)   = ICL2
           DIAGI_S%ICOL(3)   = ICL3
           DIAGI_S%IBST(1)   = 0
           DIAGI_S%IBST(2)   = 0
           DIAGI_S%IBST(3)   = 0
           DIAGI_S%ILST(1)   = ILST
           DIAGI_S%ILST(2)   = ILST
           DIAGI_S%ILST(3)   = ILST
           DIAGI_S%IOST(1)   = IOST
           DIAGI_S%IOST(2)   = IOST
           DIAGI_S%IOST(3)   = IOST
           DIAGI_S%IPST(1)   = 3
           DIAGI_S%IPST(2)   = 5
           DIAGI_S%IPST(3)   = 4
           DIAGI_S%IWST(1)   = 1
           DIAGI_S%IWST(2)   = 1
           DIAGI_S%IWST(3)   = 1
           DIAGI_S%ICLR      = 1
        ELSE IF ( MODE == 2 .AND. ( ICMP .EQ. 4 .OR. ICMP .EQ. 5 .OR. ICMP .EQ. 6 ) ) THEN
           DIAGI_S%NCLR      = 4
           DIAGI_S%NPOI(1)   = NC
           DIAGI_S%NPOI(2)   = NR
           DIAGI_S%NPOI(3)   = NF
           DIAGI_S%NPOI(4)   = NA
           DIAGI_S%ADR_X8(1) = LOC(TC)
           DIAGI_S%ADR_X8(2) = LOC(TR)
           DIAGI_S%ADR_X8(3) = LOC(TF)
           DIAGI_S%ADR_X8(4) = LOC(TA)
           DIAGI_S%ADR_Y8(1) = LOC(EC)
           DIAGI_S%ADR_Y8(2) = LOC(ER)
           DIAGI_S%ADR_Y8(3) = LOC(EF)
           DIAGI_S%ADR_Y8(4) = LOC(EA)
           DIAGI_S%LER(1)    = .FALSE.
           DIAGI_S%LER(2)    = .FALSE.
           DIAGI_S%LER(3)    = .FALSE.
           DIAGI_S%LER(4)    = .FALSE.
           DIAGI_S%ICOL(1)   = ICL1
           DIAGI_S%ICOL(2)   = ICL2
           DIAGI_S%ICOL(3)   = ICL3
           DIAGI_S%ICOL(4)   = 4
           DIAGI_S%IBST(1)   = 0
           DIAGI_S%IBST(2)   = 0
           DIAGI_S%IBST(3)   = 0
           DIAGI_S%IBST(4)   = 0
           DIAGI_S%ILST(1)   = ILST
           DIAGI_S%ILST(2)   = ILST
           DIAGI_S%ILST(3)   = ILST
           DIAGI_S%ILST(4)   = ILST
           DIAGI_S%IOST(1)   = IOST
           DIAGI_S%IOST(2)   = IOST
           DIAGI_S%IOST(3)   = IOST
           DIAGI_S%IOST(4)   = IOST
           DIAGI_S%IPST(1)   = 3
           DIAGI_S%IPST(2)   = 5
           DIAGI_S%IPST(3)   = 4
           DIAGI_S%IPST(4)   = 4
           DIAGI_S%IWST(1)   = 1
           DIAGI_S%IWST(2)   = 1
           DIAGI_S%IWST(3)   = 1
           DIAGI_S%IWST(4)   = 1
           DIAGI_S%ICLR      = 1
        ELSE IF ( MODE == 2 .AND. ( ICMP .EQ. 7 .OR. ICMP .EQ. 8 .OR. ICMP .EQ. 9 ) ) THEN
           DIAGI_S%NCLR      = 1
           DIAGI_S%NPOI(1)   = NF
           DIAGI_S%ADR_X8(1) = LOC(TF)
           DIAGI_S%ADR_Y8(1) = LOC(EF)
           DIAGI_S%LER(1)    = .FALSE.
           DIAGI_S%ICOL(1)   = ICL3
           DIAGI_S%IBST(1)   = 0
           DIAGI_S%ILST(1)   = ILST
           DIAGI_S%IOST(1)   = IOST
           DIAGI_S%IPST(1)   = 4
           DIAGI_S%IWST(1)   = 2
           DIAGI_S%ICLR      = 1
         ELSE IF ( MODE == 3 ) THEN
           DIAGI_S%NCLR      = 4
           DIAGI_S%NPOI(1)   = NC
           DIAGI_S%NPOI(2)   = NR
           DIAGI_S%NPOI(3)   = NF
           DIAGI_S%NPOI(4)   = NM
           DIAGI_S%ADR_X8(1) = LOC(TC)
           DIAGI_S%ADR_X8(2) = LOC(TR)
           DIAGI_S%ADR_X8(3) = LOC(TF)
           DIAGI_S%ADR_X8(4) = LOC(TM)
           DIAGI_S%ADR_Y8(1) = LOC(EOP_C)
           DIAGI_S%ADR_Y8(2) = LOC(EOP_R)
           DIAGI_S%ADR_Y8(3) = LOC(EF)
           DIAGI_S%ADR_Y8(4) = LOC(EM)
           DIAGI_S%LER(1)    = .FALSE.
           DIAGI_S%LER(2)    = .FALSE.
           DIAGI_S%LER(3)    = .FALSE.
           DIAGI_S%LER(4)    = .FALSE.
           DIAGI_S%ICOL(1)   = ICL1
           DIAGI_S%ICOL(2)   = ICL2
           DIAGI_S%ICOL(3)   = ICL3
           DIAGI_S%ICOL(4)   = 4
           DIAGI_S%IBST(1)   = 0
           DIAGI_S%IBST(2)   = 0
           DIAGI_S%IBST(3)   = 0
           DIAGI_S%IBST(4)   = 0
           DIAGI_S%ILST(1)   = ILST
           DIAGI_S%ILST(2)   = ILST
           DIAGI_S%ILST(3)   = ILST
           DIAGI_S%ILST(4)   = ILST
           DIAGI_S%IOST(1)   = IOST
           DIAGI_S%IOST(2)   = IOST
           DIAGI_S%IOST(3)   = IOST
           DIAGI_S%IOST(4)   = IOST
           DIAGI_S%IPST(1)   = 3
           DIAGI_S%IPST(2)   = 5
           DIAGI_S%IPST(3)   = 2
           DIAGI_S%IPST(4)   = 1
           DIAGI_S%IWST(1)   = 1
           DIAGI_S%IWST(2)   = 1
           DIAGI_S%IWST(3)   = 1
           DIAGI_S%IWST(4)   = 1
           DIAGI_S%ICLR      = 1
      END IF
      DIAGI_S%XMIN      = TIM_F(1) - DIAGI_FIE*(TIM_F(NF) - TIM_F(1))
      DIAGI_S%XMAX      = TIM_F(1) + DIAGI_FIE*(TIM_F(NF) - TIM_F(1))
      DIAGI_S%YMIN = 1.0
      DIAGI_S%YMAX = 0.0
      IF ( ICMP == 1 ) THEN
           DIAGI_S%ZAG       = 'E1'
           DIAGI_S%NAME      = '/tmp/e1'
         ELSE IF ( ICMP == 2 ) THEN
           DIAGI_S%ZAG       = 'E2'
           DIAGI_S%NAME      = '/tmp/e2'
         ELSE IF ( ICMP == 3 ) THEN
           DIAGI_S%ZAG       = 'E3'
           DIAGI_S%NAME      = '/tmp/e3'
         ELSE IF ( ICMP == 4 ) THEN
           DIAGI_S%ZAG       = 'E1-rate'
           DIAGI_S%NAME      = '/tmp/e3r'
         ELSE IF ( ICMP == 5 ) THEN
           DIAGI_S%ZAG       = 'E2-rate'
           DIAGI_S%NAME      = '/tmp/e3r'
         ELSE IF ( ICMP == 6 ) THEN
           DIAGI_S%ZAG       = 'E3-rate'
           DIAGI_S%NAME      = '/tmp/e3r'
         ELSE IF ( ICMP == 7 ) THEN
           DIAGI_S%ZAG       = 'E1-accl'
           DIAGI_S%NAME      = '/tmp/e3d'
         ELSE IF ( ICMP == 8 ) THEN
           DIAGI_S%ZAG       = 'E2-accl'
           DIAGI_S%NAME      = '/tmp/e3d'
         ELSE IF ( ICMP == 9 ) THEN
           DIAGI_S%ZAG       = 'E3-accl'
           DIAGI_S%NAME      = '/tmp/e3d'
      END IF
      DIAGI_S%ARG_UNITS = 'Time in sec'
      DIAGI_S%ITRM      = 0
      DIAGI_S%STATUS    = DIA__DEF
      DIAGI_S%IBATCH = 0
      DIAGI_S%IDEV   = IDEV
!
! --- Calling the main routine of DiaGI
!
      CALL ERR_PASS ( IUER, IER )
      CALL DIAGI ( DIAGI_S, IER )
!
      IF ( ALLOCATED ( TM ) ) DEALLOCATE ( TM )
      IF ( ALLOCATED ( TF ) ) DEALLOCATE ( TF )
      IF ( ALLOCATED ( TC ) ) DEALLOCATE ( TC )
      IF ( ALLOCATED ( TR ) ) DEALLOCATE ( TR )
      IF ( ALLOCATED ( TA ) ) DEALLOCATE ( TA )
!
      IF ( ALLOCATED ( EM ) ) DEALLOCATE ( EM )
      IF ( ALLOCATED ( EF ) ) DEALLOCATE ( EF )
      IF ( ALLOCATED ( EC ) ) DEALLOCATE ( EC )
      IF ( ALLOCATED ( ER ) ) DEALLOCATE ( ER )
      IF ( ALLOCATED ( EA ) ) DEALLOCATE ( EA )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PLOT_EOP_FCS  !#!  
