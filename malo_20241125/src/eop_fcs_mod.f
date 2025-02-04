      SUBROUTINE EOP_E3_MOD ( NV, TIMV_DAT, E3_DAT,  E3_SIG,           &
     &                        NR, TIMR_DAT, E3R_DAT, E3R_SIG,          &
     &                        NM, TIM_MOD, E3_MOD, E3_SPL_MOD,         &
     &                        TIM_FIRST, TIM_LAST, WEI_SCLV, WEI_SCLR, &
     &                        CNS_DER, CNS_DR2, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine EOP_E3_MOD 
! *                                                                      *
! *  ### 25-MAR-2016   EOP_E3_MOD  v2.0 (c)  L. Petrov  10-MAY-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INTEGER*4  MODE, NV, NR, NM, IVRB, IUER
      REAL*8     TIMV_DAT(NV), E3_DAT(NV),  E3_SIG(NV),     &
     &           TIMR_DAT(NR), E3R_DAT(NR), E3R_SIG(NR),    &
     &           TIM_MOD(NM),  E3_MOD(NM),  E3_SPL_MOD(NM), &
     &           TIM_FIRST, TIM_LAST, WEI_SCLV, WEI_SCLR, CNS_DER, CNS_DR2
      INTEGER*4  M_PAR, M_PA2, MF
      PARAMETER  ( M_PAR = 3 )
      PARAMETER  ( M_PA2 = (M_PAR*(M_PAR+1))/2 )
      REAL*8,    ALLOCATABLE :: TMP_ARR(:)
      REAL*8     NOR_MAT(M_PA2), NOR_VEC(M_PAR), EQU_VEC(M_PAR), &
     &           EST_VEC(6), RC, WEI
      REAL*8     SCL, DAY
      PARAMETER  ( SCL = 1.D-9 )
      PARAMETER  ( DAY = 86400.0D0 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, KR, KV, IP, IPP, IER
      REAL*8,    EXTERNAL :: DP_VV_V
!
      NOR_MAT = 0.0D0
      NOR_VEC = 0.0D0
!
! --- Build the normal matrix for the second degree polynomial
!
      KV = 0
!
! --- Take into account the contribution of the parameter
!
      DO 410 J1=1,NV
         IF ( TIMV_DAT(J1) < TIM_FIRST ) GOTO 410
         IF ( KV == 0 ) KV = J1
         EQU_VEC = 0.0D0
         EQU_VEC(1) = 1.0D0
         EQU_VEC(2) =  (TIMV_DAT(J1) - TIMV_DAT(NV))/DAY
         EQU_VEC(3) = ((TIMV_DAT(J1) - TIMV_DAT(NV))/DAY)**2
         WEI = WEI_SCLV*SCL/E3_SIG(J1)
         CALL DIAD_CVT_S ( WEI**2, 3, EQU_VEC, EQU_VEC, NOR_MAT )
         CALL NORVEC_UPD ( 3, WEI, E3_DAT(J1), EQU_VEC, NOR_VEC )
 410  CONTINUE 
!
      KR = 0
!
! --- Take into account the contributionof the rate
!
      DO 420 J2=1,NR
         IF ( TIMR_DAT(J2) < TIM_FIRST ) GOTO 420
         IF ( KR == 0 ) KR = J2
         EQU_VEC = 0.D0
         EQU_VEC(1) = 0.0D0
         EQU_VEC(2) = 1.0D0/DAY
         EQU_VEC(3) = 2.0D0*(TIMR_DAT(J2) - TIMR_DAT(NR))/DAY**2
         WEI = WEI_SCLR*SCL/E3R_SIG(J2) 
         CALL DIAD_CVT_S ( WEI**2, 3, EQU_VEC, EQU_VEC, NOR_MAT )
         CALL NORVEC_UPD ( 3, WEI, E3R_DAT(J2), EQU_VEC, NOR_VEC )
 420  CONTINUE 
!
! --- Impose constraints on rate
!
      IP = 2
      IPP = (IP*(IP+1))/2
      NOR_MAT(IPP) = NOR_MAT(IPP) + (SCL/CNS_DER/DAY)**2
!
      IP = 3
      IPP = (IP*(IP+1))/2
      NOR_MAT(IPP) = NOR_MAT(IPP) + (SCL/CNS_DR2/DAY**2)**2
!
! --- Solve the equation
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( M_PAR, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6641, IUER, 'EOP_E3_MOD', 'Error in an attempt '// &
     &         'to invert normal matrix' )
           RETURN 
      END IF
      CALL MUL_MV_SV_V ( M_PAR, NOR_MAT, M_PAR, NOR_VEC, M_PAR, EST_VEC, IER )
!
      ALLOCATE ( TMP_ARR(NM), STAT=IER  )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6642, IUER, 'EOP_E3_MOD', 'Error in an attempt '// &
     &         'to allocate dynamic memory for temporary array TMP_ARR' ) 
           RETURN 
      END IF
!
      DO 430 J3=1,NM
         TIM_MOD(J3) = TIM_FIRST + (J3-1)*(TIM_LAST - TIM_FIRST)/(NM-1)
         EQU_VEC(1) =  1.0D0
         EQU_VEC(2) =  (TIM_MOD(J3) - TIMV_DAT(NV))/DAY
         EQU_VEC(3) = ((TIM_MOD(J3) - TIMV_DAT(NV))/DAY)**2
         E3_MOD(J3) = DP_VV_V ( 3, EQU_VEC, EST_VEC )
 430  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL MAKE_SPLINE ( 3, NM, TIM_MOD, E3_MOD, 0.0D0, 0.0D0, &
     &                   E3_SPL_MOD, TMP_ARR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6643, IUER, 'EOP_E3_MOD', 'Error in an attempt '// &
     &         'to coimpute splione coefficicients for E3 model' )
           RETURN 
      END IF
!    
      IF ( IVRB == 23 .OR. IVRB == 53 ) THEN
!           write ( 6, * ) ' nv, nm, nr ', nv, nm, nr ! %%%%
!           write ( 6, * ) ' m_par=  ', m_par ! %%%
!           call diagi_setdef ( ier, 'diagi_ctit', 'e3_spl_mod' )
!           call diagi_1 ( nm, tim_mod, e3_spl_mod, ier )
!           call diagi_setdef ( ier, 'diagi_ctit', 'e3_mod' )
!           call diagi_1 ( nm, tim_mod, e3_mod, ier )
!
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'E3 model-short' )
           CALL DIAGI_2 ( NV-KV+1, TIMV_DAT(KV), E3_DAT(KV:NV) - E3_DAT(KV), &
     &                    NM, TIM_MOD, E3_MOD(1:NM) - E3_DAT(KV), IER )
!
      END IF
!
      DEALLOCATE ( TMP_ARR )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EOP_E3_MOD  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EOP_E12_MOD ( MP, ND, TIM_DAT, E12_DAT, E12_SIG,   &
     &                                          E12R_DAT, E12R_SIG, &
     &                         NM, TIM_MOD, E12_MOD, E12_SPL_MOD, &
     &                         TIM_FIRST, TIM_LAST, WEI_SCLV, WEI_SCLR, &
     &                         CNS_DER, CNS_DR2, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine EOP_E12_MOD 
! *                                                                      *
! *  ### 25-MAR-2016  EOP_E12_MOD  v2.0 (c)  L. Petrov  09-MAY-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INTEGER*4  MODE, MP, ND, NM, IVRB, IUER
      REAL*8     TIM_DAT(ND), E12_DAT(MP,2), E12_SIG(MP,2), &
     &                        E12R_DAT(MP,2), E12R_SIG(MP,2), &
     &           TIM_MOD(NM), E12_MOD(NM,2), E12_SPL_MOD(NM,2), &
     &           TIM_FIRST, TIM_LAST, WEI_SCLV, WEI_SCLR, CNS_DER, CNS_DR2
      INTEGER*4  M_PAR, M_PA2, MF
      PARAMETER  ( M_PAR = 6 )
      PARAMETER  ( M_PA2 = (M_PAR*(M_PAR+1))/2 )
      REAL*8,    ALLOCATABLE :: TMP_ARR(:)
      REAL*8     NOR_MAT(M_PA2), NOR_VEC(M_PAR), EQU_VEC(M_PAR), &
     &           EST_VEC(6), RC, WEI
      REAL*8     SCL, DAY
      PARAMETER  ( SCL = 1.D-9 )
      PARAMETER  ( DAY = 86400.0D0 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, KD, IP, IPP, IER
      REAL*8,    EXTERNAL :: DP_VV_V
!
      NOR_MAT = 0.0D0
      NOR_VEC = 0.0D0
!
      KD = 0
      DO 410 J1=1,ND
         IF ( TIM_DAT(J1) < TIM_FIRST ) GOTO 410
         IF ( KD == 0 ) KD = J1
         DO 420 J2=1,2
            EQU_VEC = 0.D0
            EQU_VEC(1+(J2-1)*3) = 1.0D0
            EQU_VEC(2+(J2-1)*3) =  (TIM_DAT(J1) - TIM_DAT(ND))/DAY
            EQU_VEC(3+(J2-1)*3) = ((TIM_DAT(J1) - TIM_DAT(ND))/DAY)**2
            WEI = WEI_SCLV*SCL/E12_SIG(J1,J2)
            CALL DIAD_CVT_S ( WEI**2, 6, EQU_VEC, EQU_VEC, NOR_MAT )
            CALL NORVEC_UPD ( 6, WEI, E12_DAT(J1,J2), EQU_VEC, NOR_VEC )
 420     CONTINUE 
!
         DO 430 J3=1,2
            EQU_VEC = 0.D0
            EQU_VEC(1+(J3-1)*3) = 0.0D0
            EQU_VEC(2+(J3-1)*3) = 1.0D0/DAY
            EQU_VEC(3+(J3-1)*3) = 2.0D0*(TIM_DAT(J1) - TIM_DAT(ND))/DAY**2
            WEI = WEI_SCLR*SCL/E12R_SIG(J1,J3) 
            CALL DIAD_CVT_S ( WEI**2, 6, EQU_VEC, EQU_VEC, NOR_MAT )
            CALL NORVEC_UPD ( 6, WEI, E12R_DAT(J1,J3), EQU_VEC, NOR_VEC )
 430     CONTINUE 
 410  CONTINUE 
!
      DO 440 J4=1,2
!
! ------ Impose constraints on rate
!
         IP = 2 + (J4-1)*3
         IPP = (IP*(IP+1))/2
         NOR_MAT(IPP) = NOR_MAT(IPP) + (SCL/CNS_DER/DAY)**2
!
         IP = 3 + (J4-1)*3
         IPP = (IP*(IP+1))/2
         NOR_MAT(IPP) = NOR_MAT(IPP) + (SCL/CNS_DR2/DAY**2)**2
 440  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( M_PAR, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6651, IUER, 'EOP_E12_MOD', 'Error in an attempt '// &
     &         'to invert normal matrix' )
           RETURN 
      END IF
      CALL MUL_MV_SV_V ( M_PAR, NOR_MAT, M_PAR, NOR_VEC, M_PAR, EST_VEC, IER )
!
      ALLOCATE ( TMP_ARR(NM), STAT=IER  )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6652, IUER, 'EOP_E12_MOD', 'Error in an attempt '// &
     &         'to allocate dynamic memory for temporary arrayt TMP_ARR' ) 
           RETURN 
      END IF
!
      DO 450 J5=1,NM
         TIM_MOD(J5) = TIM_FIRST + (J5-1)*(TIM_LAST - TIM_FIRST)/(NM-1)
         EQU_VEC(1) =  1.0D0
         EQU_VEC(2) =  (TIM_MOD(J5) - TIM_DAT(ND))/DAY
         EQU_VEC(3) = ((TIM_MOD(J5) - TIM_DAT(ND))/DAY)**2
         E12_MOD(J5,1) = DP_VV_V ( 3, EQU_VEC, EST_VEC(1) )
         E12_MOD(J5,2) = DP_VV_V ( 3, EQU_VEC, EST_VEC(4) )
 450  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL MAKE_SPLINE ( 3, NM, TIM_MOD, E12_MOD(1,1), 0.0D0, 0.0D0, &
     &                   E12_SPL_MOD(1,1), TMP_ARR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6653, IUER, 'EOP_E12_MOD', 'Error in an attempt '// &
     &         'to coimpute splione coefficicients for E12 model' )
           RETURN 
      END IF
!    
      CALL ERR_PASS ( IUER, IER ) 
      CALL MAKE_SPLINE ( 3, NM, TIM_MOD, E12_MOD(1,2), 0.0D0, 0.0D0, &
     &                   E12_SPL_MOD(1,2), TMP_ARR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6654, IUER, 'EOP_E12_MOD', 'Error in an attempt '// &
     &         'to coimpute splione coefficicients for E12 model' )
           RETURN 
      END IF
!    
      IF ( IVRB == 51 ) THEN
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'E1 model' )
           CALL DIAGI_2 ( ND-KD+1, TIM_DAT(KD), E12_DAT(KD:ND,1) - E12_DAT(KD,1), &
     &                    NM, TIM_MOD, E12_MOD(1:NM,1) - E12_DAT(KD,1), IER )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'E2 model' )
           CALL DIAGI_2 ( ND-KD+1, TIM_DAT(KD), E12_DAT(KD:ND,2) - E12_DAT(KD,2), &
     &                    NM, TIM_MOD, E12_MOD(1:NM,2) - E12_DAT(KD,2), IER )
      END IF
!
      DEALLOCATE ( TMP_ARR )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EOP_E12_MOD  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PLOT_EOP_MOD ( MODE, ND, TIM_DAT, EOP_DAT, SIG_DAT, &
     &                                NM, TIM_MOD, EOP_MOD, EOP_SPL_MOD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PLOT_EOP_MOD 
! *                                                                      *
! *  ### 25-MAR-2016 PLOT_EOP_MOD  v2.0 (c)  L. Petrov  07-MAY-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MODE, ND, NM, IUER
      REAL*8     TIM_DAT(ND), EOP_DAT(ND), SIG_DAT(ND), &
     &           TIM_MOD(NM), EOP_MOD(NM), EOP_SPL_MOD(NM)
      REAL*8,    ALLOCATABLE :: TIM_ARR(:), TMM_ARR(:), MOD_ARR(:), DIF_ARR(:)
      REAL*8     TIM_DAT_LAST
      INTEGER*4  J1, J2, KP, IP, IER
      REAL*8,    EXTERNAL :: FSPL8, DSPL8
      INTEGER*4, EXTERNAL :: IXMN8
      CHARACTER, EXTERNAL :: TIM_TO_DATE*23
!
      ALLOCATE ( TIM_ARR(ND) )
      ALLOCATE ( TMM_ARR(ND) )
      ALLOCATE ( MOD_ARR(ND) )
      ALLOCATE ( DIF_ARR(ND) )
!
      KP = 0
      DO 410 J1=1,ND
         IP = IXMN8 ( NM, TIM_MOD, TIM_DAT(J1) )
         IF ( IP > 1 ) THEN
              KP = KP + 1 
              TIM_ARR(KP) = TIM_DAT(J1)
              TMM_ARR(KP) = (TIM_ARR(KP) - TIM_DAT(1))/86400.0D0
              IF ( MODE == 1 .OR. MODE == 2 .OR. MODE == 3 ) THEN
                   MOD_ARR(KP) = FSPL8 ( TIM_ARR(KP), NM, TIM_MOD, EOP_MOD, IP, EOP_SPL_MOD )
                 ELSE 
                   MOD_ARR(KP) = DSPL8 ( TIM_ARR(KP), NM, TIM_MOD, EOP_MOD, IP, EOP_SPL_MOD )
              END IF
              TIM_DAT_LAST = TIM_DAT(J1)
              DIF_ARR(KP) = EOP_DAT(J1) - MOD_ARR(KP)
         END IF
 410  CONTINUE 
!
      WRITE( 6, * ) 'TIM_DAT(1):   ', TIM_TO_DATE( TIM_DAT(1),   IUER ), ' KP = ', KP
      WRITE( 6, * ) 'TIM_MOD(1):   ', TIM_TO_DATE( TIM_MOD(1),   IUER ), ' TIM_DAT_LAST= ', TIM_DAT_LAST
      WRITE( 6, * ) 'TIM_ARR(1):   ', TIM_TO_DATE( TIM_ARR(1),   IUER )
      WRITE( 6, * ) 'TIM_DAT_LAST: ', TIM_TO_DATE( TIM_DAT_LAST, IUER )
!
      IF ( MODE == 1 ) THEN
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'E1 mod differences' )
        ELSE IF ( MODE == 2 ) THEN
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'E2 mod differences' )
        ELSE IF ( MODE == 3 ) THEN
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'E3 mod differences' )
        ELSE IF ( MODE == 4 ) THEN
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'E1-rate mod differences' )
        ELSE IF ( MODE == 5 ) THEN
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'E2-rate mod differences' )
        ELSE IF ( MODE == 6 ) THEN
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'E3-rate mod differences' )
      END IF
      CALL DIAGI_2 ( KP, TIM_ARR, MOD_ARR, ND, TIM_DAT, EOP_DAT, IUER )
      CALL DIAGI_1 ( KP, TMM_ARR, DIF_ARR, IUER )
      DEALLOCATE ( TIM_ARR )
      DEALLOCATE ( TMM_ARR )
      DEALLOCATE ( MOD_ARR )
      DEALLOCATE ( DIF_ARR )
      RETURN
      END  SUBROUTINE PLOT_EOP_MOD  !#!#
