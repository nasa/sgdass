      SUBROUTINE PIMA_GET_ADDCLO ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_GET_ADDCLO
! *                                                                      *
! * ### 25-JAN-2012  PIMA_GET_ADDCLO  v1.0 (c) L. Petrov 26-JAN-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE    ) :: PIM
      INTEGER*4  IUER
      CHARACTER  MDA_FILE*128, STR*128
      REAL*8     TIM_TOL, ATM_OFF_MAX, ATM_RAT_MAX
      PARAMETER  ( TIM_TOL = 1.D-3 )
      PARAMETER  ( ATM_OFF_MAX = 1.D-6 )
      PARAMETER  ( ATM_RAT_MAX = 1.D-6 )
      REAL*8     CLO_OFF, CLO_RAT, ATM_OFF, ATM_RAT, TAI, &
     &           TIM_ARG, TIM_POL, APR_DEL, APR_RAT
      REAL*8     APR_DEL_DIF(PIM__MSCA), APR_RAT_DIF(PIM__MSCA), &
     &           TIM_ARR(PIM__MSCA), CLO_DR, CLO_OFF_0, VAL0, VAL1
      INTEGER*4  J1, J2, J3, J4, J5, LUN_MDC, NP, MJD, IND_MOD, LUN_MDA, IER
      CHARACTER  MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_UNIT
!
      MDA_FILE = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.mda'
      IF ( PIM%CONF%EXPER_DIR(I_LEN(PIM%CONF%EXPER_DIR):I_LEN(PIM%CONF%EXPER_DIR)) .EQ. '/' ) THEN
           MDA_FILE = PIM%CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))//MDA_FILE
         ELSE
           MDA_FILE = PIM%CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))//'/'//MDA_FILE
      END IF
!
      LUN_MDA = GET_UNIT()
      OPEN ( UNIT=LUN_MDA, FILE=MDA_FILE, STATUS='UNKNOWN', IOSTAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 7681, IUER, 'PIMA_GET_ADDCLO', 'Failure to '// &
     &         'open information file '//MDA_FILE(1:I_LEN(MDA_FILE))// &
     &         ' for writing -- error '//STR )
           RETURN
      END IF
!
      DO 410 J1=1,PIM%NSTA
         NP = 0
         DO 420 J2=1,PIM%STA(J1)%L_MDC
            IF ( DABS(PIM%STA(J1)%MDC%ATMO_DELAY(J2)) > ATM_OFF_MAX ) GOTO 420
            IF ( DABS(PIM%STA(J1)%MDC%ATMO_RATE(J2))  > ATM_RAT_MAX ) GOTO 420
!
            TIM_ARG = PIM%STA(J1)%MDC%TIME_CEN(J2)
            IND_MOD = 0
!
! --------- Search for the index of the array with polynomial coefficients
!
            DO 430 J3=1,PIM%STA(J1)%L_MOD
               IF ( PIM%STA(J1)%MOD(J3)%SOU_IND == PIM%STA(J1)%MDC%IND_SOU(J2) .AND. &
     &              DABS( TIM_ARG - PIM%STA(J1)%MOD(J3)%TIM_BEG ) < TIM_TOL ) THEN
                    IND_MOD = J3
                    GOTO 830
               END IF
 430        CONTINUE
 830        CONTINUE
!
            IF ( IND_MOD == 0 ) GOTO 420
!
            TIM_POL  = TIM_ARG - PIM%STA(J1)%MOD(IND_MOD)%TIM_BEG
            APR_DEL = PIM%STA(J1)%MOD(IND_MOD)%GDEL_POL(PIM__MDPL,1)
            APR_RAT = PIM%STA(J1)%MOD(IND_MOD)%PRAT_POL(PIM__MDPL,1)
            DO 440 J4=PIM__MDPL-1,0,-1
               APR_DEL = PIM%STA(J1)%MOD(IND_MOD)%GDEL_POL(J4,1) + APR_DEL*TIM_POL
               APR_RAT = PIM%STA(J1)%MOD(IND_MOD)%PRAT_POL(J4,1) + APR_RAT*TIM_POL
 440        CONTINUE
!
            NP = NP + 1
            TIM_ARR(NP) = TIM_ARG
            APR_DEL_DIF(NP) =   PIM%STA(J1)%MDC%GDELAY(J2) &
     &                        - APR_DEL &
     &                        + PIM%STA(J1)%MDC%ATMO_DELAY(J2) &
     &                        - PIM%STA(J1)%MDC%CLOCK_OFFSET(J2)
            APR_RAT_DIF(NP) =   PIM%STA(J1)%MDC%GRATE(J2)  &
     &                        - APR_RAT &
     &                        + PIM%STA(J1)%MDC%ATMO_RATE(J2) &
     &                        - PIM%STA(J1)%MDC%CLOCK_RATE(J2)
 420     CONTINUE 
!
         IF ( NP > 3 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL REGR8 ( NP, TIM_ARR, APR_DEL_DIF, PIM%STA(J1)%MDC%CLO_RATE, &
     &                     PIM%STA(J1)%MDC%CLO_OFFS, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7681, IUER, 'PIMA_GET_ADDCLO', 'Failure in '// &
     &                 'an attempt to compute linear regression for station '// &
     &                 'in apriori delay differences for station '//PIM%C_STA(J1) )
                   RETURN 
              END IF
              WRITE ( LUN_MDA, 110 ) J1, PIM%C_STA(J1), PIM%STA(J1)%MDC%CLO_OFFS, &
     &                               PIM%STA(J1)%MDC%CLO_RATE
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                   WRITE ( 6, 110 ) J1, PIM%C_STA(J1), PIM%STA(J1)%MDC%CLO_OFFS, &
     &                              PIM%STA(J1)%MDC%CLO_RATE
 110               FORMAT ( 'ADD_CLO ', I2, ') ', A, ' Add_clo_offs: ', 1PD16.8, &
     &                      ' Add_clo_rate: ', 1PD16.8 )
              END IF
              IF ( PIM%CONF%DEBUG_LEVEL == 13 ) THEN
                   VAL0 = APR_DEL_DIF(1) - PIM%STA(J1)%MDC%CLO_OFFS
                   VAL1 = APR_RAT_DIF(1) - PIM%STA(J1)%MDC%CLO_RATE
                   DO 450 J5=1,NP
                      APR_DEL_DIF(J5) = APR_DEL_DIF(J5) - &
     &                                 (PIM%STA(J1)%MDC%CLO_OFFS + &
     &                                  PIM%STA(J1)%MDC%CLO_RATE*TIM_ARR(J5))
                      APR_RAT_DIF(J5) = APR_RAT_DIF(J5) - PIM%STA(J1)%MDC%CLO_RATE
                      APR_DEL_DIF(J5) = APR_DEL_DIF(J5) - VAL0
                      APR_RAT_DIF(J5) = APR_RAT_DIF(J5) - VAL1 
 450               CONTINUE 
                   CALL DIAGI_SETDEF ( -2, 'DIAGI_CTIT', &
     &                                 'Apr_del_dif for '//PIM%C_STA(J1) )
                   CALL DIAGI_1 ( NP, TIM_ARR, APR_DEL_DIF, -2 )
                   CALL DIAGI_SETDEF ( -2, 'DIAGI_CTIT', &
     &                                 'Apr_rat_dif for '//PIM%C_STA(J1) )
                   CALL DIAGI_1 ( NP, TIM_ARR, APR_RAT_DIF, -2 )
              END IF
           ELSE 
              WRITE ( LUN_MDA, 110 ) J1, PIM%C_STA(J1), PIM%STA(J1)%MDC%CLO_OFFS, &
     &                               PIM%STA(J1)%MDC%CLO_RATE
        END IF
 410  CONTINUE 
      CLOSE ( UNIT=LUN_MDA )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GET_ADDCLO  !#!  
