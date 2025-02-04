      SUBROUTINE PIMA_TST1 ( PIM, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_TST1
! *                                                                      *
! *  ### 25-JAN-2012   PIMA_TST1   v1.0 (c)  L. Petrov  25-JAN-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE    ) :: PIM
      TYPE     ( VTD__TYPE   ) :: VTD
      INTEGER*4  IUER
      CHARACTER  MDC_FILE*128, STR*128
      CHARACTER, ALLOCATABLE ::BUF(:)*512
      INTEGER*4  J1, J2, J3, J4, J5, LUN_MDC, MP, NP, ISTA, ISOU, MJD, &
     &           IND_MOD, NDIF(PIM__MSTA), IER
      REAL*8     CLO_OFF, CLO_RAT, ATM_OFF, ATM_RAT, GRP_DEL, GRP_RAT, TAI, &
     &           TIM_TOL, TIM_ARG, TIM_POL, APR_DEL, APR_RAT
      REAL*8     APR_DEL_DIF(PIM__MSCA,PIM__MSTA), APR_RAT_DIF(PIM__MSCA,PIM__MSTA), &
     &           TIM(PIM__MSCA,PIM__MSTA), CLO_DR, CLO_OFF_0, VAL0, VAL1
      PARAMETER  ( TIM_TOL = 1.D-3 )
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF 
!
      MDC_FILE = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.mdc'
      IF ( PIM%CONF%EXPER_DIR(I_LEN(PIM%CONF%EXPER_DIR):I_LEN(PIM%CONF%EXPER_DIR)) .EQ. '/' ) THEN
           MDC_FILE = PIM%CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))//MDC_FILE
         ELSE
           MDC_FILE = PIM%CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))//'/'//MDC_FILE
      END IF
!
      MP = 128*1024
      ALLOCATE ( BUF(MP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL IINCH ( MP*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 7541, IUER, 'PIMA_TST1', 'Failure in '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic memory' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( MDC_FILE, MP, BUF, NP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7542, IUER, 'PIMA_TST1', 'Failure in '// &
     &         'attempt to read the model component file '// &
     &          MDC_FILE )
           RETURN 
      END IF
!
  write ( 6 ,* ) ' np = ' , np ! %%%%%
      NDIF = 0
      DO 410 J1=1,NP
         CALL ERR_PASS ( IUER, IER )
         CALL DATE_TO_TIME ( BUF(J1)(57:78), MJD, TAI, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 7543, IUER, 'PIMA_TST1', 'Failure '// &
     &            'parsing date in line '//STR(1:I_LEN(STR))// &
     &            ' of model component file '//MDC_FILE )
              RETURN 
         END IF
         ISTA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, BUF(J1)(19:26) )
         ISOU = LTM_DIF ( 0, PIM%NSOU, PIM%C_SOU, BUF(J1)(38:45) )
         IF ( ISTA < 1 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 7544, IUER, 'PIMA_TST1', 'Wrong '// &
     &            'station name at line '//STR(1:I_LEN(STR))// &
     &            ' of the model component file '//MDC_FILE )
              RETURN 
         END IF
         ISOU = LTM_DIF ( 0, PIM%NSOU, PIM%C_SOU, BUF(J1)(38:45) )
         READ ( UNIT=BUF(J1)(94:108),  FMT='(F15.7)'  ) CLO_OFF
         READ ( UNIT=BUF(J1)(122:136), FMT='(F15.7)'  ) CLO_RAT
         READ ( UNIT=BUF(J1)(151:165), FMT='(F15.7)'  ) ATM_OFF
         READ ( UNIT=BUF(J1)(179:193), FMT='(F15.7)'  ) ATM_RAT
         READ ( UNIT=BUF(J1)(209:227), FMT='(F19.11)' ) GRP_DEL
         READ ( UNIT=BUF(J1)(242:262), FMT='(F19.11)' ) GRP_RAT
         IF ( DABS(ATM_OFF) > 1.D-6 ) GOTO 410
         IF ( DABS(ATM_RAT) > 1.D-6 ) GOTO 410
!
         TIM_ARG = (MJD - PIM%MJD_0)*86400.0D0 + (TAI - PIM%TAI_0)
         IND_MOD = 0
!
! ------ Search for the index of the array with polynomial coefficients
!
         DO 420 J2=1,PIM%STA(ISTA)%L_MOD
            IF ( ISOU == PIM%STA(ISTA)%MOD(J2)%SOU_IND .AND. &
!     &           ( TIM_ARG - PIM%STA(ISTA)%MOD(J2)%TIM_BEG > -TIM_TOL .AND. &
!@     &             PIM%STA(ISTA)%MOD(J2)%TIM_END - TIM_ARG > -TIM_TOL       ) ) THEN
     &           DABS( TIM_ARG - PIM%STA(ISTA)%MOD(J2)%TIM_BEG ) < TIM_TOL ) THEN
               IND_MOD = J2
               GOTO 820
            END IF
 420     CONTINUE
 820     CONTINUE
!
         IF ( IND_MOD == 0 ) THEN
              GOTO 410
!              CALL CLRCH ( STR )
!              CALL INCH  ( J1, STR )
!              CALL ERR_LOG ( 7545, IUER, 'PIMA_TST1', 'Cannot '// &
!     &            'find model interval for line '//STR(1:I_LEN(STR))// &
!     &            ' of the model component file '//MDC_FILE )
!              RETURN 
         END IF
!
         TIM_POL  = TIM_ARG - PIM%STA(ISTA)%MOD(IND_MOD)%TIM_BEG
         APR_DEL = PIM%STA(ISTA)%MOD(IND_MOD)%GDEL_POL(PIM__MDPL,1)
         APR_RAT = PIM%STA(ISTA)%MOD(IND_MOD)%PRAT_POL(PIM__MDPL,1)
         DO 430 J3=PIM__MDPL-1,0,-1
            APR_DEL = PIM%STA(ISTA)%MOD(IND_MOD)%GDEL_POL(J3,1) + APR_DEL*TIM_POL
            APR_RAT = PIM%STA(ISTA)%MOD(IND_MOD)%PRAT_POL(J3,1) + APR_RAT*TIM_POL
 430     CONTINUE
!
         NDIF(ISTA) = NDIF(ISTA) + 1
         TIM(NDIF(ISTA),ISTA) = TIM_ARG
         APR_DEL_DIF(NDIF(ISTA),ISTA) = GRP_DEL - APR_DEL + ATM_OFF - CLO_OFF
         APR_RAT_DIF(NDIF(ISTA),ISTA) = GRP_RAT - APR_RAT + ATM_RAT - CLO_RAT
!
!         WRITE ( 6, 110 ) BUF(J1)(19:26), BUF(J1)(57:78), BUF(J1)(209:227), &
!     &                    APR_DEL, APR_DEL_DIF(NDIF(ISTA),ISTA) 
! 110     FORMAT ( A, 2X, A, 2X, A, 2X, 1PD19.12, ' D_delta: ', 1PD19.12 )
!         WRITE ( 6, 120 ) BUF(J1)(19:26), BUF(J1)(57:78), BUF(J1)(242:262), &
!     &                    APR_RAT, APR_RAT_DIF(NDIF(ISTA),ISTA) 
! 120     FORMAT ( A, 2X, A, 2X, A, 2X, 1PD19.12, ' D_delta: ', 1PD19.12 )
 410  CONTINUE 
!
      DO 440 J4=1,PIM%NSTA
         CALL REGR8 ( NDIF(J4), TIM(1,J4), APR_DEL_DIF(1,J4), CLO_DR, CLO_OFF_0, IER )
         VAL0 = APR_DEL_DIF(1,J4) - CLO_OFF_0
         VAL1 = APR_RAT_DIF(1,J4) - CLO_DR
         DO 450 J5=1,NDIF(J4)
            APR_DEL_DIF(J5,J4) = APR_DEL_DIF(J5,J4) - (CLO_OFF_0 + CLO_DR*TIM(J5,J4))
            APR_DEL_DIF(J5,J4) = APR_DEL_DIF(J5,J4) - VAL0
            APR_RAT_DIF(J5,J4) = APR_RAT_DIF(J5,J4) - CLO_DR - VAL1 
 450     CONTINUE 
         write ( 6, * ) 'Sta: ', PIM%C_STA(J4), ' clo_off_0: ', clo_off_0, ' clo_dr= ', clo_dr ! %%%
!         CALL DIAGI_SETDEF ( -2, 'DIAGI_CTIT', 'Apr_del_dif for '//PIM%C_STA(J4) )
!         CALL DIAGI_1 ( NDIF(J4), TIM(1,J4), APR_DEL_DIF(1,J4), -2 )
         CALL DIAGI_SETDEF ( -2, 'DIAGI_CTIT', 'Apr_rat_dif for '//PIM%C_STA(J4) )
         CALL DIAGI_1 ( NDIF(J4), TIM(1,J4), APR_RAT_DIF(1,J4), -2 )
 440  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_TST1  !#!  
