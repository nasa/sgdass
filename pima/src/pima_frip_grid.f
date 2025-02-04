      SUBROUTINE PIMA_FRIP_GRID ( PIM, SCA_TYP, GRID_ALG, VIS_SCL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_FRIP_GRID
! *                                                                      *
! * ### 29-DEC-2011 PIMA_FRIP_GRID v1.0 (c)  L. Petrov  29-DEC-2011 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  SCA_TYP, GRID_ALG, IUER
      REAL*8     VIS_SCL
      REAL*8     PH_RAT, GR_RAT, PHS_MOD, DT, TIM_MIN, TIM_AP, FREQ_REF, &
     &           TIM_FROM_EPOCH
      REAL*4     UV_GRID(2), VIS_STEP, U_MIN, V_MIN, U_SHF, V_SHF, &
     &           CON_U, CON_V, WEI_SUM, TOT_WES, WES_AP, WEI_USED, TOT_WEI_USED
      CHARACTER  STR*128
      COMPLEX*8  VIS, TOT_VIS
      REAL*4       EPS
      PARAMETER  ( EPS = 1.E-4 )
      INTEGER*4  J1, J2, J3, J4, &
     &           LFRQ, FRG_IND, UV_IND, IND_EPC, DIM, IND_U, IND_V, &
     &           IFRQ, IFRQ_VIS, IND_BND, IND_FRA, IND_OBS, &
     &           IND_AF, IU, IV, IST1, IST2, IER 
      REAL*4,    EXTERNAL :: ATAN_CS_R4, PHAS_CMPL_R4
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      VIS_STEP = VIS_SCL/(PIM%CONF%FRIP_RESOLUTION/2)
      DIM = PIM%CONF%FRIP_RESOLUTION
      IND_BND = 1
      PIM%FRIP(SCA_TYP)%MAP = (0.0, 0.0)
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
      IF ( PIM%CONF%DEBUG_LEVEL == 6 ) THEN
           WRITE ( 6, * ) 'PIMA_FRIP_GRID: Vis_step: ', VIS_STEP, &
     &                                   ' Vis_scl: ', VIS_SCL
           WRITE ( 6, * ) 'PIMA_FRIP_GRID: IF_range: ', &
     &                     PIM%CONF%BEG_FRQ, PIM%CONF%END_FRQ, &
     &                    ' NFRQ: ', INT2(PIM%FRIP(SCA_TYP)%NFRQ), &
     &                    ' NAF:  ', INT2(PIM%FRIP(SCA_TYP)%NAF)
      END IF 
      IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_DRF ) THEN
           IND_FRA = PIMA__DRF
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_LSQ ) THEN
           IND_FRA = PIMA__LSQ
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_MUL ) THEN
           IND_FRA = PIMA__MUL
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_ADD ) THEN
           IND_FRA = PIMA__ADD
      END IF
!
      IF ( .NOT. ( PIM%FRIP(SCA_TYP)%AF_STATUS == PIMA__LOADED .OR. &
     &             PIM%FRIP(SCA_TYP)%AF_STATUS == PIMA__UPDATED     ) ) THEN
           CALL ERR_LOG ( 9231, IUER, 'PIMA_FRIP_GRID', 'Trap of '// &
     &         'internal control: AF visibilities were not computed' )
           RETURN
      END IF
!
      TOT_WEI_USED = 0.0
      DO 410 J1=1,PIM%FRIP(SCA_TYP)%NOBS
         IF ( .NOT. PIM%FRIP(SCA_TYP)%USED(J1) ) GOTO 410
         IST1 = PIM%FRIP(SCA_TYP)%IND_STA(1,J1)
         IST2 = PIM%FRIP(SCA_TYP)%IND_STA(2,J1)
!
         DO 420 J2=1,LFRQ
            IF ( PIM%FRIP(SCA_TYP)%WEI_AF(J2,J1) < PIMA__WEI_MIN ) GOTO 420
            WEI_USED = 1.D0/PIM%FRIP(SCA_TYP)%WEI_AF(J2,J1)
            TOT_WEI_USED = TOT_WEI_USED + WEI_USED 
            IND_U = NINT ( PIM%FRIP(SCA_TYP)%UVW_AF(PIMA__U,J2,J1)/VIS_STEP )
            IND_V = NINT ( PIM%FRIP(SCA_TYP)%UVW_AF(PIMA__V,J2,J1)/VIS_STEP )
            UV_GRID(PIMA__U) = IND_U*VIS_STEP
            UV_GRID(PIMA__V) = IND_V*VIS_STEP
            U_SHF = ( PIM%FRIP(SCA_TYP)%UVW_AF(PIMA__U,J2,J1) - UV_GRID(PIMA__U))/VIS_STEP
            V_SHF = ( PIM%FRIP(SCA_TYP)%UVW_AF(PIMA__V,J2,J1) - UV_GRID(PIMA__V))/VIS_STEP
            IF ( U_SHF <  EPS .AND. U_SHF .GE. 0.0 ) U_SHF =  EPS
            IF ( U_SHF > -EPS .AND. U_SHF .LE. 0.0 ) U_SHF = -EPS
            IF ( V_SHF <  EPS .AND. V_SHF .GE. 0.0 ) V_SHF =  EPS
            IF ( V_SHF > -EPS .AND. V_SHF .LE. 0.0 ) V_SHF = -EPS
            IF ( PIM%CONF%DEBUG_LEVEL == 12 ) THEN
                 WRITE ( 6, 210 ) PIM%FRIP(SCA_TYP)%OBS(J1)%IND_OBS, J2, &
     &                            PIM%FRIP(SCA_TYP)%UVW_AF(PIMA__U,J2,J1), &
     &                            PIM%FRIP(SCA_TYP)%UVW_AF(PIMA__V,J2,J1)
 210             FORMAT ( 'IND_OBS: ', I5,' IFRQ: ', I2, &
     &                    ' U= ', 1PD15.7, ' V= ', 1PD15.7 )
            END IF
!
            IND_U = IND_U - PIM%CONF%FRIP_RESOLUTION/2
            IND_V = IND_V - PIM%CONF%FRIP_RESOLUTION/2
!
            DO 430 J3=-PIMA__EXS_NCON,PIMA__EXS_NCON
               IF ( GRID_ALG == PIMA__GRID_EXS ) THEN
                    IF ( ABS(V_SHF-J3) < EPS ) THEN
                         CON_V = 1.0
                       ELSE 
                         CON_V = SIN ( PIMA__EXS_SINC_COE*(V_SHF-J3) )/ &
     &                               ( PIMA__EXS_SINC_COE*(V_SHF-J3) )
                    END IF
                    CON_V = CON_V*EXP( -(J3-V_SHF)**2/(2.0*PIMA__EXS_EXP_COE**2) )/ &
     &                      (SQRT(PI2)*PIMA__EXS_EXP_COE*PIMA__EXS_NRM)
                 ELSE IF ( GRID_ALG == PIMA__GRID_PIL ) THEN
                    IF ( ABS(V_SHF-J3) < 0.5 ) THEN
                         CON_V = 1.0
                       ELSE 
                         CON_V = 0.0
                    END IF
               END IF
!
               DO 440 J4=-PIMA__EXS_NCON,PIMA__EXS_NCON
                  IF ( GRID_ALG == PIMA__GRID_EXS ) THEN
                       IF ( ABS(U_SHF-J4) < EPS ) THEN
                            CON_U = 1.0
                          ELSE 
                            CON_U = SIN ( PIMA__EXS_SINC_COE*(U_SHF-J4) )/ &
     &                                  ( PIMA__EXS_SINC_COE*(U_SHF-J4) )
                       END IF
                       CON_U = CON_U*EXP( -(U_SHF-J4)**2/(2.0*PIMA__EXS_EXP_COE**2) )/ &
     &                         (SQRT(PI2)*PIMA__EXS_EXP_COE*PIMA__EXS_NRM)
                     ELSE IF ( GRID_ALG == PIMA__GRID_PIL ) THEN
                       IF ( ABS(V_SHF-J3) < 0.5 ) THEN
                            CON_U = 1.0
                          ELSE 
                            CON_U = 0.0
                       END IF
                  END IF
!
                  IU = IND_U + J4 + 1
                  IV = IND_V + J3 + 1
                  IF ( IU < 1 ) IU = IU + PIM%CONF%FRIP_RESOLUTION
                  IF ( IV < 1 ) IV = IV + PIM%CONF%FRIP_RESOLUTION
!
                  PIM%FRIP(SCA_TYP)%MAP(IU,IV) = PIM%FRIP(SCA_TYP)%MAP(IU,IV) &
     &                              + CON_U*CON_V* &
     &                                PIM%FRIP(SCA_TYP)%VIS_AF(J2,J1)*        &
     &                                CONJG(PIM%FRIP(SCA_TYP)%GAIN(J2,IST1))* &
     &                                      PIM%FRIP(SCA_TYP)%GAIN(J2,IST2)*  &
     &                                WEI_USED/2.0
!
                  IF ( IU .NE. 1 ) IU = PIM%CONF%FRIP_RESOLUTION + 2 - IU
                  IF ( IV .NE. 1 ) IV = PIM%CONF%FRIP_RESOLUTION + 2 - IV
                  PIM%FRIP(SCA_TYP)%MAP(IU,IV) = PIM%FRIP(SCA_TYP)%MAP(IU,IV) &
     &                              + CON_U*CON_V* &
     &                                CONJG(PIM%FRIP(SCA_TYP)%VIS_AF(J2,J1))* &
     &                                      PIM%FRIP(SCA_TYP)%GAIN(J2,IST1)*  &
     &                                CONJG(PIM%FRIP(SCA_TYP)%GAIN(J2,IST2))* &
     &                                WEI_USED/2.0
 440          CONTINUE 
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
      PIM%FRIP(SCA_TYP)%MAP = PIM%FRIP(SCA_TYP)%MAP/TOT_WEI_USED
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FRIP_GRID   !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_GRID_CORR ( DIM, ARR_C2, GRID_ALG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_GRID_CORR 
! *                                                                      *
! * ### 06-JAN-2012  PIMA_GRID_CORR  v1.0 (c) L. Petrov  06-JAN-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  DIM, GRID_ALG, IUER
      COMPLEX*8  ARR_C2(DIM,DIM)
      COMPLEX*8, ALLOCATABLE :: ARR_C1(:)
      CHARACTER  STR*128
      REAL*4     CNV
      INTEGER*4  J1, J2, J3, J4, KP, IU, N_CON, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      ALLOCATE ( ARR_C1(DIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( DIM*8, STR )
           CALL ERR_LOG ( 9141, IUER, 'PIMA_GRID_CORR', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes dynamic '// &
     &         'memory' )
           RETURN 
      END IF
!
      ARR_C1 = (0.0, 0.0)
      DO 410 J1=-PIMA__EXS_NCON,PIMA__EXS_NCON
         IU = J1 + 1
         IF ( IU < 1 ) IU = DIM+1+J1
         IF ( J1 == 0 ) THEN
              CNV = 1.0
            ELSE
              CNV = SIN ( J1*PIMA__EXS_SINC_COE )/( J1*PIMA__EXS_SINC_COE )
         END IF
         IF ( GRID_ALG == PIMA__GRID_EXS ) THEN
              CNV = CNV * EXP( -FLOAT(J1)**2/(2.0D0*PIMA__EXS_EXP_COE**2) )/ &
     &                       (SQRT(PI2)*PIMA__EXS_EXP_COE*PIMA__EXS_NRM)
         END IF
         ARR_C1(IU) = CMPLX ( CNV, 0.0 )
 410  CONTINUE
!
      CALL ERR_PASS  ( IUER, IER )
      CALL FFT_1D_C8 ( DIM, -1, ARR_C1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9142, IUER, 'PIMA_GRID_CORR', 'Failure '// &
     &         'in an attempt to run inverse fast Fourier transform' )
           RETURN 
      END IF
!
      IF ( GRID_ALG == PIMA__GRID_EXS ) THEN
           DO 420 J2=1,DIM
              DO 430 J3=1,DIM
                 ARR_C2(J3,J2) = ARR_C2(J3,J2)/(ARR_C1(J3)*ARR_C1(J2))
 430          CONTINUE 
 420       CONTINUE 
      END IF
!
      DEALLOCATE ( ARR_C1 )
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE   PIMA_GRID_CORR  !#!  
