      SUBROUTINE PIMA_CHPO ( PIM, METRIC, LEVEL, IUER )
! ************************************************************************
! *                                                                      *
! *   Program PIMA_CHPO checks results of fringe fitting in the mode     *
! *   when all combinations of polarizations have been computed.         *
! *   It provides statistics of amplitude losses due to imperfection     *
! *   of polarization bandpass and provides the lis of outliers.         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * PIM    ( PIMA__TYP ) -- Object with information related to program   *
! *                         PIMA.                                        *
! * METRIC ( CHARACTER ) -- operation mode. Supported modes:             *
! * LEVEL  ( CHARACTER ) -- Level of details. Supported levels:          *
! *                         summary     -- provides statistics for each  *
! *                                        baseline.                     *
! *                         observation -- provides infromation for each *
! *                                        observation if a given metric *
! *                                        can be computed.              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the case of errors. *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ###  02-MAY-2026    PIMA_CHPO   v1.0 (d) L. Petrov  18-MAY-2026 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'pima_local.i'
      TYPE     ( PIMA__TYPE     ) :: PIM
      INTEGER*4  IUER
      INTEGER*4    MBUF__EXTRA, MIND
      PARAMETER  ( MBUF__EXTRA = 256 )
      PARAMETER  ( MIND = 256 )
      CHARACTER  METRIC*(*), LEVEL*(*)
      CHARACTER*1536, ALLOCATABLE :: BUF(:)
      CHARACTER  POLAR_FRI*8, POLAR_VAL*2, STR*128, &
     &           C_BAS(PIM__MBAS)*17, C_BAS_SORTED(PIM__MBAS)*17, &
     &           BAS_ARR(PIM__MOBS)*17, C_SOU(PIM__MOBS)*8
      INTEGER*4  IND_OBS, IND_SCA, IND_SOU, IND_STA(2), FRI_STS_VAL, IND_OBS_BAS
      REAL*8     SNR_VAL, AMPL_VAL(PIM__MFRA), AMPL_INTG, &
     &           TIME_FRT_VAL, GR_DEL_VAL(PIM__MFRA), &
     &           PH_RAT_VAL(PIM__MFRA), GR_RAT_VAL, PH_ACC_VAL, &
     &           SB_DEL, PHS_VAL(PIM__MFRA), GR_DEL_ERR(PIM__MFRA), &
     &           PH_RAT_ERR(PIM__MFRA), GR_RAT_ERR, PH_ACC_ERR, &
     &           SB_DEL_ERR, PH_DEL_ERR(PIM__MFRA), GRAMBSP, SCAN_DUR, &
     &           AP_LEN, FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &           EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD, &
     &           TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &           FEED_HORN, PA_USED, DECOR_TIM, PCAL_GDEL
      REAL*8     AMPL_ARR(PIM__MPLR+1,PIM__MOBS), SNR_ARR(PIM__MPLR+1,PIM__MOBS), &
     &           NOI_ARR(PIM__MPLR+1,PIM__MOBS), PHS_ARR(PIM__MPLR+1,PIM__MOBS), &
     &           GR_DEL(PIM__MPLR+1,PIM__MOBS), AMP_PAR, SNR_MIN
      REAL*8     RAT_AMPL(PIM__MSCA,PIM__MBAS), RAT_AMPL_IND(PIM__MSCA,PIM__MBAS), &
     &           RAT_AMPL_IND_SORTED(PIM__MSCA,PIM__MBAS), &
     &           RAT_AMPL_AVR(PIM__MBAS), RAT_AMPL_SQR(PIM__MBAS), RAT_AMPL_RMS(PIM__MBAS)
      REAL*8     CRS_AMPL(PIM__MSCA,PIM__MBAS), CRS_AMPL_IND(PIM__MSCA,PIM__MBAS), &
     &           CRS_AMPL_AVR(PIM__MBAS), CRS_AMPL_SQR(PIM__MBAS), CRS_AMPL_RMS(PIM__MBAS), &
     &           I_LOSS(PIM__MSCA,PIM__MBAS), I_LOSS_IND(PIM__MSCA,PIM__MBAS), &
     &           I_LOSS_AVR(PIM__MBAS), I_LOSS_MED(PIM__MBAS), I_LOSS_RMS(PIM__MBAS), &
     &           I_LOSS_SRT(PIM__MSCA), I_LOSS_IND_SRT(PIM__MSCA)
!
      REAL*8     PHS_DIFF(PIM__MSCA,PIM__MBAS), PHS_DIFF_IND(PIM__MSCA,PIM__MBAS), &
     &           PHS_DIFF_AVR(PIM__MBAS), PHS_DIFF_SQR(PIM__MBAS), PHS_DIFF_RMS(PIM__MBAS), &
     &           PHS_DIFF_SRT(PIM__MOBS), PHS_DIFF_IND_SRT(PIM__MOBS), SNR_PAR, &
     &           GRP_DIFF(PIM__MSCA,PIM__MBAS), GRP_DIFF_SRT(PIM__MOBS)
      REAL*8     SNR__FCT, CRS__MIN, PHS_SGNF, PHS_THRS, EPS
      PARAMETER  ( SNR__FCT  = 1.5D0  )
      PARAMETER  ( EPS       = 1.0D-6 )
      PARAMETER  ( CRS__MIN  = 0.05   )
      PARAMETER  ( PHS_SGNF  = 3.0    )
      PARAMETER  ( PHS_THRS  = 0.2   )
      INTEGER*4  FRI_STS(PIM__MPLR+1,PIM__MOBS)
      CHARACTER  POLAR_LL_CODES(PIM__MPLR)*2, POLAR_CC_CODES(PIM__MPLR)*2, &
     &           POLAR_LC_CODES(PIM__MPLR)*2, POLAR_CL_CODES(PIM__MPLR)*2
      DATA       POLAR_CC_CODES  / 'RR', 'LR', 'RL', 'LL' /
      DATA       POLAR_LL_CODES  / 'HH', 'VH', 'HV', 'VV' /
      DATA       POLAR_LC_CODES  / 'HR', 'VR', 'HL', 'VL' /
      DATA       POLAR_CL_CODES  / 'RH', 'LH', 'RV', 'LV' /
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           PCI, LIND, IND(2,MIND), NBUF, IND_FRA, GOBS, &
     &           L_BAS, I_BAS, K_BAS(PIM__MBAS), IER
      INTEGER*4, EXTERNAL :: ADD_CLIST, LTM_DIF
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
      ALLOCATE ( BUF(PIM__MOBS+MBUF__EXTRA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (PIM__MOBS+MBUF__EXTRA)*SIZEOF(BUF(1)), STR )
           CALL ERR_LOG ( 6411, IUER, 'PIMA_CHPO', 'Error in an '// &
     &         'attempt to allocate '//TRIM(STR)//' bytes of '// &
     &         'dynamic memory for the fringe file contents buffer' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( PIM%CONF%FRINGE_FILE, PIM__MOBS, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG (  6412, IUER, 'PIMA_CHPO', 'Error in an '// &
     &         'attempt to read file '//PIM%CONF%FRINGE_FILE )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
      IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FRA_DRF ) THEN
           IND_FRA = PIMA__DRF
         ELSE IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FRA_LSQ ) THEN
           IND_FRA = PIMA__LSQ
         ELSE IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FRA_MUL ) THEN
           IND_FRA = PIMA__MUL
         ELSE IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FRA_ADD ) THEN
           IND_FRA = PIMA__ADD
      END IF
      SNR_MIN = SNR__FCT*PIM%CONF%FRIB_SNR_DETECTION
!
      AMPL_ARR = 0.0D0
      SNR_ARR  = 0.0D0
      NOI_ARR  = 0.0D0
      PHS_ARR  = 0.0D0
      GR_DEL   = 0.0D0
      FRI_STS  = 0
!
      POLAR_FRI = '????????'
!
! --- Scan the frine file and collect information about fringe ampliddes,
! --- nosie, SNR, group and phase delays, and fringe status
!
      DO 410 J1=1,NBUF
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(32)//CHAR(9), IER )
         IF ( LIND < 3 ) GOTO 410
         IF ( BUF(J1)(1:13) == '# FRIB.POLAR:' ) THEN
              POLAR_FRI = BUF(J1)(IND(1,3):IND(2,3))
              IF ( POLAR_FRI == PIMA__POLAR_ALI ) THEN
                   CONTINUE 
                 ELSE IF ( POLAR_FRI == PIMA__POLAR_ALL ) THEN
                   IF ( METRIC == 'i_loss' ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG (  6413, IUER, 'PIMA_CHPO', 'Wrong polarization '// &
     &                     'type specified in the '//TRIM(STR)//'-th line of '// &
     &                     'the file '//TRIM(PIM%CONF%FRINGE_FILE)//' . Mode i_loss '// &
     &                     ' requires polarization type ALI' )
                        DEALLOCATE ( BUF )
                        RETURN 
                   END IF
                 ELSE
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG (  6413, IUER, 'PIMA_CHPO', 'Wrong polarization '// &
     &                 'type specified in the '//TRIM(STR)//'-th line of '// &
     &                 'the file '//TRIM(PIM%CONF%FRINGE_FILE)//' . It is '// &
     &                  POLAR_FRI//' while ALL is expected' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
         END IF 
         IF ( BUF(J1)(1:1) .NE. '#' ) THEN
              IF ( POLAR_FRI == '????????' ) THEN
                   CALL ERR_LOG (  6414, IUER, 'PIMA_CHPO', 'Polarization '// &
     &                 'was not specified in the header of '// &
     &                 'the file '//PIM%CONF%FRINGE_FILE )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
! ----------- Parse the line of the fringe file
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_FRI_REA_OBS ( PIM, BUF(J1), IND_OBS, &
     &             IND_SCA, IND_SOU, IND_STA, SNR_VAL, AMPL_VAL, &
     &             AMPL_INTG, TIME_FRT_VAL, GR_DEL_VAL, &
     &             PH_RAT_VAL, GR_RAT_VAL, PH_ACC_VAL, SB_DEL, PHS_VAL, &
     &             GR_DEL_ERR, PH_RAT_ERR, GR_RAT_ERR, PH_ACC_ERR, &
     &             SB_DEL_ERR, PH_DEL_ERR, GRAMBSP, SCAN_DUR, &
     &             AP_LEN, FREQ_REF, EFF_FRQ_PHS, EFF_FRQ_GRP, &
     &             EFF_FRQ_RAT, COV_PR_PH, COV_GR_MD, &
     &             TEC, TEC_RATE, TEC_ERR, TEC_RATE_ERR, &
     &             POLAR_VAL, FEED_HORN, PA_USED, &
     &             DECOR_TIM, PCAL_GDEL, FRI_STS_VAL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6415, IUER, 'PIMA_CHPO', 'Error '// &
     &                 'an attempt to parse the '//TRIM(STR)//'-th line of '// &
     &                 'the fringe file '//PIM%CONF%FRINGE_FILE )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( BUF(J1)(1:1) == '#' ) GOTO 410
              IF ( POLAR_VAL == '??'   ) GOTO 410 ! This means the was a failure in fringe fitting
              CALL CHIN ( BUF(J1)(1:6), IND_OBS )
!
! ----------- Get the polarization code of a given observation
!
              PCI = LTM_DIF ( 0, PIM__MPLR, POLAR_CC_CODES, POLAR_VAL )
              IF ( PCI < 1 ) THEN
                   PCI = LTM_DIF ( 0, PIM__MPLR, POLAR_LL_CODES, POLAR_VAL )
                   IF ( PCI < 1 ) THEN
                        PCI = LTM_DIF ( 0, PIM__MPLR, POLAR_CL_CODES, POLAR_VAL )
                        IF ( PCI < 1 ) THEN
                             PCI = LTM_DIF ( 0, PIM__MPLR, POLAR_LC_CODES, POLAR_VAL )
                        END IF
                   END IF
              END IF
              IF ( POLAR_VAL == PIMA__POLAR_I ) PCI = PIM__MPLR+1
              IF ( PCI < 1 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6416, IUER, 'PIMA_CHPO', 'Unsupported '// &
          &            'polarizaton code '//POLAR_VAL//' in line '// &
          &             TRIM(STR)//' of the fringe file '//PIM%CONF%FRINGE_FILE )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
! ----------- Extract amplitude, SNR, noise, group delays etc
!
              AMPL_ARR(PCI,IND_OBS) = AMPL_VAL(IND_FRA)
              SNR_ARR(PCI,IND_OBS)  = SNR_VAL
              NOI_ARR(PCI,IND_OBS)  = AMPL_VAL(IND_FRA)/MAX(SNR_VAL,0.1D0)
              PHS_ARR(PCI,IND_OBS)  = PHS_VAL(IND_FRA)
              GR_DEL(PCI,IND_OBS)   = GR_DEL_VAL(IND_FRA)
              FRI_STS(PCI,IND_OBS)  = FRI_STS_VAL
              BAS_ARR(IND_OBS)      = BUF(J1)(33:40)//'/'//BUF(J1)(42:49)
              I_BAS = ADD_CLIST ( PIM__MBAS, L_BAS, C_BAS, BAS_ARR(IND_OBS), IER )
              C_SOU(IND_OBS) = BUF(J1)(24:31)
         END IF
 410  CONTINUE 
!
      WRITE ( 6, 110 ) PIMA__VERSION, GET_CDATE()
 110  FORMAT ( 'PIMA ', A, ' task chpo executed at ', A )
!
      GOBS = 0
      RAT_AMPL = 0.0D0
      RAT_AMPL_AVR = 0.0D0
      RAT_AMPL_SQR = 0.0D0
      CRS_AMPL = 0.0D0
      CRS_AMPL_AVR = 0.0D0
      CRS_AMPL_SQR = 0.0D0
      PHS_DIFF = 0.0D0
      GRP_DIFF = 0.0D0
      PHS_DIFF_AVR = 0.0D0
      PHS_DIFF_SQR = 0.0D0
      K_BAS = 0
      DO 420 J2=1,PIM%NOBS
         IF ( FRI_STS(1,J2) == 2      .AND. FRI_STS(4,J2) == 2      .AND. &
     &        SNR_ARR(1,J2) > SNR_MIN .AND. SNR_ARR(4,J2) > SNR_MIN       ) THEN
!
! ----------- This observation had SNR at both parallel polarization above 
! ----------- the limit
!
              GOBS = GOBS + 1
              I_BAS = LTM_DIF ( 0, L_BAS, C_BAS, BAS_ARR(J2) )
              K_BAS(I_BAS) = K_BAS(I_BAS) + 1
              RAT_AMPL(K_BAS(I_BAS),I_BAS) = AMPL_ARR(4,J2)/AMPL_ARR(1,J2)
              RAT_AMPL_IND(K_BAS(I_BAS),I_BAS) = J2 + 1.0D-8
              RAT_AMPL_AVR(I_BAS) = RAT_AMPL_AVR(I_BAS) + RAT_AMPL(K_BAS(I_BAS),I_BAS)
              RAT_AMPL_SQR(I_BAS) = RAT_AMPL_SQR(I_BAS) + RAT_AMPL(K_BAS(I_BAS),I_BAS)**2
!
! ----------- Get the amplitude of the parallel polarization if phase the bandpass
! ----------- were perfect
!
              AMP_PAR = DSQRT ( AMPL_ARR(1,J2)**2  + AMPL_ARR(4,J2)**2 )
              IF (SNR_ARR(2,J2)  < SNR_MIN ) AMPL_ARR(2,J2) = 0.0
              IF ( SNR_ARR(3,J2) < SNR_MIN ) AMPL_ARR(3,J2) = 0.0
!
! ----------- Get the ratio of the cross polarization ampltiude to the parallel amplitude
!
              CRS_AMPL(K_BAS(I_BAS),I_BAS) = DSQRT ( AMPL_ARR(2,J2)**2  + AMPL_ARR(3,J2)**2 )/AMP_PAR
              CRS_AMPL_IND(K_BAS(I_BAS),I_BAS) = J2 + 1.0D-8
              CRS_AMPL_AVR(I_BAS) = CRS_AMPL_AVR(I_BAS) + CRS_AMPL(K_BAS(I_BAS),I_BAS)
              CRS_AMPL_SQR(I_BAS) = CRS_AMPL_SQR(I_BAS) + CRS_AMPL(K_BAS(I_BAS),I_BAS)**2
!
! ----------- Get the loss of I-polrization SNR
!
              I_LOSS(K_BAS(I_BAS),I_BAS) = 1.0D0 - SNR_ARR(5,J2)/ &
     &                                             DSQRT ( SNR_ARR(1,J2)**2 + SNR_ARR(4,J2)**2 )
     &                                     
              I_LOSS_IND(K_BAS(I_BAS),I_BAS) = J2 + 1.0D-8
!
! ----------- Get the phase differences in parallel visibilities
!
              PHS_DIFF(K_BAS(I_BAS),I_BAS) = PHS_ARR(4,J2) - PHS_ARR(1,J2) 
              GRP_DIFF(K_BAS(I_BAS),I_BAS) = GR_DEL(4,J2)  - GR_DEL(1,J2) 
              IF ( PHS_DIFF(K_BAS(I_BAS),I_BAS) >  PI__NUM ) PHS_DIFF(K_BAS(I_BAS),I_BAS) = PHS_DIFF(K_BAS(I_BAS),I_BAS) - PI2
              IF ( PHS_DIFF(K_BAS(I_BAS),I_BAS) < -PI__NUM ) PHS_DIFF(K_BAS(I_BAS),I_BAS) = PHS_DIFF(K_BAS(I_BAS),I_BAS) + PI2
              PHS_DIFF_IND(K_BAS(I_BAS),I_BAS) = J2 + 1.0D-8
              PHS_DIFF_AVR(I_BAS) = PHS_DIFF_AVR(I_BAS) + PHS_DIFF(K_BAS(I_BAS),I_BAS)
              PHS_DIFF_SQR(I_BAS) = PHS_DIFF_SQR(I_BAS) + PHS_DIFF(K_BAS(I_BAS),I_BAS)**2
         END IF
 420  CONTINUE 
!
      C_BAS_SORTED(1:L_BAS) = C_BAS(1:L_BAS) 
      CALL SORT_CH ( L_BAS, C_BAS_SORTED )
!
      WRITE ( 6, 120 ) TRIM(PIM%CONF%SESS_CODE), TRIM(PIM%CONF%BAND), &
     &                 PIM%NOBS, GOBS, GOBS/FLOAT(PIM%NOBS)
 120  FORMAT ( 'Session: ', A8, ' Band: ', A, ' NOBS: ', I6, &
     &         ' Good_obs: ', I6, ' Good_fraction: ', F5.3 )
      IF ( METRIC == 'par_ampl' ) THEN
           WRITE ( 6, '(A)' ) '#'
           DO 430 J3=1,L_BAS
              I_BAS = LTM_DIF ( 0, L_BAS, C_BAS, C_BAS_SORTED(J3) )
              IF ( K_BAS(I_BAS) > 2 ) THEN
!
! ---------------- Compute statistics: average, rms, and median
!
                   RAT_AMPL_AVR(I_BAS) = RAT_AMPL_AVR(I_BAS)/K_BAS(I_BAS)
                   RAT_AMPL_RMS(I_BAS) = DSQRT ( RAT_AMPL_SQR(I_BAS)/K_BAS(I_BAS) - RAT_AMPL_AVR(I_BAS)**2 + EPS**2 )
                   CALL SORT8 ( K_BAS(I_BAS), RAT_AMPL(1,I_BAS),  RAT_AMPL_IND(1,I_BAS) )
                   IF ( LEVEL(1:3) == 'sum' ) THEN
                        WRITE ( 6, 130 ) C_BAS(I_BAS), K_BAS(I_BAS), RAT_AMPL(1,I_BAS), &
     &                                   RAT_AMPL(K_BAS(I_BAS)/2,I_BAS), RAT_AMPL(K_BAS(I_BAS),I_BAS), &
     &                                   RAT_AMPL_AVR(I_BAS), RAT_AMPL_RMS(I_BAS)
 130                    FORMAT ( 'Par_ampl_ratio Baseline ', A, ' Nobs: ', I6, &
     &                           ' Rat_min: ', F5.3, ' Rat_med: ', F5.3, ' Rat_max: ', F5.3, &
     &                           ' Rat_avr: ', F5.3, ' Rat_rms: ', F5.3 )
                     ELSE IF ( LEVEL(1:3) == 'obs' ) THEN
                        DO 440 J4=K_BAS(I_BAS),1,-1
                           IND_OBS = INT(RAT_AMPL_IND(J4,I_BAS))
                           WRITE ( 6, 140 ) C_BAS(I_BAS), IND_OBS, C_SOU(IND_OBS), &
     &                                      SNR_ARR(5,IND_OBS), RAT_AMPL(J4,I_BAS)
  140                      FORMAT ( '  par_ampl_ratio ', A, 1X, I6, 1X, A, &
     &                              ' SNR_I: ', F9.1, ' Ampl_rat: ', F6.3 )
  440                   CONTINUE 
                        WRITE ( 6, '(A)' ) '#'
                   END IF
              END IF
 430       CONTINUE 
      END IF
!
! --- Print statistics of cross to parallel fringe ampltide ratios per baseline
!
      IF ( METRIC == 'cross_ampl' ) THEN
           DO 450 J5=1,L_BAS
              I_BAS = LTM_DIF ( 0, L_BAS, C_BAS, C_BAS_SORTED(J5) )
              IF ( K_BAS(I_BAS) > 2 ) THEN
                   CRS_AMPL_AVR(I_BAS) = CRS_AMPL_AVR(I_BAS)/K_BAS(I_BAS)
                   CRS_AMPL_RMS(I_BAS) = DSQRT ( CRS_AMPL_SQR(I_BAS)/K_BAS(I_BAS) - CRS_AMPL_AVR(I_BAS)**2 + EPS**2 )
                   CALL SORT8 ( K_BAS(I_BAS), CRS_AMPL(1,I_BAS),  CRS_AMPL_IND(1,I_BAS) )
                   IF ( LEVEL(1:3) == 'sum' ) THEN
                        WRITE ( 6, 150 ) C_BAS(I_BAS), K_BAS(I_BAS), &
     &                                   CRS_AMPL(K_BAS(I_BAS)/2,I_BAS), CRS_AMPL(K_BAS(I_BAS),I_BAS), &
     &                                   CRS_AMPL_AVR(I_BAS), CRS_AMPL_RMS(I_BAS), &
     &                                   INT(CRS_AMPL_IND(K_BAS(I_BAS),I_BAS)), &
     &                                   PIM%C_SOU(PIM%OBS( INT(CRS_AMPL_IND(K_BAS(I_BAS),I_BAS)))%SOU_IND)
 150                    FORMAT ( 'Crs_to_par_ratio. Baseline ', A, ' Nobs: ', I6, &
     &                           ' Crs_med: ', F5.3, ' Crs_max: ', F5.3, &
     &                           ' Crs_avr: ', F5.3, ' Crs_rms: ', F5.3 )
                      ELSE IF ( LEVEL(1:3) == 'obs' ) THEN
                        DO 460 J6=K_BAS(I_BAS),1,-1
                           IF ( CRS_AMPL(J6,I_BAS) > CRS__MIN ) THEN
                                IND_OBS = INT(CRS_AMPL_IND(J6,I_BAS))
                                SNR_PAR = DSQRT ( SNR_ARR(1,IND_OBS)**2 + SNR_ARR(4,IND_OBS)**2 )
                                WRITE ( 6, 160 ) K_BAS(I_BAS)+1-J5, C_BAS(I_BAS), C_SOU(IND_OBS), &
     &                                           IND_OBS, SNR_PAR, CRS_AMPL(J6,I_BAS)
 160                            FORMAT ( '  cross_to_par_ratio i= ', i4, ' bas: ', A, ' Sou: ', A, &
     &                                   ' ind_obs: ', I6, ' snr_par: ', F7.1, ' crs_ampl: ', F5.3 )
                           END IF
 460                    CONTINUE 
                        WRITE ( 6, '(A)' ) '#'
                   END IF
              END IF
 450       CONTINUE 
      END IF
      IF ( METRIC == 'par_phas' ) THEN
           WRITE ( 6, '(A)' ) '#'
!
! -------- Print statatistics of phase differenes in parallel polarization visibilities
!
           DO 470 J7=1,L_BAS
              I_BAS = LTM_DIF ( 0, L_BAS, C_BAS, C_BAS_SORTED(J7) )
              IF ( K_BAS(I_BAS) > 2 ) THEN
                   DO 480 J8=K_BAS(I_BAS),1,-1
                      PHS_DIFF_SRT(J8)     = PHS_DIFF(J8,I_BAS)
                      GRP_DIFF_SRT(J8)     = GRP_DIFF(J8,I_BAS)
                      PHS_DIFF_IND_SRT(J8) = PHS_DIFF_IND(J8,I_BAS) + 1.D-8
 480               CONTINUE 
!
! ---------------- Compute the average, rms, and median
!
                   PHS_DIFF_AVR(I_BAS) = PHS_DIFF_AVR(I_BAS)/K_BAS(I_BAS)
                   PHS_DIFF_RMS(I_BAS) = DSQRT ( PHS_DIFF_SQR(I_BAS)/K_BAS(I_BAS) - PHS_DIFF_AVR(I_BAS)**2 + EPS**2 )
                   CALL SORT83 ( K_BAS(I_BAS), PHS_DIFF_SRT, GRP_DIFF_SRT, PHS_DIFF_IND_SRT )
!
                   IF ( LEVEL(1:3) == 'sum' ) THEN
                        WRITE ( 6, 170 ) C_BAS(I_BAS), K_BAS(I_BAS), PHS_DIFF_SRT(1), &
     &                                   PHS_DIFF_SRT(K_BAS(I_BAS)/2), PHS_DIFF_SRT(K_BAS(I_BAS)), &
     &                                   PHS_DIFF_AVR(I_BAS), PHS_DIFF_RMS(I_BAS) 
 170                    FORMAT ( 'Par_phas_dif Baseline ', A, ' Nobs: ', I6, &
     &                            ' Phs_dif_min: ', F6.3, ' Phs_dif_med: ', F6.3, ' Phs_dif_max: ', F6.3, &
     &                            ' Phs_dif_avr: ', F6.3, ' Phs_dif_rms: ', F6.3 )
                      ELSE IF ( LEVEL(1:3) == 'obs' ) THEN
                         DO 490 J9=K_BAS(I_BAS),1,-1
                            PHS_DIFF_SRT(J9)     = ABS(PHS_DIFF(J9,I_BAS))
                            PHS_DIFF_IND_SRT(J9) = PHS_DIFF_IND(J9,I_BAS) + 1.D-8
 490                     CONTINUE 
                         CALL SORT83 ( K_BAS(I_BAS), PHS_DIFF_SRT, GRP_DIFF_SRT, PHS_DIFF_IND_SRT )
!
                         DO 4100 J10=K_BAS(I_BAS),1,-1
                            IND_OBS = INT(PHS_DIFF_IND_SRT(J10))
                            SNR_PAR = DSQRT ( SNR_ARR(1,IND_OBS)**2 + SNR_ARR(4,IND_OBS)**2 )
                            WRITE ( 6, 180 ) K_BAS(I_BAS)+1-J10, C_BAS(I_BAS), C_SOU(IND_OBS), &
     &                                       IND_OBS, SNR_PAR, PHS_DIFF_SRT(J10), &
     &                                       1.D9*GRP_DIFF_SRT(J10)
 180                        FORMAT ( '  par_phas_dif i= ', i4, ' bas: ', A, ' Sou: ', A, &
     &                               ' ind_obs: ', I6, ' snr_par: ', F7.1, &
     &                               ' phs_diff: ', F6.3, ' grp_diff: ', F12.3, ' ns' )
 4100                     CONTINUE 
                          WRITE ( 6, '(A)' ) '#'
                   END IF
              END IF
 470       CONTINUE 
      END IF
!
      IF ( METRIC == 'i_loss' ) THEN
           DO 4110 J11=1,L_BAS
              I_BAS = LTM_DIF ( 0, L_BAS, C_BAS, C_BAS_SORTED(J11) )
              I_LOSS_AVR(I_BAS) = 0.0
              I_LOSS_RMS(I_BAS) = 0.0
              IF ( K_BAS(I_BAS) > 2 ) THEN
                   DO 4120 J12=1,K_BAS(I_BAS)
                      I_LOSS_SRT(J12) = -I_LOSS(J12,I_BAS)
                      I_LOSS_IND_SRT(J12) = J12 + 1.D-8
                      I_LOSS_AVR(I_BAS) = I_LOSS_AVR(I_BAS) + I_LOSS(J12,I_BAS)
                      I_LOSS_RMS(I_BAS) = I_LOSS_RMS(I_BAS) + I_LOSS(J12,I_BAS)**2
 4120              CONTINUE 
                   I_LOSS_AVR(I_BAS) = I_LOSS_AVR(I_BAS)/K_BAS(I_BAS)
                   I_LOSS_RMS(I_BAS) = DSQRT ( I_LOSS_RMS(I_BAS)/K_BAS(I_BAS) - I_LOSS_AVR(I_BAS)**2 + EPS**2 )
                   CALL SORT8 ( K_BAS(I_BAS), I_LOSS_SRT, I_LOSS_IND_SRT )
!
                   IF ( LEVEL(1:3) == 'sum' ) THEN
                        WRITE ( 6, 190 ) C_BAS(I_BAS), K_BAS(I_BAS), -I_LOSS_SRT(K_BAS(I_BAS)), &
     &                                   -I_LOSS_SRT(K_BAS(I_BAS)/2), -I_LOSS_SRT(1), &
     &                                   I_LOSS_AVR(I_BAS), I_LOSS_RMS(I_BAS)
 190                    FORMAT ( 'I_loss Baseline ', A, ' Nobs: ', I6, &
     &                           ' I_loss_min: ', F6.3, ' I_loss_med: ', F6.3, ' I_loss_max: ', F6.3, &
     &                           ' I_loss_avr: ', F6.3, ' I_loss_rms: ', F6.3 )
                     ELSE IF ( LEVEL(1:3) == 'obs' ) THEN
                        DO 4130 J13=1,K_BAS(I_BAS)
                           IND_OBS_BAS = INT(I_LOSS_IND_SRT(J13))
                           IND_OBS = I_LOSS_IND(IND_OBS_BAS,I_BAS)
                           WRITE ( 6, 1100 ) C_BAS(I_BAS), IND_OBS, C_SOU(IND_OBS), &
     &                                      SNR_ARR(5,IND_OBS), I_LOSS(IND_OBS_BAS,I_BAS)
 1100                      FORMAT ( '  i_loss ', A, 1X, I6, 1X, A, ' SNR_I: ', F9.1, ' I_loss: ', F6.3 )
 4130                   CONTINUE 
                        WRITE ( 6, '(A)' ) '#'
                   END IF 
              END IF
 4110      CONTINUE 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  PIMA_CHPO !#!#
