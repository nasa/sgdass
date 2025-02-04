      SUBROUTINE PIMA_FRIP_LOAD ( PIM, VTD, SCA_TYP, IND_SCA, &
     &                            L_OBS, LIS_OBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FRIP_LOAD load observations from the list 
! *   L_OBS/LIS_OBS of the specified type, specified scan into the       *
! *   internal data structure of PIM object for their processing in the  *
! *   phase referencing mode.                                            *
! *                                                                      *
! * ### 27-DEC-2011  PIMA_FRIP_LOAD v1.2 (c)  L. Petrov  04-MAR-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( VTD__TYPE   ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      INTEGER*4  SCA_TYP, IND_SCA, L_OBS, LIS_OBS(L_OBS), IUER
      CHARACTER  STR*128, PIMA_FRI_USED_VERS*32
      INTEGER*8  MEM_SIZE
      COMPLEX*8, ALLOCATABLE :: UV(:,:,:), AC(:,:,:,:), &
     &           PCAL_C8(:,:,:)
      LOGICAL*1  FL_NODATA, FL_BPASS
      REAL*8     FREQ_REF, TIM_ARR(PIM__MAP), THE_GR_DEL, THE_RATE, &
     &           DT, UVW_ARR(3), DER_DEL(VTD__NDER), DER_RAT(VTD__NDER)
      REAL*4     PHAS_R4, AMPL_R4, AC_MEAN(PIM__MFRQ,2), WEI_MAX, &
     &           WEI_1D(PIM__MUV,0:2), PHS_MOD
      REAL*4     WES_MIN, RMS_MIN, RMS_MAX, WEI, TOT_WES
      PARAMETER  ( WES_MIN = 1.E-5 )
      PARAMETER  ( RMS_MIN = 1.E-5 )
      PARAMETER  ( RMS_MAX = 1.E12 )
      REAL*8     PHAS, PH_RAT, GR_DEL, GR_RAT, AP_LEN, TIME_FRT, &
     &           SEFD(PIM__MFRQ), X1(8192), X2(8192)
      REAL*4     PC_GDEL(2)
      COMPLEX*8  BPASS_C8, TOT_VIS, &
     &           CRF_F1W1_IF(PIM__MFRQ), CRF_F1W2_IF(PIM__MFRQ)
      REAL*4     SIG_IF(PIM__MFRQ), WES_IF(PIM__MFRQ), WW_IF(PIM__MFRQ), &
     &           CRF_F2W2_IF(PIM__MFRQ), SIG_SQ, AMPL_IF(PIM__MFRQ)
      INTEGER*4  IFRQ, IND_OBS, LTIM, LFRQ, FRG_IND, IND_AP(PIM__MAP), &
     &           UV_IND, IND_FRQ, J1, J2, J3, J4, J5, J6, J7, J8, J9, &
     &           J10, J11, J12, J13, J14, J15, J16, J17, J18, &
     &           J19, J20, J21, J22, IND_BND, IND_FRA, &
     &           FRQ_TO_IF(PIM__MFRQ*PIM__MCHN), IND_IF, IND_IF_OLD, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*8,    EXTERNAL :: GET_APR_SEFD_BAS_FRQ
      LOGICAL*4, EXTERNAL :: IS_R4_NAN
      REAL*4,    EXTERNAL :: ATAN_CS_R4, PHAS_CMPL_R4
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Set up for VTD
!
      OBS_TYP%PLRZ       = 'RR'
      OBS_TYP%FRQ_REF(1) = PIM%REF_FREQ
      OBS_TYP%N_BND      = 1
      OBS_TYP%DELAY_TYPE = VTD__ML__DTP
      OBS_TYP%FRQ_ION_EFF(1) = PIM%REF_FREQ
      OBS_TYP%STATUS     = VTD__BND
      IND_BND = 1 ! So far, only the first band
!
! --- Get fringe results
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_FRI_READ ( PIM, IND_BND, PIMA_FRI_USED_VERS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9111, IUER, 'PIMA_FRIP_LOAD', 'Error in an '// &
     &         'attempt to read results of fringing from the fringe file '// &
     &          PIM%CONF%FRINGE_FILE )
           RETURN
      END IF
!
! --- Compute theoretical path delay for all observations (and UV coordinates as well)
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_THEO ( PIM, VTD, 'OBS_SRT', '1ST_STA', IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9112, IUER, 'PIMA_FRIP_LOAD', 'Error in an attempt '// &
     &         'to compute theoretical path delays' )
           RETURN
      END IF
!
      PIM%FRIP(SCA_TYP)%IND_SCA = IND_SCA
      PIM%FRIP(SCA_TYP)%NOBS    = L_OBS
      PIM%FRIP(SCA_TYP)%NFRQ    = (PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1)*PIM%NCHN
      ALLOCATE ( PIM%FRIP(SCA_TYP)%FRQ(PIM%FRIP(SCA_TYP)%NFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%FRIP(SCA_TYP)%NFRQ*SIZEOF(PIM%FRIP(SCA_TYP)%FRQ(1)), STR )
           CALL ERR_LOG ( 9113, IUER, 'PIMA_FRIP_LOAD', 'Failure in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dymanic memfory for array PIM%FRIP(SCA_TYP)%FRQ' )
           RETURN 
      END IF
      ALLOCATE ( PIM%FRIP(SCA_TYP)%CFRQ_REF(PIM%NCHN,(PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1)), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%FRIP(SCA_TYP)%NFRQ*SIZEOF(PIM%FRIP(SCA_TYP)%CFRQ_REF(1,1)), STR )
           CALL ERR_LOG ( 9114, IUER, 'PIMA_FRIP_LOAD', 'Failure in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dymanic memfory for array PIM%FRIP(SCA_TYP)%CFRQ_REF' )
           RETURN 
      END IF
!
      FREQ_REF = PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ
      IND_IF = 0
      IFRQ = 0
      DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IND_IF = IND_IF + 1
         DO 420 J2=1,PIM%NCHN
            IFRQ = IFRQ + 1
            PIM%FRIP(SCA_TYP)%FRQ(IFRQ) = PIM%FREQ_ARR(J2,J1,PIM%NFRG)
            PIM%FRIP(SCA_TYP)%CFRQ_REF(J2,IND_IF) = &
     &               PI2*(PIM%FREQ_ARR(J2,J1,PIM%NFRG) - FREQ_REF)
 420     CONTINUE 
 410  CONTINUE 
!
      ALLOCATE ( PIM%FRIP(SCA_TYP)%OBS(PIM%FRIP(SCA_TYP)%NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%FRIP(SCA_TYP)%NOBS*SIZEOF(PIM%FRIP(SCA_TYP)%OBS(1)), STR )
           CALL ERR_LOG ( 9115, IUER, 'PIMA_FRIP_LOAD', 'Failure in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dymanic memfory for array PIM%FRIP(SCA_TYP)%OBS' )
           RETURN 
      END IF
!
      MEM_SIZE = 0
      DO 430 J3=1,PIM%FRIP(SCA_TYP)%NOBS
         IND_OBS = LIS_OBS(J3)
         PIM%FRIP(SCA_TYP)%OBS(J3)%IND_OBS = IND_OBS
         LTIM = PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
         LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
         FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) 
         PIM%FRIP(SCA_TYP)%OBS(J3)%NAP = 0
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
              IF ( SCA_TYP == PIMA__CAL ) STR = 'Cal'
              IF ( SCA_TYP == PIMA__TAG ) STR = 'Tag'
              WRITE  ( 6, 210 ) STR(1:3), J3, PIM%FRIP(SCA_TYP)%NOBS, IND_OBS
 210          FORMAT ( 'PIMA_FRIP_LOAD  Loading ', A, ' observation ',I3, &
     &                 ' ( ', I3, ' ) Ind_Obs: ', I6 )
         END IF
         PIM%FRIP(SCA_TYP)%OBS(J3)%VIS_STATUS = PIMA__UNDEF
!
! ------ Allocate memory for temporary arrays
!
         ALLOCATE ( UV(PIM%NCHN,LFRQ,LTIM), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%OBS(IND_OBS)%NUM_EPC(PIM%CONF%FRQ_GRP)*PIM%NCHN*LFRQ, STR )
              CALL ERR_LOG ( 9116, IUER, 'PIMA_FRIP_LOAD', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for array UV' )
              RETURN
         END IF
!
         ALLOCATE ( PCAL_C8(PIM%NCHN,LFRQ,2), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%NCHN*LFRQ, STR )
              CALL ERR_LOG ( 9117, IUER, 'PIMA_FRIP_LOAD', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for array PCAL_C8' )
              DEALLOCATE ( UV )
              RETURN
         END IF
!
         IF ( PIM%CONF%FRIB_AUTOCORR_CALIB .NE. PIMA__ACCR_NO ) THEN
              ALLOCATE ( AC(PIM%NCHN,LFRQ,LTIM,2), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*PIM%NCHN*LFRQ, STR )
                   CALL ERR_LOG ( 9119, IUER, 'PIMA_FRIP_LOAD', 'Failure to '// &
     &                 'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                 'memory for array AC' )
                   DEALLOCATE ( PCAL_C8 )
                   DEALLOCATE ( UV )
                   RETURN
              END IF
!
! ----------- Get autocorrelation data for the first station
!
              CALL NOUT_R4 ( 4*PIM%NCHN*LFRQ*LTIM, AC )
              FL_NODATA = .FALSE.
              CALL ERR_PASS ( IUER, IER )
!
! @@@@@ this path should be re-written
!
   call pima_get_uv ( %val(-1) )
              CALL PIMA_GET_UV ( PIM, IND_OBS, LFRQ, PIM%CONF%BEG_FRQ, &
     &                           PIM%CONF%END_FRQ, PIM%CONF%FRQ_GRP, &
     &                           AC(1,1,1,1), WEI_1D(1,1), &
     &                           PIM%OBS(IND_OBS)%AP_LEN, 1, IER  )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IND_OBS, STR )
                   CALL ERR_PASS ( IUER, IER )
                   CALL ERR_LOG  ( 9120, IUER, 'PIMA_FRIP_LOAD', 'Failure to '// &
     &                 'get autocorrelation data for the 1st station for '// &
     &                 'the '//STR(1:I_LEN(STR))//'th observation '// &
     &                 'from input FITS-IDI file ' )
                   IF ( PIM%CONF%FRINGE_ERRORS == PIMA__ERR_STOP ) THEN
                        DEALLOCATE ( PCAL_C8 )
                        DEALLOCATE ( UV )
                        CALL ERR_PASS ( IER, IUER )
                        RETURN
                   END IF
                   FL_NODATA = .TRUE.
                   IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                        WRITE ( 6, 220 ) IND_OBS, 1
 220                    FORMAT ( 'PIMA_FRINGE Obs ', I6, &
     &                           ' No autocorrelation data for station ', I1 )
                   END IF
              END IF
!
! ----------- Check whether the data have been received
!
              IF ( .NOT. FL_NODATA ) THEN
                   WEI_MAX = MAXVAL ( WEI_1D(1:PIM%OBS(IND_OBS)%NUM_EPC(PIM%CONF%FRQ_GRP),1) )
                   IF ( WEI_MAX == 0.0 ) FL_NODATA = .TRUE.
              END IF
!
              IF ( FL_NODATA ) GOTO 830
!
! ----------- Get autocorrelation data for the second station
!
              FL_NODATA = .FALSE.
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_GET_UV ( PIM, IND_OBS, LFRQ, PIM%CONF%BEG_FRQ, &
     &                           PIM%CONF%END_FRQ, PIM%CONF%FRQ_GRP, &
     &                           AC(1,1,1,2), WEI_1D(1,2), &
     &                           PIM%OBS(IND_OBS)%AP_LEN, 2, IER  )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IND_OBS, STR )
                   CALL ERR_PASS ( IUER, IER )
                   CALL ERR_LOG ( 9121, IER, 'PIMA_FRIP_LOAD', 'Failure to '// &
     &                 'get autocorrelation data for the 2nd station for '// &
     &                 'the '//STR(1:I_LEN(STR))//'th observation '// &
     &                 'from input FITS-IDI file ' )
                   IF ( PIM%CONF%FRINGE_ERRORS == PIMA__ERR_STOP ) THEN
                        DEALLOCATE ( PCAL_C8 )
                        DEALLOCATE ( UV )
                        CALL ERR_PASS ( IER, IUER )
                        RETURN
                   END IF
                   FL_NODATA = .TRUE.
              END IF
!
! ----------- Check whether the data has been received
!
              IF ( .NOT. FL_NODATA ) THEN
                   WEI_MAX = MAXVAL ( WEI_1D(1:PIM%OBS(IND_OBS)%NUM_EPC(PIM%CONF%FRQ_GRP),2) )
                   IF ( WEI_MAX == 0.0 ) FL_NODATA = .TRUE.
              END IF
!
              IF ( FL_NODATA ) GOTO 830
!
! ----------- Re-normalize spectrum of the autocorrlelation function and
! ----------- compute the mean autotocorrelation for each IF ( AC_MEAN )
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_ACCOR_MEAN ( PIM, IND_OBS, LTIM, AC, WEI_1D, &
     &                               AC_MEAN, .FALSE., IER  )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IND_OBS, STR )
                   CALL ERR_LOG  ( 9122, IUER, 'PIMA_FRIP_LOAD', 'Failure in an '// &
     &                 'attempt to compute averaged autcorrelation function '// &
     &                 'for the '//STR(1:I_LEN(STR))//' th observation '// &
     &                 'from input FITS-IDI file ' )
                   DEALLOCATE ( PCAL_C8 )
                   DEALLOCATE ( UV )
                   RETURN
              END IF
         END IF
!
! ------ Get UV data and their 1D weights (that depenend only on time)
!
         CALL NOUT_R4 ( 2*PIM%NCHN*LFRQ*LTIM, UV )
         FL_NODATA = .FALSE.
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_GET_UV ( PIM, IND_OBS, LFRQ, PIM%CONF%BEG_FRQ, &
     &                      PIM%CONF%END_FRQ, PIM%CONF%FRQ_GRP, &
     &                      UV, WEI_1D(1,0), PIM%OBS(IND_OBS)%AP_LEN, 0, IER  )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IND_OBS, STR )
              CALL ERR_PASS ( IUER, IER )
              CALL ERR_LOG  ( 9123, IUER, 'PIMA_FRIP_LOAD', 'Failure to get '// &
     &            'UV data for the '//STR(1:I_LEN(STR))//'th observation '// &
     &            'from input FITS-IDI file ' )
              IF ( PIM%CONF%FRINGE_ERRORS == PIMA__ERR_STOP ) THEN
                   DEALLOCATE ( PCAL_C8 )
                   DEALLOCATE ( UV )
                   CALL ERR_PASS ( IER, IUER )
                   RETURN
              END IF
              FL_NODATA = .TRUE.
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                   WRITE ( 6, 230 ) IND_OBS
 230               FORMAT ( 'PIMA_FRINGE Obs ', I6, ' No cross-correlation data' )
              END IF
         END IF
!
! ------ Check whether the data has been received
!
         IF ( .NOT. FL_NODATA ) THEN
              WEI_MAX = MAXVAL ( WEI_1D(1:PIM%OBS(IND_OBS)%NUM_EPC(PIM%CONF%FRQ_GRP),0) )
              IF ( WEI_MAX == 0.0 ) FL_NODATA = .TRUE.
         END IF
!
         IF ( FL_NODATA ) THEN
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                   WRITE ( 6, 240 ) IND_OBS
 240               FORMAT ( 'PIMA_FRINGE Obs ', I6, ' All weights for ', &
     &                      'cross-correlation data are zero' )
              END IF
!
! ----------- Alas, no cross-correlation data have been received
!
              GOTO 830
         END IF
!
         IF ( PIM%CONF%PHAS_CAL_CODE .NE. PIMA__PCAL_NO ) THEN
!
! ----------- Use phase calibration phasors
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_USE_PCAL ( PIM, PIMA__APPLY_PCAL, IND_OBS, LFRQ, &
     &                             PIM%CONF%BEG_FRQ, PIM%CONF%END_FRQ, PCAL_C8, &
     &                             PC_GDEL, IER  )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IND_OBS, STR )
                   CALL ERR_PASS ( IUER, IER )
                   CALL ERR_LOG  ( 9124, IUER, 'PIMA_FRIP_LOAD', 'Error '// &
     &                 'during attempt to use phase calibration data '// &
     &                 'for the '//STR(1:I_LEN(STR))//'th observation' )
                   IF ( PIM%CONF%FRINGE_ERRORS == PIMA__ERR_STOP ) THEN
                        DEALLOCATE ( PCAL_C8 )
                        DEALLOCATE ( UV )
                        CALL ERR_PASS ( IER, IUER )
                        RETURN
                      ELSE
                        CALL ERR_PASS ( IUER, IER )
                        GOTO 830
                   END IF
              END IF
         END IF
         IF ( PIM%CONF%BANDPASS_USE .NE. PIMA__BPASS_NO  .AND. &
     &        ASSOCIATED ( PIM%BPASS )                         ) THEN
              FL_BPASS = .TRUE.
            ELSE
              FL_BPASS = .FALSE.
         END IF
!
         IFRQ = 0
         DO 440 J4=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            DO 450 J5=1,PIM%NCHN
               IF ( FL_BPASS ) THEN
!
! ----------------- Get bandpass calibration. NB: sign!
!
                    PHAS_R4 = -PHAS_CMPL_R4 ( PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(1))%BPS(J5,J4) ) &
     &                        +PHAS_CMPL_R4 ( PIM%BPASS(PIM%OBS(IND_OBS)%STA_IND(2))%BPS(J5,J4) )
                    IF ( PIM%CONF%BANDPASS_USE == PIMA__BPASS_AMP_PHS  ) THEN
                         BPASS_C8 = CMPLX ( COS(-PHAS_R4), SIN(-PHAS_R4) )
                       ELSE IF ( PIM%CONF%BANDPASS_USE == PIMA__BPASS_PHS ) THEN
                         BPASS_C8 = CMPLX ( COS(-PHAS_R4), SIN(-PHAS_R4) )
                       ELSE
                         BPASS_C8 = CMPLX ( 1.0, 0.0 )
                    END IF
               END IF
!
               DO 460 J6=1,LTIM
                  IF ( WEI_1D(J6,0) < PIM%CONF%FRIB_WEIGHTS_THRESHOLD ) THEN
                       UV(J5,IFRQ,J6) = CMPLX ( 0.0, 0.0 )
                       WEI_1D(J6,0)   = 0
                  END IF
!
! --------------- Correct insane data
!
                  IF ( IS_R4_NAN ( REAL ( UV(J5,IFRQ,J6) ) ) .OR. &
     &                 IS_R4_NAN ( IMAG ( UV(J5,IFRQ,J6) ) )      ) THEN
                       UV(J5,IFRQ,J6) = CMPLX ( 0.0, 0.0 )
                  END IF
                  IF ( REAL ( UV(J5,IFRQ,J6) ) >  PIMA__AMP_MAX .OR. &
     &                 REAL ( UV(J5,IFRQ,J6) ) < -PIMA__AMP_MAX .OR. &
     &                 IMAG ( UV(J5,IFRQ,J6) ) >  PIMA__AMP_MAX .OR. &
     &                 IMAG ( UV(J5,IFRQ,J6) ) < -PIMA__AMP_MAX      ) THEN
                       UV(J5,IFRQ,J6) = CMPLX ( 0.0, 0.0 )
                  END IF
!
                  IF ( PIM%CONF%PHAS_CAL_CODE .NE. PIMA__PCAL_NO ) THEN
!
! -------------------- Apply phase calibration phases
!
                       UV(J5,IFRQ,J6) = UV(J5,IFRQ,J6) * PCAL_C8(J5,IFRQ,1) * &
     &                                                   PCAL_C8(J5,IFRQ,2)
                  END IF
!
                  IF ( PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_SQRT_MEA .OR. &
     &                 PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_SQRT_EVR .OR. &
     &                 PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_SQRT_KOG ) THEN
                       IF ( ABS(AC(J5,IFRQ,J6,1)) == 0.0  .OR. &
     &                      ABS(AC(J5,IFRQ,J6,2)) == 0.0       ) THEN
!
! ------------------------- Bypass observations without autocorrelations
!
                            UV(J5,IFRQ,J6) = ( 0.0, 0.0 )
                          ELSE IF ( SQRT ( ABS(AC(J5,IFRQ,J6,1))* &
     &                                     ABS(AC(J5,IFRQ,J6,2))  ) < &
     &                              PIM%CONF%FRIB_AUTOCORR_THRESHOLD ) THEN
!
! ------------------------- Bypass observations with very small
! ------------------------- autocorrelations
!
                            UV(J5,IFRQ,J6) = ( 0.0, 0.0 )
                          ELSE
!
! ------------------------- Normalize observations for their autocorrelation
!
                            IF ( PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_SQRT_MEA .OR. &
     &                           PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_SQRT_EVR .OR. &
     &                           PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_SQRT_KOG      ) THEN
                                 UV(J5,IFRQ,J6) = UV(J5,IFRQ,J6) / &
     &                                            SQRT ( ABS(AC_MEAN(IFRQ,1))* &
     &                                                   ABS(AC_MEAN(IFRQ,2))  )
                               ELSE
!
! ------------------------------ ??? It is not right!
!

                                 UV(J5,IFRQ,J6) = UV(J5,IFRQ,J6) / &
     &                                            SQRT ( ABS(AC(J5,IFRQ,J6,1))* &
     &                                                   ABS(AC(J5,IFRQ,J6,2))  )
                            END IF
                       END IF
                    ELSE IF ( PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_INTG .OR. &
     &                        PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_VLBA_CNST1 .OR. &
     &                        PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_VLBA_CNST2      ) THEN
                       IF ( AC_MEAN(J4,1) < PIM%CONF%FRIB_AUTOCORR_THRESHOLD .OR. &
     &                      AC_MEAN(J4,2) < PIM%CONF%FRIB_AUTOCORR_THRESHOLD      ) THEN
!
! ------------------------- Bypass observations with very small
! ------------------------- autocorrelations
!
                            UV(J5,IFRQ,J6) = ( 0.0, 0.0 )
                          ELSE
                            UV(J5,IFRQ,J6) = UV(J5,IFRQ,J6) / &
     &                                       SQRT ( AC_MEAN(IFRQ,1) * &
     &                                              AC_MEAN(IFRQ,2) )
                       END IF
                  END IF
!
                  IF ( PIM%CONF%SAMPLER_CAL_CODE == PIMA__SMPL_USE ) THEN
!
! -------------------- Correction for digitization in the sampler
!
                       IF ( PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(1)) == 4 .AND. &
     &                      PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(2)) == 4      ) THEN
                            UV(J5,IFRQ,J6) = UV(J5,IFRQ,J6)/0.8825D0
                          ELSE IF ( PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(1)) == 4 .AND. &
     &                              PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(2)) == 2      ) THEN
                            UV(J5,IFRQ,J6) = UV(J5,IFRQ,J6)/0.7495D0
                          ELSE IF ( PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(1)) == 2 .AND. &
     &                              PIM%NLEV(PIM%OBS(IND_OBS)%STA_IND(2)) == 4      ) THEN
                            UV(J5,IFRQ,J6) = UV(J5,IFRQ,J6)/0.7495D0
                          ELSE
                            UV(J5,IFRQ,J6) = P2I*UV(J5,IFRQ,J6)
                       END IF
                  END IF
!
                  IF ( FL_BPASS ) THEN
                       UV(J5,IFRQ,J6) = UV(J5,IFRQ,J6)*BPASS_C8
                  END IF
!
                  IF ( PIM%BANDPASS_MASK_STYLE .NE. PIMA__NO ) THEN
!
! -------------------- Apply fine bandpass mask if available
!
                       UV(J5,IFRQ,J6) = UV(J5,IFRQ,J6)* &
     &                       PIM%BANDPASS_MASK(J5,J4,PIM%OBS(IND_OBS)%STA_IND(1),PIMA__MASK_SPLT) * &
     &                       PIM%BANDPASS_MASK(J5,J4,PIM%OBS(IND_OBS)%STA_IND(2),PIMA__MASK_SPLT)
                  END IF
                  IF ( PIM%CONF%CORR_FLAG_MIN .GE. -2 ) THEN
!
! -------------------- Apply calibration for the correlator flag
!
                       IF ( PIM%OBS(IND_OBS)%CORR_FLAG(J6,FRG_IND) .LE. &
     &                      PIM%CONF%CORR_FLAG_MIN ) THEN
                            WEI_1D(J6,0) = 0.0
                       END IF
                  END IF
!
                  IF ( ILEN(PIM%CONF%TIME_FLAG_FILE) > 0         .AND. &
     &                 ASSOCIATED ( PIM%OBS(IND_OBS)%USER_FLAG ) .AND. &
     &                 IFRQ == 1                                 .AND. &
     &                 J5 == 1                                         ) THEN
!
! -------------------- Apply calibration for user time flag
!
                       WEI_1D(J6,0) = WEI_1D(J6,0)*PIM%OBS(IND_OBS)%USER_FLAG(J6)
                  END IF
!
                  IF ( PIM%CONF%FRIB_AMPL_FUDGE_TYPE == PIMA__FUDGE_VLBA ) THEN
!
! -------------------- Amplitude correction for register saturation
! -------------------- specific for the VLBA hardware correlator
!
                       IF ( PIM%NPOL == 1 ) THEN
                            UV(J5,IFRQ,J6) = UV(J5,IFRQ,J6)/(1.0D0 + WEI_1D(J6,0)/8.0D0)
                          ELSE IF ( PIM%NPOL .GE. 2 ) THEN
                            UV(J5,IFRQ,J6) = UV(J5,IFRQ,J6)/(1.0D0 + WEI_1D(J6,0)/4.0D0)
                       END IF
                    ELSE IF ( PIM%CONF%FRIB_AMPL_FUDGE_TYPE == PIMA__FUDGE_KOGAN ) THEN
!
! -------------------- Amplitude correction for register saturation
! -------------------- specific for the VLBA hardware correlator using
! -------------------- original Kogan formula, considering weights are equal to 1.0
!
                       IF ( PIM%NPOL == 1 ) THEN
                            UV(J5,IFRQ,J6) = UV(J5,IFRQ,J6)/(1.0D0 + 1.0D0/8.0D0)
                          ELSE IF ( PIM%NPOL .GE. 2 ) THEN
                            UV(J5,IFRQ,J6) = UV(J5,IFRQ,J6)/(1.0D0 + 1.0D0/4.0D0)
                       END IF
                    ELSE IF ( PIM%CONF%FRIB_AMPL_FUDGE_TYPE == PIMA__FUDGE_DIFX ) THEN
                       CONTINUE 
                  END IF
                  IF ( IFRQ == 1 .AND. J5 == 1 ) THEN
                       PIM%FRIP(SCA_TYP)%OBS(J3)%NAP = PIM%FRIP(SCA_TYP)%OBS(J3)%NAP + 1
                       IF ( PIM%FRIP(SCA_TYP)%OBS(J3)%NAP > PIM__MAP ) THEN
                            CALL CLRCH ( STR )
                            CALL INCH  ( PIM__MAP, STR )
                            CALL ERR_LOG ( 9125, IUER, 'PIMA_FRIP_LOAD', &
     &                          'trap of internal control: too many AP in '// &
     &                          'a scan of source '//PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND)// &
     &                          'PIM__MAP='//STR )
                           RETURN
                       END IF
                       IND_AP(PIM%FRIP(SCA_TYP)%OBS(J3)%NAP) = J6
                       UV_IND  = PIM%OBS(IND_OBS)%UV_IND(J6,FRG_IND)
                       TIM_ARR(J6) = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) - &
     &                               PIM%OBS(IND_OBS)%TIM_BEG
                  END IF
 460           CONTINUE
 450        CONTINUE
 440     CONTINUE
 830     CONTINUE 
!
         ALLOCATE ( PIM%FRIP(SCA_TYP)%OBS(J3)%TIM_AP(PIM%FRIP(SCA_TYP)%OBS(J3)%NAP), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%FRIP(SCA_TYP)%OBS(J3)%NAP, STR )
              CALL ERR_LOG ( 9126, IUER, 'PIMA_FRIP_LOAD', 'Failure in an '// &
     &            'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &            'dymanic memory for array %TIM_AP' )
              RETURN 
         END IF
         MEM_SIZE = MEM_SIZE + 8*PIM%FRIP(SCA_TYP)%OBS(J3)%NAP
!
         ALLOCATE ( PIM%FRIP(SCA_TYP)%OBS(J3)%WEI(PIM%FRIP(SCA_TYP)%NFRQ,PIM%FRIP(SCA_TYP)%OBS(J3)%NAP), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 4*PIM%FRIP(SCA_TYP)%NFRQ*PIM%FRIP(SCA_TYP)%OBS(J3)%NAP, STR )
              CALL ERR_LOG ( 9127, IUER, 'PIMA_FRIP_LOAD', 'Failure in an '// &
     &            'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &            'dymanic memory for array %WEI' )
              RETURN 
         END IF
         PIM%FRIP(SCA_TYP)%OBS(J3)%WEI = 0.0
         MEM_SIZE = MEM_SIZE + 4*PIM%FRIP(SCA_TYP)%NFRQ*PIM%FRIP(SCA_TYP)%OBS(J3)%NAP
!
         ALLOCATE ( PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(PIM%FRIP(SCA_TYP)%NFRQ,PIM%FRIP(SCA_TYP)%OBS(J3)%NAP), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%FRIP(SCA_TYP)%NFRQ*PIM%FRIP(SCA_TYP)%OBS(J3)%NAP, STR )
              CALL ERR_LOG ( 9128, IUER, 'PIMA_FRIP_LOAD', 'Failure in an '// &
     &            'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &            'dymanic memory for array %VIS' )
              RETURN 
         END IF
         PIM%FRIP(SCA_TYP)%OBS(J3)%VIS_STATUS = PIMA__ALLOCATED
         PIM%FRIP(SCA_TYP)%OBS(J3)%VIS = 0.0
         MEM_SIZE = MEM_SIZE + 8*PIM%FRIP(SCA_TYP)%NFRQ* &
     &                           PIM%FRIP(SCA_TYP)%OBS(J3)%NAP
!
         ALLOCATE ( PIM%FRIP(SCA_TYP)%OBS(J3)%UVW(3,PIM%FRIP(SCA_TYP)%NFRQ,PIM%FRIP(SCA_TYP)%OBS(J3)%NAP), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 12*PIM%FRIP(SCA_TYP)%NFRQ*PIM%FRIP(SCA_TYP)%OBS(J3)%NAP, STR )
              CALL ERR_LOG ( 9129, IUER, 'PIMA_FRIP_LOAD', 'Failure in an '// &
     &            'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &            'dymanic memory for array %UVW' )
              RETURN 
         END IF
         PIM%FRIP(SCA_TYP)%OBS(J3)%UVW = 0.0
         MEM_SIZE = MEM_SIZE + 12*PIM%FRIP(SCA_TYP)%NFRQ* &
     &                            PIM%FRIP(SCA_TYP)%OBS(J3)%NAP
!
         CRF_F1W1_IF = CMPLX ( 0.0, 0.0 )
         CRF_F1W2_IF = CMPLX ( 0.0, 0.0 )
         WES_IF      = 0.0
         WW_IF       = 0.0
         CRF_F2W2_IF = 0.0
!
         DO 470 J7=1,PIM%FRIP(SCA_TYP)%OBS(J3)%NAP
            PIM%FRIP(SCA_TYP)%OBS(J3)%TIM_AP(J7) = TIM_ARR(IND_AP(J7))
            PIM%FRIP(SCA_TYP)%OBS(J3)%WEI(1,J7)  = WEI_1D(IND_AP(J7),0)
            IFRQ = 0
            IND_FRQ = 0
            DO 480 J8=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
               IFRQ = IFRQ + 1
               IF ( J7 == 1 ) THEN
                    IF ( PIM%CONF%GAIN_CAL_CODE == PIMA__GAIN_USE ) THEN
                         SEFD(J8) = GET_APR_SEFD_BAS_FRQ ( PIM, IND_OBS, J8 )
                       ELSE 
                         SEFD(J8) = 1.0D0
                    END IF
               END IF
               DO 490 J9=1,PIM%NCHN
                  IND_FRQ = IND_FRQ + 1
                  PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(IND_FRQ,J7) = UV(J9,IFRQ,IND_AP(J7))*SEFD(J8)
                  FRQ_TO_IF(IND_FRQ) = IFRQ
 490           CONTINUE 
 480        CONTINUE 
 470     CONTINUE 
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 9 ) THEN
              WRITE ( 6, * ) 'PIMA_FRIP_LOAD IND_OBS= ', IND_OBS, &
     &                       ' SEFD= ', SEFD(1:LFRQ)
         END IF
!
         IF ( SCA_TYP == PIMA__CAL ) THEN
              IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_DRF ) THEN
                   IND_FRA = PIMA__DRF
                 ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_LSQ ) THEN
                   IND_FRA = PIMA__LSQ
                 ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_MUL ) THEN
                   IND_FRA = PIMA__MUL
                 ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_ADD ) THEN
                   IND_FRA = PIMA__ADD
              END IF
              PHAS     = PIM%OBS(IND_OBS)%RES_PHS(IND_FRA,IND_BND)
              PH_RAT   = PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,IND_BND)
              GR_DEL   = PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,IND_BND)
              GR_RAT   = PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND)
              AP_LEN   = PIM%OBS(IND_OBS)%AP_LEN
              TIME_FRT = PIM%OBS(IND_OBS)%FRT_OFFSET(IND_BND)
              IF ( PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%SIDE_BAND == 1 ) THEN
                   FREQ_REF = PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ
                 ELSE 
                   FREQ_REF = PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ + &
     &                            PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%BAND_WIDTH - &
     &                            PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%CHAN_WIDTH
              END IF
!
              TOT_VIS = ( 0.0, 0.0 )
              TOT_WES = 0.0
              DO 4100 J10=1,PIM%FRIP(SCA_TYP)%OBS(J3)%NAP
                 DT = TIM_ARR(IND_AP(J10)) - TIME_FRT 
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL VTD_DELAY ( PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND), &
     &                            PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                            PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                            PIM%MJD_0, PIM%TAI_0 + PIM%OBS(IND_OBS)%TIM_BEG + TIM_ARR(IND_AP(J10)), &
     &                            OBS_TYP, VTD, THE_GR_DEL, THE_RATE, &
     &                            DER_DEL, DER_RAT, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( IND_OBS, STR )
                      CALL ERR_LOG ( 9130, IUER, 'PIMA_FRIP_LOAD', 'Error in '// &
     &                    'an attempt to compute theoretical path delay for '// &
     &                    'the '//STR(1:I_LEN(STR))//' th observation' )
                      RETURN
                 END IF
!
                 DO 4110 J11=1,PIM%FRIP(SCA_TYP)%NFRQ
                    IF ( PIM%CONF%DEBUG_LEVEL .GE. 12 ) THEN
                         WRITE ( 6, 250 ) J11, J10, &
     &                                    PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(J11,J10), &
     &                                    PIM%FRIP(SCA_TYP)%OBS(J3)%WEI(1,J10)
 250                     FORMAT ( 'FRQ: ', I4,' AP= ', I3, ' UV = ', &
     &                            F10.5, ', ', F10.5, ' WEI =', F9.6 )
                    END IF
!
! ----------------- Get parameters of the fringe fitting model
!
                    PHS_MOD = PH_RAT*PI2* FREQ_REF*DT &
     &                      + GR_DEL*PI2*(PIM%FRIP(SCA_TYP)%FRQ(J11) - FREQ_REF)  &
     &                      + GR_RAT*PI2*(PIM%FRIP(SCA_TYP)%FRQ(J11) - FREQ_REF)*DT
                    WEI = PIM%FRIP(SCA_TYP)%OBS(J3)%WEI(1,J10)
                    IF ( ABS(PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(J11,J10)) < PIMA__AMP_MIN ) THEN
                         WEI = 0.0
                    END IF
!
! ----------------- Apply the fringe fitting model to the calibrated visibility
!
                    PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(J11,J10) = &
                        PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(J11,J10)* &
     &                  CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
                    TOT_VIS = TOT_VIS + PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(J11,J10)*WEI
                    TOT_WES = TOT_WES + WEI
!
                    IFRQ = FRQ_TO_IF(J11)
                    WES_IF(IFRQ) = WES_IF(IFRQ) + WEI
                    WW_IF(IFRQ)  = WW_IF(IFRQ)  + WEI**2
                    CRF_F1W1_IF(IFRQ) = CRF_F1W1_IF(IFRQ) + PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(J11,J10)*WEI
                    CRF_F1W2_IF(IFRQ) = CRF_F1W2_IF(IFRQ) + PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(J11,J10)*WEI**2
                    CRF_F2W2_IF(IFRQ) = CRF_F2W2_IF(IFRQ) + CONJG(PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(J11,J10))* &
     &                                                            PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(J11,J10)* &
     &                                                            WEI**2
!
! ----------------- Now restore visibilities to their original form
! ----------------- (unrotate phase back)
!
                    PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(J11,J10) = &
                        PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(J11,J10)/CMPLX ( COS(PHS_MOD), SIN(PHS_MOD) )
!    
                    CALL ERR_PASS ( IUER, IER )
                    CALL VTD_GET_UVW ( PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND), &
     &                                 PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                                 PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                                 PIM%MJD_0, PIM%TAI_0 + PIM%OBS(IND_OBS)%TIM_BEG + TIM_ARR(IND_AP(J10)), &
     &                                 PIM%FRIP(SCA_TYP)%FRQ(J11), VTD, UVW_ARR, IER )
!
                    IF ( PIM%CONF%DEBUG_LEVEL == 9  .AND. &
     &                   ( J11 == 1  .OR.  J11 == PIM%FRIP(SCA_TYP)%NFRQ ) ) THEN
                         STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%OBS(IND_OBS)%TIM_BEG + TIM_ARR(IND_AP(J10)), -2 ) 
                         WRITE ( 6, 260 ) PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND), & 
                           &              PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
                           &              PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
                           &              STR(1:24), PIM%FRIP(SCA_TYP)%FRQ(J11), UVW_ARR(1:2), &
                           &              TIM_ARR(IND_AP(J10)), J11, THE_GR_DEL, THE_RATE
 260                       FORMAT ( 'PIMA_FRIP_LOAD Sou: ', A, ' Sta: ', A, ' / ', A, &
                           &      ' Date: ', A, ' Frq: ', 1PD15.7, &
     &                            ' UV: ', F12.1, 1X, F12.1, &
                           &      ' Tim_arr: ', f12.2, ' j11= ', i4, &
                           &      ' The_gr: ', 1PD20.12, ' The_rt: ', 1PD20.12 ) 
                    END IF
!
                    IF ( IER .NE. 0 ) THEN
                         CALL CLRCH ( STR )
                         CALL INCH  ( IND_OBS, STR )
                         CALL ERR_LOG ( 9131, IUER, 'PIMA_FRIP_LOAD', 'PIMA_SPLT', 'Error in '// &
     &                       'an attempt to compute UVW for the '// &
     &                        STR(1:I_LEN(STR))//' th observation' )
                         RETURN
                    END IF
                    IF ( PIM%OBS(IND_OBS)%STA_IND(1) < PIM%OBS(IND_OBS)%STA_IND(2) ) THEN
                         PIM%FRIP(SCA_TYP)%OBS(J3)%UVW(1:3,J11,J10) =  UVW_ARR(1:3)
                      ELSE 
                         PIM%FRIP(SCA_TYP)%OBS(J3)%UVW(1:3,J11,J10) = -UVW_ARR(1:3)
                         PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(J11,J10)     = &
     &                       CONJG ( PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(J11,J10) )
                    END IF
 4110            CONTINUE 
 4100         CONTINUE 
!
              DO 4120 J12=1,LFRQ
                 IF ( WES_IF(J12) > PIMA__WEI_MIN ) THEN
                      AMPL_IF(J12) = ABS(CRF_F1W1_IF(J12)/WES_IF(J12))
                      SIG_SQ =           CRF_F2W2_IF(J12)/WW_IF(J12) &
     &                         - ( CONJG(CRF_F1W2_IF(J12))     *CRF_F1W1_IF(J12) &
     &                         +         CRF_F1W2_IF(J12)*CONJG(CRF_F1W1_IF(J12)) )/ &
     &                           ( WES_IF(J12)*WW_IF(J12) ) &
     &                         + CONJG(CRF_F1W1_IF(J12))*CRF_F1W1_IF(J12)/ &
     &                             WES_IF(J12)**2
                       IF ( AMPL_IF(J12) > PIMA__AMP_MIN ) THEN
                            SIG_IF(J12) = SQRT(SIG_SQ/PIM%NCHN)
                          ELSE
                            SIG_IF(J12) = 0.0
                       END IF
                     ELSE 
                       SIG_IF(J12)  = 0.0
                       AMPL_IF(J12) = 0.0
                 END IF
 4120         CONTINUE 
!
              DO 4130 J13=1,PIM%FRIP(SCA_TYP)%OBS(J3)%NAP
!
! -------------- This was the original weight -- a fraction of correlated samples
! -------------- in the AP to the total number of samples
!
                 WEI = PIM%FRIP(SCA_TYP)%OBS(J3)%WEI(1,J13)
                 IND_IF_OLD = 0
                 DO 4140 J14=1,PIM%FRIP(SCA_TYP)%NFRQ
                    IND_IF = FRQ_TO_IF(J14)
                    IF ( SIG_IF(IND_IF) > PIMA__WEI_MIN ) THEN
!
! ---------------------- Compute the weight for this AP. It is proportional 
! ---------------------- to the original weight and reciprocal to the SIG_IF.
!
                         PIM%FRIP(SCA_TYP)%OBS(J3)%WEI(J14,J13) = &
     &                            WEI/SIG_IF(IND_IF)
                      ELSE 
                         PIM%FRIP(SCA_TYP)%OBS(J3)%WEI(J14,J13) = 0.0
                    END IF
                    IF ( PIM%CONF%DEBUG_LEVEL == 6  .AND.  IND_IF .NE. IND_IF_OLD ) THEN
                         WRITE ( 6, 270 ) IND_OBS, J13, J14, IND_IF, &
     &                                    AMPL_IF(IND_IF), &
     &                                    SIG_IF(IND_IF), &
     &                                    PIM%FRIP(SCA_TYP)%OBS(J3)%WEI(J14,J13), &
     &                                    WEI
  270                 FORMAT ( 'PIMA_FRIP_LOAD Ind_obs: ', I5, &
     &                         ' Ind_ap: ', I5, ' Ind_frq: ', I5, ' Ind_if: ', I5, &
     &                         ' Ampl: ', F11.5, ' Sig_ap: ', F11.5, &
     &                         ' Wei: ', F11.5, ' Wei_orig: ', F11.5 )
                    END IF
                    IND_IF_OLD = IND_IF
 4140            CONTINUE 
 4130         CONTINUE 
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                   WRITE ( 6, 280 ) IND_OBS, ABS(TOT_VIS)/TOT_WES
 280               FORMAT ( 'PIMA_FRIP_LOAD  Ind_obs: ', I5, ' Tot_vis: ', F11.5 )
                   DO 4150 J15=1,LFRQ
                      WRITE ( 6, 290 ) IND_OBS, J15, AMPL_IF(J15), &
     &                                 SIG_IF(J15)/DSQRT(1.0D0*PIM%NCHN)
 290                  FORMAT ( 'PIMA_FRIP_LOAD  Ind_obs: ', I5, ' Ifrq: ', I2, &
     &                         ' Ampl_if: ', F11.5, ' -+ ', F11.5, ' Jy' )
 4150              CONTINUE 
              END IF
           ELSE IF ( SCA_TYP == PIMA__TAG ) THEN
              DO 4190 J19=1,PIM%FRIP(SCA_TYP)%OBS(J3)%NAP
                 PIM%FRIP(SCA_TYP)%OBS(J3)%TIM_AP(J19) = TIM_ARR(IND_AP(J19))
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL VTD_DELAY ( PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND), &
     &                            PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                            PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                            PIM%MJD_0, PIM%TAI_0 + PIM%OBS(IND_OBS)%TIM_BEG + TIM_ARR(IND_AP(J19)), &
     &                            OBS_TYP, VTD, THE_GR_DEL, THE_RATE, &
     &                            DER_DEL, DER_RAT, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( IND_OBS, STR )
                      CALL ERR_LOG ( 9132, IUER, 'PIMA_FRIP_LOAD', 'Error in '// &
     &                    'an attempt to compute theoretical path delay for '// &
     &                    'the '//STR(1:I_LEN(STR))//' th observation' )
                      RETURN
                 END IF
!
                 IND_IF  = 0
                 IND_FRQ = 0
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! %%   write ( 6, * ) 'IND_oBs: ', int2(j19), ' ind_ap= ', ind_ap(j19), ' wei= ', WEI_1D(IND_AP(J19),0) ! %%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                 DO 4200 J20=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                    IND_IF = IND_IF + 1
                    IF ( J19 == 1 ) THEN
                         IF ( PIM%CONF%GAIN_CAL_CODE == PIMA__GAIN_USE ) THEN
                              SEFD(J20) = GET_APR_SEFD_BAS_FRQ ( PIM, IND_OBS, J20 )
                           ELSE 
                              SEFD(J20) = 1.0D0
                         END IF
                    END IF
!
                    DO 4210 J21=1,PIM%NCHN
                       IND_FRQ = IND_FRQ + 1
                       PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(IND_FRQ,J19) = UV(J21,IND_IF,IND_AP(J19))*SEFD(J20)
                       PIM%FRIP(SCA_TYP)%OBS(J3)%WEI(IND_FRQ,J19) = WEI_1D(IND_AP(J19),0)
!    
                       CALL ERR_PASS ( IUER, IER )
                       CALL VTD_GET_UVW ( PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND), &
     &                                    PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                                    PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                                    PIM%MJD_0, PIM%TAI_0 + PIM%OBS(IND_OBS)%TIM_BEG + TIM_ARR(IND_AP(J19)), &
     &                                    PIM%FRIP(SCA_TYP)%FRQ(IND_FRQ), VTD, &
     &                                    UVW_ARR, IER )
!
                       IF ( IER .NE. 0 ) THEN
                            CALL CLRCH ( STR )
                            CALL INCH  ( IND_OBS, STR )
                            CALL ERR_LOG ( 9133, IUER, 'PIMA_FRIP_LOAD', 'PIMA_SPLT', &
     &                          'Error in an attempt to compute UVW for the '// &
     &                           STR(1:I_LEN(STR))//' th observation' )
                            RETURN
                       END IF
                       IF ( PIM%OBS(IND_OBS)%STA_IND(1) < PIM%OBS(IND_OBS)%STA_IND(2) ) THEN
                            PIM%FRIP(SCA_TYP)%OBS(J3)%UVW(1:3,IND_FRQ,J19) =  UVW_ARR(1:3)
                         ELSE 
                            PIM%FRIP(SCA_TYP)%OBS(J3)%UVW(1:3,IND_FRQ,J19) = -UVW_ARR(1:3)
                            PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(IND_FRQ,J19)     = &
     &                          CONJG ( PIM%FRIP(SCA_TYP)%OBS(J3)%VIS(IND_FRQ,J19) )
                       END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%   write ( 6, * ) 'inD_Obs= ', ind_obs, ' ind_ap= ', int2(j19), ' ind_frq= ', int2(ind_frq), &  ! %%%%
!%     &              ' wei = ', PIM%FRIP(SCA_TYP)%OBS(J3)%WEI(ind_frq,j19), &
!%     &              ' uv= ', PIM%FRIP(SCA_TYP)%OBS(J3)%UVW(1:2,IND_FRQ,J19), &
!%     &              ' vis = ', pim%frip(sca_typ)%obs(j3)%vis(ind_frq,j19) 
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 4210               CONTINUE 
                    IF ( PIM%CONF%DEBUG_LEVEL == 9  .AND. &
     &                 ( IND_IF == 1  .OR.  IND_IF == PIM%FRIP(SCA_TYP)%NFRQ ) ) THEN
                         STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%OBS(IND_OBS)%TIM_BEG + TIM_ARR(IND_AP(J19)), -2 ) 
                         WRITE ( 6, 2100 ) PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND), & 
     &                           PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                           PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
     &                           STR(1:24), PIM%FRIP(SCA_TYP)%FRQ(IND_IF), &
     &                                      UVW_ARR(1:2), TIM_ARR(IND_AP(J19)), IND_IF
 2100                    FORMAT ( 'PIMA_FRIP_LOAD Sou: ', A, ' Sta: ', A, ' / ', A, &
     &                            ' Date: ', A, ' Frq: ', 1PD15.7, &
     &                            ' UV: ', 0PF12.1, 1X, 0PF12.1, &
     &                            ' Tim_arr: ', 0PF12.2, ' j11= ', I4 )
                    END IF
 4200            CONTINUE 
 4190         CONTINUE 
         END IF
!
         PIM%FRIP(SCA_TYP)%OBS(J3)%VIS_STATUS = PIMA__LOADED
         DEALLOCATE ( PCAL_C8 )
         DEALLOCATE ( UV )
         DEALLOCATE ( AC )
 430  CONTINUE 
!
      PIM%FRIP(SCA_TYP)%GAIN = CMPLX ( 1.0, 0.0 )
      CALL CLRCH ( PIM%FRIP(SCA_TYP)%MOD_FILE )
      PIM%FRIP(SCA_TYP)%NAF = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
      PIM%FRIP(SCA_TYP)%MOD_STATUS = PIMA__UNDEF
      PIM%FRIP(SCA_TYP)%MAP_STATUS = PIMA__UNDEF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 8 ) THEN
           WRITE ( 6, * ) 'PIMA_FRIP_LOAD: HAPPY END MEM_SIZE= ', MEM_SIZE
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_FRIP_LOAD  !#!#
