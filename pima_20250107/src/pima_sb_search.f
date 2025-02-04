      SUBROUTINE PIMA_SB_SEARCH ( PIM, IND_OBS, LCHN, LFRQ, LTIM, &
     &                            FREQ_ARR, FREQ_REF, UV, WEI_1D, AP_LEN, &
     &                            TIME_FRT, PH_RAT, GR_DEL, PHAS, AMPL, &
     &                            SNR, SB_DEL, SB_DEL_ERR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_SB_SEARCH provides estimates of the single-band delay *
! *   using prior results of the multiband delay estimation.             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       PIM ( PIMA__TYP ) -- Object with information related to        *
! *                            program PIMA.                             *
! *   IND_OBS ( INTEGER*4 ) -- Observation index.                        *
! *      LCHN ( INTEGER*4 ) -- The number of spectral channels           *
! *                            in one frequency channels.                *
! *      LFRQ ( INTEGER*4 ) -- Number of frequency channels.             *
! *      LTIM ( INTEGER*4 ) -- Number of accumulation periods.           *
! *  FREQ_ARR ( REAL*8    ) -- Frequency array. Dimension: (LCHN,LFRQ).  *
! *  FREQ_REF ( REAL*8    ) -- Reference frequency.                      *
! *        UV ( COMPLEX*8 ) -- Array of the cross correlation function.  *
! *                            Dimension: (LCHN,LFRQ,LTIM).              *
! *    WEI_1D ( REAL*8    ) -- One-dimensional Array of weights for      *
! *                            each accumulation period in range [0, 1]. *
! *                            Dimension: LTIM.                          *   
! *    AP_LEN ( REAL*8    ) -- Length of the accumulation period.        *
! *  TIME_FRT ( REAL*8    ) -- Fringe reference time from the nominal    *
! *                            start of the observation.                 *
! *    PH_RAT ( REAL*8    ) -- Phase delay rate.                         *
! *    GR_DEL ( REAL*8    ) -- Group delay.                              *
! *      PHAS ( REAL*8    ) -- Fringe phase at reference moment of time  *
! *                            at reference frequency.                   *
! *      AMPL ( REAL*8    ) -- Amplitude of the cross-correlation        *
! *                            function which corresponds to GR_DEL,     *
! *                            PH_RAT.                                   *
! *       SNR ( REAL*8    ) -- Signal to noise ration for fringe         *
! *                            amplitude.                                *
! *                            at reference frequency.                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   TIME_FRT ( REAL*8   ) -- Fringe reference time from the nominal    *
! *                            start of the observation.                 *
! *     SB_DEL ( REAL*8   ) -- Error of the narrow-band group delay.     *
! * SB_DEL_ERR ( REAL*8   ) -- Error of the narrow-band group delay.     *
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
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! *  ### 28-JUL-2009 PIMA_SB_SEARCH  v1.1 (c) L. Petrov 29-MAR-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'fftw3.f'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IND_OBS, LCHN, LFRQ, LTIM, IUER
      REAL*8     FREQ_ARR(LCHN,LFRQ), FREQ_REF, AP_LEN
      REAL*4     WEI_1D(LTIM)
      REAL*8     TIME_FRT, GR_DEL, PH_RAT, PHAS, AMPL, SNR, SB_DEL, SB_DEL_ERR
      COMPLEX*8  UV(LCHN,LFRQ,LTIM)
      CHARACTER  STR*128
      REAL*8     FREQ_HW, RC, SIG
      REAL*4     PHAS_ADD_R4, UV_PHS, AMP_MIN, AMP_MAX
      PARAMETER  ( AMP_MIN = 1.0E-8 )
      PARAMETER  ( AMP_MAX = 1.0E-1 )
      COMPLEX*8  DRF
      REAL*8,    ALLOCATABLE :: OBS_MAT(:,:), OBS_VEC(:), WEI_VEC(:), &
     &                          NOR_MAT(:), EST_VEC(:), DSP_VEC(:)
      INTEGER*4  J1, J2, J3, J4, J5, KEQ, LEQ, LPAR, IFRQ, IER
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4 
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
!      SB_DEL_ERR = DSQRT ( 3.0D0/LFRQ )/(PI__NUM*LCHN*PIM%CHAN_BW*SNR)
!
      LPAR = LFRQ + 1 
      LEQ  = LCHN*LFRQ + LFRQ
      ALLOCATE ( OBS_MAT(LPAR,LEQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LPAR*LEQ, STR )
           CALL ERR_LOG ( 7231, IUER, 'PIMA_SB_SEARCH', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array OBS_MAT' )
           RETURN 
      END IF
      CALL NOUT_R8 ( LPAR*LEQ, OBS_MAT )
!
      ALLOCATE ( WEI_VEC(LEQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LEQ, STR )
           CALL ERR_LOG ( 7232, IUER, 'PIMA_SB_SEARCH', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array WEI_VEC' )
           RETURN 
      END IF
!
      ALLOCATE ( OBS_VEC(LEQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LEQ, STR )
           CALL ERR_LOG ( 7232, IUER, 'PIMA_SB_SEARCH', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array OBS_VEC' )
           RETURN 
      END IF
!
      ALLOCATE ( NOR_MAT((LPAR*(LPAR+1))/2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*(LEQ*(LEQ+1))/2, STR )
           CALL ERR_LOG ( 7233, IUER, 'PIMA_SB_SEARCH', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array NOR_MAT' )
           RETURN 
      END IF
      CALL NOUT_R8 ( (LPAR*(LPAR+1))/2, NOR_MAT ) 
!
      ALLOCATE ( EST_VEC(LPAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LPAR, STR )
           CALL ERR_LOG ( 7234, IUER, 'PIMA_SB_SEARCH', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array EST_VEC' )
           RETURN 
      END IF
!
      ALLOCATE ( DSP_VEC(LPAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LPAR, STR )
           CALL ERR_LOG ( 7235, IUER, 'PIMA_SB_SEARCH', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array DSP_VEC' )
           RETURN 
      END IF
!
      IFRQ = 0
      KEQ = 0 
      FREQ_HW = ( FREQ_ARR(LCHN,1) - FREQ_ARR(1,1) )/2.0D0
      DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IFRQ = IFRQ + 1
         DO 420 J2=1,LCHN
            DRF = 0.0
            DO 430 J3=1,LTIM
               PHAS_ADD_R4 = - PHAS &
     &                       + PI2*FREQ_REF*PH_RAT* &
     &                         ((J3-1)*AP_LEN - TIME_FRT) &
     &                       + PI2*(FREQ_ARR(J2,IFRQ)-FREQ_REF)*GR_DEL
               DRF = DRF + UV(J2,IFRQ,J3)*WEI_1D(J3)* &
     &                       CMPLX ( COS(PHAS_ADD_R4), SIN(PHAS_ADD_R4) )
 430        CONTINUE 
            KEQ = KEQ + 1
            OBS_MAT(1,KEQ) = (FREQ_ARR(J2,IFRQ) - FREQ_ARR(1,IFRQ))/FREQ_HW
            OBS_MAT(IFRQ+1,KEQ) = 1.0D0
            WEI_VEC(KEQ) = SQRT ( (MIN (ABS(DRF/LTIM), AMP_MAX) )**2 + AMP_MIN**2 )/AMPL
            UV_PHS = PHAS_CMPL_R4 ( DRF/LTIM )
            IF ( UV_PHS > PI__NUM ) THEN
                 UV_PHS = UV_PHS - PI2
            END IF
            OBS_VEC(KEQ) = UV_PHS 
 420     CONTINUE 
 410  CONTINUE 
!
! --- Set equations of constraints on residual phases. We need them
! --- to support a case when one or more channels have zero amplitudes
!
      DO 440 J4=1,LFRQ
         KEQ = KEQ+1
         OBS_MAT(1+J4,KEQ) = 1.0D0
         OBS_VEC(KEQ)      = 0.0D0
         WEI_VEC(KEQ)      = 1.0D-2
 440  CONTINUE 
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  write ( 6, * ) ' snr = ', snr, ' ampl= ', ampl ! %%%
!  write ( 6, * ) ' lpar = ', lpar, ' leq = ', leq ! %%%%%%%
!  write ( 6, * ) ' wei_vec= ', wei_vec  ! %%%
!  write ( 6, * ) ' FREQ_HW= ', FREQ_HW
!  call matview_1 ( lpar, leq, obs_mat ) ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      CALL ERR_PASS ( IUER, IER ) 
      CALL LSQW ( LPAR, LEQ, OBS_MAT, OBS_VEC, WEI_VEC, EST_VEC, DSP_VEC, &
     &            NOR_MAT, RC, SIG, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7441, IUER, 'PIMA_SB_SEARCH', 'Failure in an '// &
     &         'attempt to solve LSQ problem for deriving single band '// &
     &         'delay' )
           RETURN 
      END IF
!
      SB_DEL     = GR_DEL - EST_VEC(1)/(PI2*FREQ_HW)
      SB_DEL_ERR = DSP_VEC(1)/(PI2*FREQ_HW)
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      write ( 6, * ) 'delta_sb:       ' , est_vec(1)/freq_hw
!      write ( 6, * ) 'sb_del_err_lsq: ' , sb_del_err 
!      write ( 6, * ) 'sb_del_err_sts: ' , dsqrt ( 3.0d0/lfrq )/(pi__num*lchn*pim%chan_bw*snr)
!      write ( 6, * ) ' est_vec = ', est_vec ! %%%
!      write ( 6, * ) ' dsp_vec = ', dsp_vec ! %%%
!      write ( 6, * ) ' sig = ' , sig, ' rc  = ', rc 
!      write ( 6, * ) ' ampl = ', ampl
!      write ( 6, * ) ' freq_hw = ', freq_hw
!      write ( 6, * ) ' rc = ', rc
!      write ( 6, * ) ' freq2 = ', freq_arr(1,1), freq_arr(lchn,1) 
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      DEALLOCATE ( OBS_MAT ) 
      DEALLOCATE ( WEI_VEC ) 
      DEALLOCATE ( OBS_VEC ) 
      DEALLOCATE ( NOR_MAT ) 
      DEALLOCATE ( EST_VEC ) 
      DEALLOCATE ( DSP_VEC ) 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_SB_SEARCH  !#!#
