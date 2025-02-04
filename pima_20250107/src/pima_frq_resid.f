      SUBROUTINE PIMA_FRE_RESID ( PIM, IND_OBS, LCHN, LFRQ, LTIM, &
     &                            FREQ_ARR, FREQ_REF, UV, WEI_1D, AP_LEN, &
     &                            TIME_FRT, PH_RAT, GR_DEL, PHAS, AMPL, &
     &                            IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FRE_RESID 
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
! *  ### 22-AUG-2009 PIMA_FREQ_RESID v1.0 (c) L. Petrov 22-AUG-2009 ###  *
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
      REAL*8     TIME_FRT, GR_DEL, PH_RAT, PHAS, AMPL, SNR, 
      COMPLEX*8  UV(LCHN,LFRQ,LTIM)
      CHARACTER  STR*128
      REAL*8     FREQ_HW, RC, SIG
      REAL*4     PHAS_ADD_R4, UV_PHS, AMP_MIN
      PARAMETER  ( AMP_MIN = 1.0E-8 )
      COMPLEX*8  DRF
      REAL*8,    ALLOCATABLE :: OBS_MAT(:,:), OBS_VEC(:), WEI_VEC(:), &
     &                          NOR_MAT(:), EST_VEC(:), DSP_VEC(:)
      INTEGER*4  J1, J2, J3, J4, J5, KEQ, LEQ, LPAR, IFRQ, IER
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4 
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FREQ_RESID  !#!#
