      SUBROUTINE PIMA_ACCNRM_TAPER ( N, NLEV1, NLEV2, ACC_C8, ACC_AVR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_ACCNRM_TAPER  performs normalization of the          *
! *   autocorrelation spectrum for the given intermediate frequency (IF) *
! *   for digitization effects of the FX correlator for the FFT with     *
! *   the rectangular taper.                                             &
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        N ( INTEGER*4 ) -- The number of spectral channels in the IF. *
! *    NLEV1 ( INTEGER*4 ) -- The number of levels for the first station *
! *                           of the baseline.                           *
! *    NLEV2 ( INTEGER*4 ) -- The number of levels for the second        *
! *                           station of the baseline.                   *
! *                                                                      *
! *                           NB: the number of levels is twice the      *
! *                               numbers of bits per sample!            *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  ACC_AVR ( REAL*4    ) -- Averaged value of the corrected            *
! *                           autcorrelation amplitude.                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   ACC_C8 ( COMPLEX*8      ) -- Comlex autocorrelation function       *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 04-NOV-2009 PIMA_ACCNRM_TAPER v1.0 (c) L. Petrov 16-DEC-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N, NLEV1, NLEV2, IUER
      REAL*4     ACC_AVR
      COMPLEX*8  ACC_C8(N)
      COMPLEX*8, ALLOCATABLE :: TMP_C8(:)
      REAL*4,    ALLOCATABLE :: TMP_R4(:)
      REAL*4     CE, CORMAX, TAPER, RDIG, RANL, RAT, TAPER_MIN
      PARAMETER  ( TAPER_MIN = 1.E-7 )
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, ARG_RCONV, IER
      REAL*4,    EXTERNAL :: PIMA_RCONV
!
      IF ( NLEV1 == 2 .AND. NLEV2 == 2 ) THEN
           ARG_RCONV = 22 
         ELSE IF ( NLEV1 == 2 .AND. NLEV2 == 4  .OR. &
     &             NLEV1 == 4 .AND. NLEV2 == 2 ) THEN
           ARG_RCONV = 24 
         ELSE IF ( NLEV1 == 4 .AND. NLEV2 == 4 ) THEN
           ARG_RCONV = 44 
         ELSE 
           CALL CLRCH ( STR )
           CALL INCH  ( NLEV1, STR(1:12)  )
           CALL INCH  ( NLEV2, STR(31:42) )
           CALL ERR_LOG ( 8411, IUER, 'PIMA_ACCNRM_TAPER', 'Unsupported '// &
     &         'combination of levels: '//STR(1:12)//' and '//STR(31:42)// &
     &         ' supported combinations: 22, 24 and 44' )
           RETURN 
      END IF      
!
      ALLOCATE   ( TMP_C8(N+1) )
      ALLOCATE   ( TMP_R4(2*N) )
!
! --- Put the autocorrelation data into the first part of the array for 
! --- non-negative frequnecies
!
      DO 410 J1=1,N
         TMP_C8(J1) = ACC_C8(J1)
 410  CONTINUE
!
! --- Get the value of the N-th freuqnecy of the spectrum by using 
! --- the linear extrapolation from the N-2 -th and N-1 -th frequencies
! --- of the spectrum
!
      TMP_C8(N+1) = 2.0*(TMP_C8(N)) - (TMP_C8(N-1))
!
! --- Fast Fourier transform of the autocorrelatin data of dimension 2N.
! --- We consider the autocorrelation spectrum to be Hermitian. Therefore,
! --- we use the complex-to-real variant of the fast Fouriser transform.
! --- The output array TMP_R4 is of lenght 2*N
!
      CALL ERR_PASS  ( IUER, IER )
      CALL FFT_1D_C2R_C8 ( 2*N, TMP_C8, TMP_R4, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8412, IUER, 'PIMA_ACCNRM_TAPER', 'Failure in an '// &
     &         'attempt to perform FFT' )
           DEALLOCATE ( TMP_C8 )
           DEALLOCATE ( TMP_R4 )
           RETURN 
      END IF
!
      DO 420 J2=1,N+1
!
! ------ Compute the autoconvolution of the taper
!
         TAPER = 1.0 - (J2 - 1.0)/N
!
         TMP_R4(J2) = TMP_R4(J2)/(2*N)
         IF ( TAPER > TAPER_MIN ) THEN
!
! ----------- Normaization to have the maximum number 1.0
!
              IF ( J2 .EQ. 1 ) THEN
                   CORMAX = ABS(TMP_R4(1))/TAPER
                   IF ( CORMAX < TAPER_MIN ) CORMAX = 1.0
              END IF
!
! ----------- Get RDIG -- De-tapered and normalized digitized autocorrelation
! ----------- function
!
              RDIG = ABS(TMP_R4(J2))/(TAPER*CORMAX)
!
! ----------- Compute RANL -- the analogue normalized autcorrelation function 
! ----------- that corresponds to the digital autocorrelation function
!
              RANL = PIMA_RCONV ( ARG_RCONV, RDIG )
              IF ( RDIG > TAPER_MIN ) THEN
                   RAT  = RANL/RDIG
                ELSE 
                   RAT  = 1.0
              END IF
         END IF
!
! ------ Apply digital correction. If the taper autoconvolution was to 
! ------ small, we apply the same analogue/digital correction as before
!
         TMP_R4(J2) = RAT*TMP_R4(J2)
 420  CONTINUE 
!
! --- Add symmetrical values of the lag correlation function in order
! --- to get real autocorrelation spectrum
!
      DO 430 J3=N+2,2*N
         TMP_R4(J3) = TMP_R4(2*N+2-J3)
 430  CONTINUE 
!
! --- Inverse Fourier Transform
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFT_1D_R2C_R4 ( 2*N, TMP_R4, TMP_C8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8413, IUER, 'PIMA_ACCNRM_TAPER', 'Failure in an '// &
     &         'attempt to perform FFT' )
           DEALLOCATE ( TMP_C8 )
           DEALLOCATE ( TMP_R4 )
           RETURN 
      END IF
!
! --- And store the corrected autocorrelation function
! --- We also compute its average value
!
      ACC_AVR = 0.0
      DO 440 J4=1,N 
         ACC_C8(J4) = TMP_C8(J4) 
         ACC_AVR = ACC_AVR + REAL( ACC_C8(J4) )
 440  CONTINUE 
      ACC_AVR = ACC_AVR/N
!
      DEALLOCATE ( TMP_C8 )
      DEALLOCATE ( TMP_R4 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_ACCNRM_TAPER  !#!#
