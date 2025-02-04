      SUBROUTINE PIMA_ACCNRM_KOGAN ( N, NLEV1, NLEV2, ACC_CMP_C8, ACC_AVR, &
     &                               IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_ACCNRM_KOGAN performs normalization of the            *
! *   autocorrelation spectrum for the given intermediate frequiency     *
! *   for digitazation distortion of the FX correlator using the         *
! *   algrorithm developed by Leonid Kogan.                              *
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
! * ACC_CMP_C8 ( COMPLEX*8 ) -- Complex autocorrelation spectrum for     *
! *                             the given IF.                            &
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 04-NOV-2009 PIMA_ACCNRM_KOGAN v1.1 (c) L. Petrov 16-DEC-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N, NLEV1, NLEV2, IUER
      REAL*4     ACC_AVR
      COMPLEX*8  ACC_CMP_C8(N)
      REAL*4     CORMAX, TAPER, RDIG, FB, FE, RB, RE, ARG_RCONV, TAPER_MIN
      PARAMETER  ( TAPER_MIN = 1.E-20 )
      COMPLEX*8, ALLOCATABLE :: AC_C8(:)
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, IER
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
           CALL ERR_LOG ( 8421, IUER, 'PIMA_ACCNRM_KOGAN', 'Unsupported '// &
     &         'combination of levels: '//STR(1:12)//' and '//STR(31:42)// &
     &         ' supported combinations: 22, 24 and 44' )
           RETURN 
      END IF      
!
      ALLOCATE   ( AC_C8(2*(N+1)) )
      CALL NOUT_R4 ( 4*(N+1), AC_C8   ) ! Zero the complex array of 2*N+1
!
! --- Put the autocorrelation data into the first part of the array for 
! --- non-negative frequnecies, keeping the negative frequencies zero
!
      DO 410 J1=1,N
         AC_C8(J1) = ACC_CMP_C8(J1)
 410  CONTINUE
!
! --- Store some data
!
      FB = REAL(ACC_CMP_C8(1))
!
! --- Apparently this is the extrapolated point in the spectrum
!
      FE = 2.0*REAL(ACC_CMP_C8(N)) - REAL(ACC_CMP_C8(N-1))
!
! --- Fast Fourier transform of the autocorrelatin data of dimension 2N
!
      CALL ERR_PASS  ( IUER, IER )
      CALL FFT_1D_C8 ( 2*N, 1, AC_C8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8422, IUER, 'PIMA_ACCNRM_TAPER', 'Failure in an '// &
     &         'attempt to perform FFT' )
           DEALLOCATE ( AC_C8 )
           RETURN 
      END IF
!
! --- Zero the negative lags for cross-correlation function
!
      DO 420 J2=N+1,2*N
         AC_C8(J2) = 0.0
 420  CONTINUE
!
      DO 430 J3=1,N+1
!
! ------ black magia
!
         AC_C8(J3) = (2*AC_C8(J3) - FB - FE * (-1)**J3) /(2*N)
!
! ------ Compute the autoconvolutino of the taper
!
         TAPER = 1.0 - (J3 - 1.0)/N
!
         IF ( REAL(TAPER) > TAPER_MIN ) THEN
!
! ----------- Detaper the cross-correlation function
!
              AC_C8(J3) = AC_C8(J3) / TAPER
              IF ( J3 .EQ. 1 ) THEN
                   CORMAX = REAL(AC_C8(1))
                   IF ( CORMAX < TAPER ) CORMAX = 1.0
              END IF
!
! ----------- Normaization to have the maximum number 1.0
!
              RDIG = REAL(AC_C8(J3)) / CORMAX
!
! ----------- Apply digital correction and apply the taper again
!
              AC_C8(J3) = CMPLX ( CORMAX * TAPER * PIMA_RCONV ( ARG_RCONV, RDIG ), 0.0 )
            ELSE
              AC_C8(J3) = 0.0
         END IF
 430  CONTINUE 
      RB = REAL(AC_C8(1))
      RE = REAL(AC_C8(N+1))
!
! --- Inverse Fourier Transform
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFT_1D_C8 ( 2*N, -1, AC_C8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8423, IUER, 'PIMA_ACCNRM_TAPER', 'Failure in an '// &
     &         'attempt to perform FFT' )
           DEALLOCATE ( AC_C8 )
           RETURN 
      END IF
!
! --- The third time black magia
!
      ACC_AVR = 0.0
      DO 440 J4=1,N
!
! ------ Again black magia
!
         ACC_CMP_C8(J4) = 2 * REAL(AC_C8(J4)) - RB - RE*(-1)**J4
         ACC_AVR = ACC_AVR + REAL( ACC_CMP_C8(J4) )
440  CONTINUE 
!
      DEALLOCATE ( AC_C8 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_ACCNRM_KOGAN  !#!#
