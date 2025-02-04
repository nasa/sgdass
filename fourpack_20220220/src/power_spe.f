      SUBROUTINE POWER_SPECTRUM_R8 ( N, MODE, TIM_ARR, VAL_ARR, &
     &                               FRQ_ARR, POW_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  POWER_SPECTRUM_R8  computes the power spectrum of the     *
! *   real 1D array of data VAL_ARR of dimension of N that is a function *
! *   of argument TIM_ARR using fast Fourier transform. The result is    *
! *   function POW_ARR of argument FRQ_ARR of dimension N/2+1. Frequency *
! *   runs from 0 through the Nyquest frequency                          *
! *   2/(TIM_ARR(2)-TIM_ARR(1)) with step 1/(TIM_ARR(N)-TIM_ARR(1)).     *
! *   Depending on MODE, several windows will be applied to the data     *
! *   before Fourier tansfrom.                                           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       N ( INTEGER*4 ) -- The number of points of the input array.    *
! *    MODE ( INTEGER*4 ) -- Windowing mode.                             *
! *                          0 -- no windis applied.                     *
! *                          1 -- Hann window (recommended).             *
! *                          2 -- Hamming window.                        *
! * TIM_ARR ( REAL*8    ) -- Argument of the input data. It may not      *
! *                          be necessarily time. Dimension: N.          *
! *                          NB: the argument MUST be ordered in         *
! *                          ascending order and MUST have the           *
! *                          equidistant step. Dimension: N.             *
! * VAL_ARR ( REAL*8    ) -- The function, which power spectrum is to    *
! *                          be computed.                                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * FRQ_ARR ( REAL*8    ) -- Adgument of the power spectrum.             *
! *                          Runs from 0.0 thorugh                       *
! *                          2/(TIM_ARR(2)-TIM_ARR(1)) with step         *
! *                          1/(N*(TIM_ARR(N)-TIM_ARR(1))).              *
! *                          Dimension: N/2+1.                           *
! * POW_ARR ( REAL*8    ) -- The power spectrum. Dimension: N/2+1.       *
! *                          Unit: squaire of val_arr.                   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                          Input: switch IUER=0 -- no error messages   *
! *                                 will be generated even in the case   *
! *                                 of error. IUER=-1 -- in the case of  *
! *                                 error the message will be put on     *
! *                                 stdout.                              *
! *                          Output: 0 in the case of successful         *
! *                                  completion and non-zero in the      *
! *                                  case of error.                      *
! *                                                                      *
! * ### 12-JUN-2009 POWER_SPECTRUM_R8 v1.0 (c) L. Petrov 12-JUN-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE 
      INCLUDE    'astro_constants.i'
      INTEGER*4   MODE, N, IUER
      REAL*8      TIM_ARR(N), VAL_ARR(N), FRQ_ARR(N), POW_ARR(N)
      REAL*8,     ALLOCATABLE :: ARR_R8(:)
      COMPLEX*16, ALLOCATABLE :: ARR_C16(:)
      REAL*8      WIN
      INTEGER*4   J1, J2, J3, J4, IER
!
      ALLOCATE ( ARR_R8(N) )
      ALLOCATE ( ARR_C16(N/2+1) )
!
! --- Compute fast periodogram
!
      DO 410 J1=1,N
         IF ( MODE == 1 ) THEN
              WIN = 0.5D0*(1.0D0 - DCOS ( (PI2*J1)/(N-1) ) )
            ELSE IF ( MODE == 2 ) THEN
              WIN = 0.54D0 - 0.46D0*DCOS( (PI2*J1)/(N-1) )
         END IF
         IF ( MODE == 0 ) THEN
              ARR_R8(J1) = VAL_ARR(J1)
            ELSE IF ( MODE == 1  .OR. &
     &                MODE == 2       ) THEN
              ARR_R8(J1) = WIN*VAL_ARR(J1)
         END IF
 410  CONTINUE
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFT_1D_R2C_R8 ( N, ARR_R8, ARR_C16, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1211, IUER, 'POWER_SPECTR_R8', 'Error '// &
     &         'in an attempt to compute Fourier transform of input '// &
     &         'data' )
           RETURN 
      END IF
!
      FRQ_ARR(1) = 0.0D0
      DO 420 J2=1,N/2+1
         FRQ_ARR(J2) = (J2-1)/((TIM_ARR(2)-TIM_ARR(1))*N)
         IF ( J2 == 1 ) THEN
              POW_ARR(J2) = REAL(ARR_C16(J2))**2/N**2
            ELSE 
              POW_ARR(J2) = 2.0D0*ABS(ARR_C16(J2))**2/N**2
         END IF
 420  CONTINUE 
!
      DEALLOCATE ( ARR_C16 )
      DEALLOCATE ( ARR_R8 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  POWER_SPECTRUM_R8  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE POWER_SPECTRUM_R4 ( N, MODE, TIM_ARR, VAL_ARR, &
     &                               FRQ_ARR, POW_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  POWER_SPECTRUM_R4  computes the power spectrum of the     *
! *   real 1D array of data VAL_ARR of dimension of N that is a function *
! *   of argument TIM_ARR using fast Fourier transform. The result is    *
! *   function POW_ARR of argument FRQ_ARR of dimension N/2+1. Frequency *
! *   runs from 0 through the Nyquest frequency                          *
! *   2/(TIM_ARR(2)-TIM_ARR(1)) with step 1/(TIM_ARR(N)-TIM_ARR(1)).     *
! *   Depending on MODE, several windows will be applied to the data     *
! *   before Fourier tansfrom.                                           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       N ( INTEGER*4 ) -- The number of points of the input array.    *
! *    MODE ( INTEGER*4 ) -- Windowing mode.                             *
! *                          0 -- no windis applied.                     *
! *                          1 -- Hann window (recommended).             *
! *                          2 -- Hanning window.                        *
! * TIM_ARR ( REAL*4    ) -- Argument of the input data. It may not      *
! *                          be necessarily time. Dimension: N.          *
! *                          NB: the argument MUST be ordered in         *
! *                          ascending order and MUST have the           *
! *                          equidistant step. Dimension: N.             *
! * VAL_ARR ( REAL*4    ) -- The function, which power spectrum is to    *
! *                          be computed.                                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * FRQ_ARR ( REAL*4    ) -- Adgument of the power spectrum.             *
! *                          Runs from 0.0 thorugh                       *
! *                          2/(TIM_ARR(2)-TIM_ARR(1)) with step         *
! *                          1/(N*(TIM_ARR(N)-TIM_ARR(1))).              *
! *                          Dimension: N/2+1.                           *
! * POW_ARR ( REAL*4    ) -- The power spectrum. Dimension: N/2+1.       *
! *                          Unit: squaire of val_arr.                   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                          Input: switch IUER=0 -- no error messages   *
! *                                 will be generated even in the case   *
! *                                 of error. IUER=-1 -- in the case of  *
! *                                 error the message will be put on     *
! *                                 stdout.                              *
! *                          Output: 0 in the case of successful         *
! *                                  completion and non-zero in the      *
! *                                  case of error.                      *
! *                                                                      *
! * ### 12-JUN-2009 POWER_SPECTRUM_R4 v1.0 (c) L. Petrov 15-JUN-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE 
      INCLUDE    'astro_constants.i'
      INTEGER*4   MODE, N, IUER
      REAL*4      TIM_ARR(N), VAL_ARR(N), FRQ_ARR(N), POW_ARR(N)
      REAL*4,     ALLOCATABLE :: ARR_R4(:)
      COMPLEX*8,  ALLOCATABLE :: ARR_C8(:)
      REAL*4      WIN
      INTEGER*4   J1, J2, J4, IER
!
      ALLOCATE ( ARR_R4(N) )
      ALLOCATE ( ARR_C8(N/2+1) )
!
! --- Compute fast periodogram
!
      DO 410 J1=1,N
         IF ( MODE == 1 ) THEN
              WIN = 0.50*(1.0 - COS ( (SNGL(PI2)*J1)/(N-1) ) )
            ELSE IF ( MODE == 2 ) THEN
              WIN = 0.54 - 0.46*COS( (SNGL(PI2)*J1)/(N-1) )
         END IF
         IF ( MODE == 0 ) THEN
              ARR_R4(J1) = VAL_ARR(J1)
            ELSE IF ( MODE == 1  .OR. &
     &                MODE == 2       ) THEN
              ARR_R4(J1) = WIN*VAL_ARR(J1)
         END IF
 410  CONTINUE
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFT_1D_R2C_R4 ( N, ARR_R4, ARR_C8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1211, IUER, 'POWER_SPECTR_R4', 'Error '// &
     &         'in an attempt to compute Fourier transform of input '// &
     &         'data' )
           RETURN 
      END IF
!
      FRQ_ARR(1) = 0.0D0
      DO 420 J2=1,N/2+1
         FRQ_ARR(J2) = (J2-1)/((TIM_ARR(2)-TIM_ARR(1))*N)
         IF ( J2 == 1 ) THEN
              POW_ARR(J2) = REAL(ARR_C8(J2))**2/N**2
            ELSE 
              POW_ARR(J2) = 2.0D0*ABS(ARR_C8(J2))**2/N**2
         END IF
 420  CONTINUE 
!
      DEALLOCATE ( ARR_C8 )
      DEALLOCATE ( ARR_R4 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  POWER_SPECTRUM_R4  !#!  
