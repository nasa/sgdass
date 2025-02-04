      SUBROUTINE ROOT_EDGE_FILTER_R8 ( N, MODE, FRQ_THR, BETA, &
     &                                 TIM_ARR, VAL_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  ROOT_EDGE_FILTER_R8  implements either low-pass           *
! *   ( MODE = 1 ), or high=pass ( MODE =2 ) filter. Filtering is        *
! *   performed in three steps: 1) Fourier transform to the frequency    *
! *   domain; 2) multiplication the result by the filter function;       *
! *   3) inverse Fourier transform of the result.                        *
! *                                                                      *******
! *   The following filter function H(f) for the high-pass filter is used:     *
! *                                                                            *
! * 1             beta*(f + f_o)/f_o                  beta*(f - f_o)/f_o       *
! * - * --------------------------------- - ---------------------------------- *
! * 2   sqrt(1 + (beta*(f + f_o)/f_o))**2)  sqrt(1 + (beta*(f - f_o)/f_o))**2) *
! *                                                                            *
! *   The low-pass filter is 1 - H(f)                                    *******
! *                                                                      *
! *   Parameter f_o determines the cut-off frequency, and beta           *
! *   determines the sharpness of the filter, the higher, the sharper.   *
! *   beta=20 is recommended for many applications.                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       N ( INTEGER*4 ) -- The number of points of the input array.    *
! *    MODE ( INTEGER*4 ) -- Filter mode:                                *
! *                          1 -- low-pass filter ( frequencies          *
! *                               f < frq_thr are passed, higher         *
! *                               frequencies are removed ).             *
! *                          2 -- high pass filter ( frequencies         *
! *                               f > frq_thr are passed, lower          *
! *                               frequencies are removed ).             *
! * FRQ_THR ( REAL*8    ) -- The cut-off cyclic frequency of the filter. *
! *                          Units: reciprocal to TIM_ARR units.         *
! *    BETA ( REAL*8    ) -- Parameter that determines the sharpness     *
! *                          of the filter. The higher beta, the closer  *
! *                          the filter shape in the frequency domain    *
! *                          to the capital greek letter "Pi" shape.     *
! *                          NB: a very sharp filter results in an       *
! *                          extended base in the time domain.           *
! * TIM_ARR ( REAL*8    ) -- Argument of the input data. It may not      *
! *                          be necessarily time. Dimension: N.          *
! *                          NB: the argument MUST be ordered in         *
! *                          ascending order and MUST have the           *
! *                          equidistant step. Dimension: N.             *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * VAL_ARR ( REAL*8    ) -- The function that is to be transformed.     *
! *                          Dimension: N.                               *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ## 07-JUN-2009 ROOT_EDGE_FILTER_R8 v1.1 (c) L. Petrov 09-DEC-2009 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MODE, N, IUER
      REAL*8     FRQ_THR, BETA, TIM_ARR(N), VAL_ARR(N)
      COMPLEX*16, ALLOCATABLE :: SPE_CMPL(:)
      REAL*8     FREQ_MIN, FREQ, WIN
      INTEGER*4  J1, IER
!
! --- Allocate memory for the spectrum
!
      ALLOCATE ( SPE_CMPL(N/2+1) )
!
! --- Perform real-value forward Fourier transform of the input data.
! --- Results is the complex array with frequencyes in the range [0, F_n},
! --- where F/N is the Nyquist frequency
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFT_1D_R2C_R8 ( N, VAL_ARR, SPE_CMPL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1231, IUER, 'ROOT_EDGE_FILTER_R8', 'Error '// &
     &         'in an attempt to compute Fourier transform of input '// &
     &         'data' )
           RETURN 
      END IF
!
! --- Determine the step of the frequncies in the spectrum
!
      FREQ_MIN = 1.0/(N*(TIM_ARR(2)-TIM_ARR(1)))
!
! --- Apply the filter to the data
!
      DO 410 J1=1,N/2+1
!
! ------ Get the frequency
!
         FREQ = (J1-1)*FREQ_MIN
!
! ------ Compute the Filter for this frequency
!
         WIN = 0.5D0*( BETA*(FREQ+FRQ_THR)/FRQ_THR / &
     &                 DSQRT( 1.0D0 + (BETA*(FREQ+FRQ_THR)/FRQ_THR)**2 ) - &
     &                 BETA*(FREQ-FRQ_THR)/FRQ_THR / &
     &                 DSQRT( 1.0D0 + (BETA*(FREQ-FRQ_THR)/FRQ_THR)**2 ) )
!
! ------ Apply the filter
!
         IF ( MODE == 1 ) THEN
!
! ----------- Low pass filter
!
              SPE_CMPL(J1) = WIN*SPE_CMPL(J1)
            ELSE IF ( MODE == 2 ) THEN
!
! ----------- High pass filter
!
              SPE_CMPL(J1) = (1.0D0 - WIN)*SPE_CMPL(J1)
         END IF
 410  CONTINUE 
!
! --- Perform inverse (backward) Fourier transform
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFT_1D_C2R_C16 ( N, SPE_CMPL, VAL_ARR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1232, IUER, 'ROOT_EDGE_FILTER_R8', 'Error '// &
     &         'in an attempt to compute backward Fourier transform' )
           RETURN 
      END IF
!
! --- Do not forget about spectrum normalization!!
!
      VAL_ARR = VAL_ARR/N
!
! --- Deallocate memory for spectrum
!
      DEALLOCATE ( SPE_CMPL )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  ROOT_EDGE_FILTER_R8  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ROOT_EDGE_FILTER_R4 ( N, MODE, FRQ_THR, BETA, &
     &                                 TIM_ARR, VAL_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  ROOT_EDGE_FILTER_R4  implements either low-pass           *
! *   ( MODE = 1 ), or high=pass ( MODE =2 ) filter. Filtering is        *
! *   performed in three steps: 1) Fourier transfrom to the frequency    *
! *   domain; 2) mulitiplacation the result by the filter function;      *
! *   3) inverse Foureier tansform of the result.                        *
! *                                                                      *
! *                                                                      *******
! *   The following filter function H(f) for the htigh-pass filter is used:    *
! *                                                                            *
! * 1             beta*(f + f_o)/f_o                  beta*(f - f_o)/f_o       *
! * - * --------------------------------- - ---------------------------------- *
! * 2   sqrt(1 + (beta*(f + f_o)/f_o))**2)  sqrt(1 + (beta*(f - f_o)/f_o))**2) *
! *                                                                            *
! *   The low-pass filter is 1 - H(f)                                    *******
! *                                                                      *
! *   Parameter f_o determines the cut-off frquency, and beta determines *
! *   the sharpeness of the filter, the higher, the sharper.             *
! *   beta=20 is recommended for many applications.                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       N ( INTEGER*4 ) -- The number of points of the input array.    *
! *    MODE ( INTEGER*4 ) -- Filter mode:                                *
! *                          1 -- low-pass filter ( frequencies          *
! *                               f < frq_thr are passed, higher         *
! *                               frequencies are removed ).             *
! *                          2 -- high pass filter ( frequencies         *
! *                               f > frq_thr are passed, lower          *
! *                               frequencies are removed ).             *
! * FRQ_THR ( REAL*4    ) -- The cut-off cyclic frequency of the filter. *
! *                          Units: reciprocal to TIM_ARR units.         *
! *    BETA ( REAL*4    ) -- Parameter that determines the sharpness     *
! *                          of the filter. The higher beta, the closer  *
! *                          the filter shape in the frequency domain    *
! *                          to the capital greek letter "Pi" shape.     *
! *                          NB: a very sharp filter results in an       *
! *                          extended base in the time domain.           *
! * TIM_ARR ( REAL*4    ) -- Argument of the input data. It may not      *
! *                          be necessarily time. Dimension: N.          *
! *                          NB: the argument MUST be ordered in         *
! *                          ascending order and MUST have the           *
! *                          equidistant step. Dimension: N.             *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * VAL_ARR ( REAL*4    ) -- The function that is to be transformed.     *
! *                          Dimension: N.                               *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ## 07-JUN-2009 ROOT_EDGE_FILTER_R4 v1.1 (c) L. Petrov 09-DEC-2009 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MODE, N, IUER
      REAL*4     FRQ_THR, BETA, TIM_ARR(N), VAL_ARR(N)
      COMPLEX*8, ALLOCATABLE :: SPE_CMPL(:)
      REAL*4     FREQ_MIN, FREQ, WIN
      INTEGER*4  J1, IER
!
! --- Allocate memory for the spectrum
!
      ALLOCATE ( SPE_CMPL(N/2+1) )
!
! --- Perform real-value forward Fourier transform of the input data.
! --- Results is the complex array with frequencyes in the range [0, F_n},
! --- where F/N is the Nyquist frequency
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFT_1D_R2C_R4 ( N, VAL_ARR, SPE_CMPL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1241, IUER, 'ROOT_EDGE_FILTER_R4', 'Error '// &
     &         'in an attempt to compute Fourier transform of input '// &
     &         'data' )
           RETURN 
      END IF
!
! --- Determine the step of the frequncies in the spectrum
!
      FREQ_MIN = 1.0/(N*(TIM_ARR(2)-TIM_ARR(1)))
!
! --- Apply the filter to the data
!
      DO 410 J1=1,N/2+1
!
! ------ Get the frequency
!
         FREQ = (J1-1)*FREQ_MIN
!
! ------ Compute the filter for this frequency
!
         WIN = 0.5*( BETA*(FREQ+FRQ_THR)/FRQ_THR / &
     &               SQRT( 1.0 + (BETA*(FREQ+FRQ_THR)/FRQ_THR)**2 ) - &
     &               BETA*(FREQ-FRQ_THR)/FRQ_THR / &
     &               SQRT( 1.0 + (BETA*(FREQ-FRQ_THR)/FRQ_THR)**2 ) )
!
! ------ Apply the filter
!
         IF ( MODE == 1 ) THEN
!
! ----------- Low pass filter
!
              SPE_CMPL(J1) = WIN*SPE_CMPL(J1)
            ELSE IF ( MODE == 2 ) THEN
!
! ----------- High pass filter
!
              SPE_CMPL(J1) = (1.0 - WIN)*SPE_CMPL(J1)
         END IF
 410  CONTINUE 
!
! --- Perform inverse (backward) Fourier transform
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFT_1D_C2R_C8 ( N, SPE_CMPL, VAL_ARR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1242, IUER, 'ROOT_EDGE_FILTER_R4', 'Error '// &
     &         'in an attempt to compute backward Fourier transform' )
           RETURN 
      END IF
!
! --- Do not forget about spectrum normalization!!
!
      VAL_ARR = VAL_ARR/N
!
! --- Deallocate memory for spectrum
!
      DEALLOCATE ( SPE_CMPL )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  ROOT_EDGE_FILTER_R4  !#!@
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ROOT_BAND_FILTER_R8 ( N, FRQ_LOW, FRQ_HIGH, BETA, &
     &                                 TIM_ARR, VAL_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  ROOT_BAND_FILTER_R8  implements the band-pass digital     *
! *   filter. Filtering is                                               *
! *   performed in three steps: 1) Fourier transfrom to the frequency    *
! *   domain; 2) mulitiplacation the result by the filter function;      *
! *   3) inverse Foureier tansform of the result.                        *
! *                                                                      *
! *   Filter finction K(f,f_l,f_h) = (1 - H(f,f_l)*H(f,f_h),             *
! *   where H(f,f_o):                                                    *
! *                                                                      *******
! *                                                                            *
! * 1             beta*(f + f_o)/f_o                  beta*(f - f_o)/f_o       *
! * - * --------------------------------- - ---------------------------------- *
! * 2   sqrt(1 + (beta*(f + f_o)/f_o))**2)  sqrt(1 + (beta*(f - f_o)/f_o))**2) *
! *                                                                            *
! *   Parameter f_l, f_h determines the low and high cut-off             *******
! *   frequencies, while parameter beta determines the sharpeness of     *
! *   the filterm the higher, the sharper. beta=20 is recommended for    *
! *   many applications.                                                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        N ( INTEGER*4 ) -- The number of points of the input array.   *
! *  FRQ_LOW ( REAL*8    ) -- The low cut-off cyclic frequency of the    *
! *                           filter. Frequncies f < FRE_LOW will not be *
! *                           passed. Units: reciprocal to TIM_ARR units.*
! * FRQ_HIGH ( REAL*8    ) -- The high cut-off cyclic frequency of the   *
! *                           filter. Frequncies f > FRE_HIGH will not   *
! *                           be passed. Units: reciprocal to TIM_ARR    *
! *                           units.                                     *
! *     BETA ( REAL*8    ) -- Parameter that determines the sharpness    *
! *                           of the filter. The higher beta, the closer *
! *                           the filter shape in the frequency domain   *
! *                           to the capital greek letter "Pi" shape.    *
! *                           NB: a very sharp filter results in an      *
! *                           extended base in the time domain.          *
! *  TIM_ARR ( REAL*8    ) -- Argument of the input data. It may not     *
! *                           be necessarily time. Dimension: N.         *
! *                           NB: the argument MUST be ordered in        *
! *                           ascending order and MUST have the          *
! *                           equidistant step. Dimension: N.            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * VAL_ARR ( REAL*8    ) -- The function that is to be transformed.     *
! *                          Dimension: N.                               *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ## 07-JUN-2009 ROOT_BAND_FILTER_R8 v1.1 (c) L. Petrov 09-DEC-2009 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MODE, N, IUER
      REAL*8     FRQ_LOW, FRQ_HIGH, BETA, TIM_ARR(N), VAL_ARR(N)
      COMPLEX*16, ALLOCATABLE :: SPE_CMPL(:)
      REAL*8     FREQ_MIN, FREQ, WIN_LOW, WIN_HIGH
      INTEGER*4  J1, IER
!
! --- Determine the step of the frequncies in the spectrum
!
      FREQ_MIN = 1.0/(N*(TIM_ARR(2)-TIM_ARR(1)))
!
! --- Allocate memory for the spectrum
!
      ALLOCATE ( SPE_CMPL(N/2+1) )
!
! --- Perform real-value forward Fourier transform of the input data.
! --- Results is the complex array with frequencyes in the range [0, F_n},
! --- where F/N is the Nyquist frequency
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFT_1D_R2C_R8 ( N, VAL_ARR, SPE_CMPL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1251, IUER, 'ROOT_BAND_FILTER_R8', 'Error '// &
     &         'in an attempt to compute Fourier transform of input '// &
     &         'data' )
           RETURN 
      END IF
!
      DO 410 J1=1,N/2+1
!
! ------ Get the frequency
!
         FREQ = (J1-1)*FREQ_MIN
         WIN_LOW = 0.5D0*( BETA*(FREQ+FRQ_LOW)/FRQ_LOW / &
     &                 DSQRT( 1.0D0 + (BETA*(FREQ+FRQ_LOW)/FRQ_LOW)**2 ) - &
     &                 BETA*(FREQ-FRQ_LOW)/FRQ_LOW / &
     &                 DSQRT( 1.0D0 + (BETA*(FREQ-FRQ_LOW)/FRQ_LOW)**2 ) )
         WIN_HIGH = 0.5D0*( BETA*(FREQ+FRQ_HIGH)/FRQ_HIGH / &
     &                 DSQRT( 1.0D0 + (BETA*(FREQ+FRQ_HIGH)/FRQ_HIGH)**2 ) - &
     &                 BETA*(FREQ-FRQ_HIGH)/FRQ_HIGH / &
     &                 DSQRT( 1.0D0 + (BETA*(FREQ-FRQ_HIGH)/FRQ_HIGH)**2 ) )
         SPE_CMPL(J1) = (1.0D0 - WIN_LOW)*WIN_HIGH*SPE_CMPL(J1)
 410  CONTINUE 
!
! --- Perform inverse (backward) Fourier transform
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFT_1D_C2R_C16 ( N, SPE_CMPL, VAL_ARR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1252, IUER, 'ROOT_BAND_FILTER_R8', 'Error '// &
     &         'in an attempt to compute backward Fourier transform' )
           RETURN 
      END IF
!
! --- Do not forget about spectrum normalization!!
!
      VAL_ARR = VAL_ARR/N
!
! --- Deallocate memory for spectrum
!
      DEALLOCATE ( SPE_CMPL )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  ROOT_BAND_FILTER_R8  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ROOT_BAND_FILTER_R4 ( N, FRQ_LOW, FRQ_HIGH, BETA, &
     &                                 TIM_ARR, VAL_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  ROOT_BAND_FILTER_R4  implements the band-pass digital     *
! *   filter. Filtering is                                               *
! *   performed in three steps: 1) Fourier transfrom to the frequency    *
! *   domain; 2) mulitiplacation the result by the filter function;      *
! *   3) inverse Foureier tansform of the result.                        *
! *                                                                      *
! *   Filter finction K(f,f_l,f_h) = (1 - H(f,f_l)*H(f,f_h), where H(f): *
! *                                                                      *
! * 1    beta * ( f + f_o )                 beta * ( f - f_o )           *
! * - * ------------------------------  -  ------------------------------*
! * 2    sqrt(1 + (beta*(f + f_o))**2)     sqrt( 1 + (beta*(f - f_o))**2)*
! *                                                                      *
! *   Parameter f_l, f_h determines the low and high cut-off             *
! *   frequencies, while parameter beta determines the sharpeness of     *
! *   the filter.                                                        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        N ( INTEGER*4 ) -- The number of points of the input array.   *
! *  FRQ_LOW ( REAL*4    ) -- The low cut-off cyclic frequency of the    *
! *                           filter. Frequncies f < FRE_LOW will not be *
! *                           passed. Units: reciprocal to TIM_ARR units.*
! * FRQ_HIGH ( REAL*4    ) -- The high cut-off cyclic frequency of the   *
! *                           filter. Frequncies f > FRE_HIGH will not   *
! *                           be passed. Units: reciprocal to TIM_ARR    *
! *                           units.                                     *
! *     BETA ( REAL*4    ) -- Parameter that determines the sharpness    *
! *                           of the filter. The higher beta, the closer *
! *                           the filter shape in the frequency domain   *
! *                           to the capital greek letter "Pi" shape.    *
! *                           NB: a very sharp filter results in an      *
! *                           extended base in the time domain.          *
! *  TIM_ARR ( REAL*4    ) -- Argument of the input data. It may not     *
! *                           be necessarily time. Dimension: N.         *
! *                           NB: the argument MUST be ordered in        *
! *                           ascending order and MUST have the          *
! *                           equidistant step. Dimension: N.            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * VAL_ARR ( REAL*4    ) -- The function that is to be transformed.     *
! *                          Dimension: N.                               *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ## 07-JUN-2009 ROOT_BAND_FILTER_R4 v1.1 (c) L. Petrov 09-DEC-2009 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MODE, N, IUER
      REAL*4     FRQ_LOW, FRQ_HIGH, BETA, TIM_ARR(N), VAL_ARR(N)
      COMPLEX*8, ALLOCATABLE :: SPE_CMPL(:)
      REAL*4     FREQ_MIN, FREQ, WIN_LOW, WIN_HIGH
      INTEGER*4  J1, IER
!
! --- Determine the step of the frequncies in the spectrum
!
      FREQ_MIN = 1.0/(N*(TIM_ARR(2)-TIM_ARR(1)))
!
! --- Allocate memory for the spectrum
!
      ALLOCATE ( SPE_CMPL(N/2+1) )
!
! --- Perform real-value forward Fourier transform of the input data.
! --- Results is the complex array with frequencyes in the range [0, F_n},
! --- where F/N is the Nyquist frequency
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFT_1D_R2C_R4 ( N, VAL_ARR, SPE_CMPL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1261, IUER, 'ROOT_BAND_FILTER_R4', 'Error '// &
     &         'in an attempt to compute Fourier transform of input '// &
     &         'data' )
           RETURN 
      END IF
!
! --- Determine the step of the frequncies in the spectrum
!
      FREQ_MIN = 1.0/(N*(TIM_ARR(2)-TIM_ARR(1)))
!
      DO 410 J1=1,N/2+1
!
! ------ Get the frequency
!
         FREQ = (J1-1)*FREQ_MIN
         WIN_LOW = 0.5*( BETA*(FREQ+FRQ_LOW)/FREQ / &
     &                 SQRT( 1.0 + (BETA*(FREQ+FRQ_LOW)/FRQ_LOW)**2 ) - &
     &                 BETA*(FREQ-FRQ_LOW)/FRQ_LOW / &
     &                 SQRT( 1.0 + (BETA*(FREQ-FRQ_LOW)/FRQ_LOW)**2 ) )
         WIN_HIGH = 0.5*( BETA*(FREQ+FRQ_HIGH)/FRQ_HIGH / &
     &                 SQRT( 1.0 + (BETA*(FREQ+FRQ_HIGH)/FRQ_HIGH)**2 ) - &
     &                 BETA*(FREQ-FRQ_HIGH)/FRQ_HIGH / &
     &                 SQRT( 1.0 + (BETA*(FREQ-FRQ_HIGH)/FRQ_HIGH)**2 ) )
         SPE_CMPL(J1) = (1.0 - WIN_LOW)*WIN_HIGH*SPE_CMPL(J1)
 410  CONTINUE 
!
! --- Perform inverse (backward) Fourier transform
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFT_1D_C2R_C8 ( N, SPE_CMPL, VAL_ARR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1262, IUER, 'ROOT_BAND_FILTER_R4', 'Error '// &
     &         'in an attempt to compute backward Fourier transform' )
           RETURN 
      END IF
!
! --- Do not forget about spectrum normalization!!
!
      VAL_ARR = VAL_ARR/N
!
! --- Deallocate memory for spectrum
!
      DEALLOCATE ( SPE_CMPL )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  ROOT_BAND_FILTER_R4  !#!#
