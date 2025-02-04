#ifdef PIMA_USE_MKL
      SUBROUTINE MKL_FFT_2D ( DIM1, DIM2, IEXP, ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine FFT_2D  computes Fourier-Transform of an array of          *
! *   Complesx8 ARR(DIM1,DIM2) using MKL routines.                       *
! *   IEXP ==  1  -- Forward Fourier transform.                          *
! *   IEXP == -1  -- Inverse Fourier transform.                          *
! *                                                                      *
! *  ### 04-AUG-2007  MKL_FFT_2Dc  v1.0 (c)  L. Petrov  04-AUG-2007 ###  *
! *                                                                      *
! ************************************************************************
      USE        MKL_DFTI
      INTEGER*4  DIM1, DIM2, IEXP, IUER
      COMPLEX*8  ARR(DIM1,DIM2)
      TYPE ( DFTI_DESCRIPTOR ), POINTER :: DESC_HANDLE
      INTEGER*4    LENGTHS(2), STRIDES_IN(3), STATUS
!
      LENGTHS(1) = DIM1
      LENGTHS(2) = DIM2
      
      STRIDES_IN(1) = 1
      STRIDES_IN(2) = 1
      STRIDES_IN(3) = DIM1
!
      STATUS = DFTICREATEDESCRIPTOR ( DESC_HANDLE, DFTI_SINGLE, &
     &                                DFTI_COMPLEX, 2, LENGTHS )
      IF ( STATUS .NE. 0 ) THEN
           CALL ERR_LOG ( 121, IUER, 'FFT_2D', 'Error in '// &
     &         'DFTICREATEDESCRIPTOR ' )
           RETURN 
      END IF 
!
      STATUS = DFTICOMMITDESCRIPTOR ( DESC_HANDLE )
      IF ( STATUS .NE. 0 ) THEN
           CALL ERR_LOG ( 121, IUER, 'FFT_2D', 'Error in '// &
     &         'DFTICOMMITDESCRIPTOR  ' )
           RETURN 
      END IF 
!
      IF ( IEXP == 1 ) THEN
           STATUS = DFTI_COMPUTE_FORWARD_C ( DESC_HANDLE, ARR )
           IF ( STATUS .NE. 0 ) THEN
                CALL ERR_LOG ( 121, IUER, 'FFT_2D', 'Error in '// &
     &              'DFTI_COMPUTE_FORWARD_C ' )
                RETURN 
           END IF 
        ELSE 
           STATUS = DFTI_COMPUTE_BACKWARD_C ( DESC_HANDLE, ARR )
           IF ( STATUS .NE. 0 ) THEN
                CALL ERR_LOG ( 121, IUER, 'FFT_2D', 'Error in '// &
     &              'DFTI_COMPUTE_BACKWAR_C ' )
                RETURN 
           END IF 
      END IF 
!    
      STATUS = DFTIFREEDESCRIPTOR( DESC_HANDLE )
      IF ( STATUS .NE. 0 ) THEN
           CALL ERR_LOG ( 121, IUER, 'FFT_2D', 'Error in '// &
     &         'DFTIFREEDESCRIPTOR(  ' )
           RETURN 
      END IF 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE  MKL_FFT_2D  !#!#  
#else
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFTW_FFT_2D ( DIM1, DIM2, IEXP, ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine FFTW_FFT_2D 
! *                                                                      *
! *  ### 04-AUG-2007  FFTW_FFT_2D  v1.0 (c)  L. Petrov  04-AUG-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fftw3.f'
      INTEGER*4  DIM1, DIM2, IEXP, IUER
      COMPLEX*8  ARR(DIM1,DIM2)
      INTEGER*8  PLAN_FFTW
!
      IF ( IEXP == 1 ) THEN
           CALL SFFTW_PLAN_DFT_2D ( PLAN_FFTW, DIM1, DIM2, ARR, ARR, &
     &                               FFTW_FORWARD, FFTW_ESTIMATE )
         ELSE 
           CALL SFFTW_PLAN_DFT_2D ( PLAN_FFTW, DIM1, DIM2, ARR, ARR, &
     &                               FFTW_BACKWARD, FFTW_ESTIMATE )
      END IF
!
      CALL SFFTW_EXECUTE ( PLAN_FFTW )
      CALL SFFTW_DESTROY_PLAN ( PLAN_FFTW )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFTW_FFT_2D  !#!#
#endif
