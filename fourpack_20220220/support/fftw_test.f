      PROGRAM    FFTW_TEST
! ************************************************************************
! *                                                                      *
! *   Program for checking FFTW library
! *                                                                      *
! *  ### 31-DEC-2012   FFTW_TEST   v1.0 (c)  L. Petrov  31-DEC-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'fftw3.f'
      INTEGER*4  DIM
      PARAMETER  ( DIM = 16  )
      COMPLEX*8  ARR_C8(DIM,DIM)
      COMPLEX*16 ARR_C16(DIM,DIM)
      INTEGER*4  NUM_THR
      INTEGER*8  SFFTW_PLAN, DFFTW_PLAN
!
      NUM_THR = 2
#ifdef OPENMP_TEST
      CALL DFFTW_PLAN_WITH_NTHREADS ( NUM_THR )
      CALL SFFTW_PLAN_WITH_NTHREADS ( NUM_THR )
#endif
      CALL SFFTW_PLAN_DFT_1D ( DFFTW_PLAN, DIM, ARR_C8, ARR_C8, &
     &                         FFTW_BACKWARD, FFTW_ESTIMATE )
      CALL DFFTW_PLAN_DFT_1D ( DFFTW_PLAN, DIM, ARR_C16, ARR_C16, & 
     &                         FFTW_FORWARD,  FFTW_ESTIMATE )
!
      CALL SFFTW_EXECUTE_DFT ( SFFTW_PLAN, ARR_C8,  ARR_C8 )
      CALL DFFTW_EXECUTE_DFT ( DFFTW_PLAN, ARR_C16, ARR_C16 )
!
      CALL EXIT ( 0 )
      END  PROGRAM  FFTW_TEST  !#!  
