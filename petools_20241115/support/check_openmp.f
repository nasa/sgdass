      PROGRAM CHECK_OPENMP
! ************************************************************************
! *                                                                      *
! *   OpenMP test
! *                                                                      *
! *  ### 22-AUG-2020  CHECK_OPENMP  v1.0 (c)  L. Petrov  22-AUG-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8  HAR_SUM_SER, HAR_SUM_OMP, ESP
      REAL*8  EPS, HAR_RES
      PARAMETER  ( EPS     = 1.D-11 )
      PARAMETER  ( HAR_RES = 17.9058951938D0 )
      INTEGER*4  J1, J2, NP
!
      NP = 32*1024*1024
      HAR_SUM_SER = 0.0D0
      DO 410 J1=1,NP
         HAR_SUM_SER = HAR_SUM_SER + 1.D0/J1
 410  CONTINUE 
!
      HAR_SUM_OMP = 0.0D0
!$OMP PARALLEL DO        &
!$OMP&   PRIVATE ( J2 ), &
!$OMP&   REDUCTION (+: HAR_SUM_OMP)
      DO 420 J2=1,NP
         HAR_SUM_OMP = HAR_SUM_OMP + 1.D0/J2
 420  CONTINUE 
!$OMP END PARALLEL DO
      IF ( DABS ( HAR_SUM_OMP - HAR_SUM_SER ) > EPS .OR. DABS ( HAR_SUM_OMP - HAR_RES ) > EPS ) THEN
           WRITE ( 6, * ) 'OpenMP test has failed'
           WRITE ( 6, * ) 'HAR_RES     = ', HAR_RES
           WRITE ( 6, * ) 'HAR_SUM_SER = ', HAR_SUM_SER
           WRITE ( 6, * ) 'HAR_SUM_OMP = ', HAR_SUM_OMP
           WRITE ( 6, * ) 'HAR_SUM_OMP - HAR_RES     = ', HAR_SUM_OMP - HAR_RES
           WRITE ( 6, * ) 'HAR_SUM_OMP - HAR_SUM_SER = ', HAR_SUM_OMP - HAR_SUM_SER
      END IF
      END  PROGRAM CHECK_OPENMP  !#!#
