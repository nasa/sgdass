       PROGRAM    FFT_SPEED_TEST_MAIN
       IMPLICIT   NONE 
       INCLUDE   'pima.i'
       CHARACTER  STR*128
       INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
       PARAMETER  ( GB = 1024*1024*1024 )
       PARAMETER  ( STACK_SIZE_IN_BYTES = PIMA__STACK_SIZE_IN_GIGABYTES * GB )
       INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! ---- Set stacksize
!
       IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
       CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
       CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
       CALL FFT_SPEED_TEST()
       END  PROGRAM  FFT_SPEED_TEST_MAIN
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFT_SPEED_TEST()
! ************************************************************************
! *                                                                      *
! *   Program FFT_SPEED_TEST
! *                                                                      *
! * ### 28-DEC-2012  FFT_SPEED_TEST  v1.0 (c) L. Petrov 29-DEC-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      COMPLEX*8, ALLOCATABLE :: ARR(:,:)
      CHARACTER  METHOD*128, FFT_CONFIG*128, STR*128, STR1*128, FFT_PROC*3, &
     &           STR_WALL*28, STR_CPU*28
      INTEGER*4  NTH, J1, J2, J3, J4, IP, DIM1, DIM2, DIM_MAX, FFTW_MODE, IUER
      REAL*8     RAN1, RAN2, CPU(2), WALL(2)
      INTEGER*4  SEEDS(64), IS, IVRB
      PARAMETER  ( DIM_MAX = 256*1024 )
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IS = 20391234
!
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: fft_speed_test method fft_config num_threads dim'
           CALL EXIT ( 0 )
         ELSE
           CALL GETARG ( 1, METHOD )
           CALL GETARG ( 2, FFT_CONFIG )
           CALL GETARG ( 3, STR        )
           CALL CHIN ( STR, NTH )
           IF ( NTH < 0 .OR. NTH > 32 ) THEN
                WRITE ( 6, '(A)' ) 'Wrong number of threads: ', STR
                CALL EXIT ( 1 )
           END IF
           CALL GETARG ( 4, STR )
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5, STR1 )
                CALL CHIN   ( STR1, IVRB )
              ELSE 
                IVRB = 0
           END IF 
      END IF
!
      IF ( METHOD == 'ESTIMATE' ) THEN
           FFTW_MODE = 0
         ELSE IF ( METHOD == 'MEASURE' ) THEN
           FFTW_MODE = 1
         ELSE IF ( METHOD == 'PATIENT' ) THEN
           FFTW_MODE = 2
         ELSE IF ( METHOD == 'EXHAUSTIVE' ) THEN
           FFTW_MODE = 3
         ELSE IF ( METHOD == 'MKL' ) THEN
           FFTW_MODE = 4
         ELSE 
           WRITE ( 6, * ) 'Unsupported method '//METHOD
           WRITE ( 6, * ) 'Supported methods: ESTIMATE, MEASURE, PATIENT, EXHAUSTIVE' 
           CALL EXIT ( 1 )
      END IF
      IF ( STR(1:3 ) == 'cib' ) THEN
           FFT_PROC = 'cib'
         ELSE IF ( STR(1:3) == 'cif' ) THEN
           FFT_PROC = 'cif'
         ELSE 
           WRITE ( 6, * ) 'Dim should have prefix cib or cif'
           CALL EXIT ( 1 )
      END IF
      IP = INDEX ( STR, 'x' )
      IF ( IP < 5 ) THEN
           WRITE ( 6, * ) 'There should be character x in the 4th parameter for dimension separation'
           CALL EXIT ( 1 )
      END IF
      IF ( IP == ILEN(STR) ) THEN
           WRITE ( 6, * ) 'The second dimension should follow separator x'
           CALL EXIT ( 1 )
      END IF
!
      CALL CHIN ( STR(4:IP-1), DIM1 )
      CALL CHIN ( STR(IP+1:ILEN(STR)), DIM2 )
!
      IF ( DIM1 < 1 .OR. DIM1 > DIM_MAX ) THEN
           WRITE ( 6, * ) 'Wrong dimension1: '//STR(4:IP-1)
           CALL EXIT ( 1 )
      END IF
!
      IF ( DIM2 < 1 .OR. DIM2 > DIM_MAX ) THEN
           WRITE ( 6, * ) 'Wrong dimension2: '//STR(IP+1:ILEN(STR))
           CALL EXIT ( 1 )
      END IF
!
      CALL RANDOM_SEED ( GET=SEEDS )
      SEEDS(1) = IS
      CALL RANDOM_SEED ( PUT=SEEDS )
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, 110 ) CHAR(13)
           CALL FLUSH ( 6 )
 110       FORMAT ( '  Fill array with random numbers...  ',A$ ) 
      END IF
!
      ALLOCATE ( ARR(DIM1,DIM2) )
      DO 410 J1=1,DIM2
         CALL RANDOM_NUMBER ( RAN1 )
         CALL RANDOM_NUMBER ( RAN2 )
         DO 420 J2=1,DIM1
            RAN1 = RAN1+RAN2
            ARR(J2,J1) = CMPLX ( SNGL(RAN1), SNGL(RAN2) )
 420     CONTINUE 
 410  CONTINUE 
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, 120 ) CHAR(13)
 120       FORMAT ( '  Initialization...               ',A$ )
           CALL FLUSH ( 6 )
      END IF
      CALL WALL_TIMER ( %VAL(0) )
      CALL CPU_TIMER  ( %VAL(0) )
      IUER = -1
      IF ( FFTW_MODE == 4 ) THEN
           CALL INIT_FFT_MKL ( NTH, IUER )
         ELSE 
           CALL INIT_FFTW ( FFT_CONFIG, FFTW_MODE, 3.0D0, NTH, IUER )
      END IF
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      CALL CPU_TIMER  ( STR_CPU )
      CALL WALL_TIMER ( STR_WALL )
      READ (  UNIT=STR_CPU(12:27),  FMT='(F16.8)' ) CPU(1)
      READ (  UNIT=STR_WALL(12:27), FMT='(F16.8)' ) WALL(1)
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, '(/A)' ) 'Wall time: '//STR_CPU(12:28)// &
     &                         '  CPU time: '//STR_WALL(12:28)
!
           WRITE ( 6, 130 ) DIM1, DIM2, CHAR(13)
 130       FORMAT ( '  Perform FFT ',I6, ' x ', I6, 2X, A$ )
      END IF
      CALL WALL_TIMER ( %VAL(0) )
      CALL CPU_TIMER  ( %VAL(0) )
      IUER = -1
      IF ( FFT_PROC == 'cif' ) THEN
           CALL FFT_2D_C8 ( DIM1, DIM2,  1, ARR, IUER )
         ELSE IF ( FFT_PROC == 'cib' ) THEN
           CALL FFT_2D_C8 ( DIM1, DIM2, -1, ARR, IUER )
      END IF
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      CALL CPU_TIMER  ( STR_CPU )
      CALL WALL_TIMER ( STR_WALL )
      READ (  UNIT=STR_CPU(12:27),  FMT='(F16.8)' ) CPU(2)
      READ (  UNIT=STR_WALL(12:27), FMT='(F16.8)' ) WALL(2)
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, '(A) ' ) ' '
      END IF
      WRITE ( 6, 140 ) DIM1, DIM2, NTH, CPU(1), CPU(2), WALL(1), WALL(2)
 140  FORMAT ( 'FFT dim: ', I6, 'x', I6, ' N_thr: ', I2, &
     &         ' Cpu: ',F12.6, 1X, F12.6, &
     &         ' Wall: ',F12.6, 1X, F12.6 )
!
      END  SUBROUTINE   FFT_SPEED_TEST  !#!  
