      PROGRAM    CREATE_FFTW_PLAN_START
      IMPLICIT   NONE 
      CHARACTER  STR*128
      INTEGER*4  FOURPACK_STACK_SIZE_IN_GIGABYTES
      PARAMETER  ( FOURPACK_STACK_SIZE_IN_GIGABYTES = 4 )
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = FOURPACK_STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
!
! --- Launch the main program
!
      CALL CREATE_FFTW_PLAN_MAIN()
      END  PROGRAM  CREATE_FFTW_PLAN_START
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CREATE_FFTW_PLAN_MAIN()
! ************************************************************************
! *                                                                      *
! *   Program CREATE_FFTW_PLAN  creates the so-called wisdom file --     *
! *   parameter for the tune-ip of the FFTW library that performs        *
! *   fast Fourier transform. If hyphen preceeds the method, then        *
! *   create_fftw_plan prints timing.                                    *
! *                                                                      *
! * # 01-APR-2006 CREATE_FFTW_PLAN_MAIN v1.1 (c) L. Petrov 19-FEB-2022 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fftw3.f'
      INTEGER*4  MBUF, MIND, MAX_DIM 
      PARAMETER  ( MAX_DIM = 32768, MBUF = 4096, MIND = 32 )
      LOGICAL*4  LEX
      CHARACTER  BUF(MBUF)*128, FILIN*128, FILOUT*128, REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      CHARACTER  STR*80, NUM_THR_STR*4
      INTEGER*4  MAX_THR
      PARAMETER  ( MAX_THR = 1024 )
      INTEGER*4  IUER, NBUF, IND(2,MIND), J1, IL, IP, LIND, IRET, NUM_THR
      ADDRESS__TYPE ADR_RE_4, ADR_RE_8, ADR_CM_8, ADR_CM_16, MEM_ADR, MEM_LEN
      CHARACTER  FFTW_VERSION_STR*11, FFTWF_VERSION_STR*11
      INTEGER*8, EXTERNAL :: FFTW_VERSION, FFTWF_VERSION
      LOGICAL*1  FL_TIMING
      INTEGER*4  DIM1, DIM2, DIM3
      INTEGER*8  PLAN_FFTW(2)
      INTEGER*4  FFTW_METHOD
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, * ) 'Usage: create_fftw_plan <method> <num_threads> '// &
     &                    '<request_file> <plan_file> '
           CALL EXIT ( 1 ) 
         ELSE
           CALL GETARG ( 1, STR    ) 
           CALL TRAN   ( 11, STR, STR )
           IF ( STR == 'MEASURE' ) THEN
                FFTW_METHOD = FFTW_MEASURE
                FL_TIMING   = .FALSE.
              ELSE IF ( STR == '-MEASURE' ) THEN
                FFTW_METHOD = FFTW_MEASURE
                FL_TIMING   = .TRUE.
              ELSE IF ( STR == 'PATIENT' ) THEN
                FFTW_METHOD = FFTW_PATIENT
                FL_TIMING   = .FALSE.
              ELSE IF ( STR == '-PATIENT' ) THEN
                FFTW_METHOD = FFTW_PATIENT
                FL_TIMING   = .TRUE.
              ELSE IF ( STR == 'EXHAUSTIVE' ) THEN
                FFTW_METHOD = FFTW_EXHAUSTIVE
                FL_TIMING   = .FALSE.
              ELSE IF ( STR == '-EXHAUSTIVE' ) THEN
                FFTW_METHOD = FFTW_EXHAUSTIVE
                FL_TIMING   = .TRUE.
              ELSE IF ( STR == '-FFTWF_VERSION' ) THEN
                CALL STRNCPY ( STR, %VAL(LOC(FFTWF_VERSION)) )
                IP = INDEX ( STR(6:), '-' ) + 4
                FFTWF_VERSION_STR = STR(1:IP) 
                WRITE ( 6, '(A)' ) TRIM(FFTWF_VERSION_STR)
                CALL EXIT ( 0 )
              ELSE IF ( STR == '-FFTW_VERSION' ) THEN
                CALL STRNCPY ( STR, %VAL(LOC(FFTW_VERSION)) )
                IP = INDEX ( STR(6:), '-' ) + 4
                FFTW_VERSION_STR = STR(1:IP) 
                WRITE ( 6, '(A)' ) TRIM(FFTW_VERSION_STR)
                CALL EXIT ( 0 )
              ELSE 
                CALL ERR_LOG ( 6001, -2, 'CREATE_FFTW_PLAN_MAIN', 'Wrong '// &
     &              'method: only MEASURE, PATIENT and EXAUSTIVE are allowed' )
                CALL EXIT ( 1 )
           END IF
           CALL GETARG ( 2, NUM_THR_STR ) 
           CALL GETARG ( 3, FILIN  ) 
           CALL GETARG ( 4, FILOUT ) 
      END IF
!
      IUER = -1
      CALL CREATE_FFTW_PLAN ( FFTW_METHOD, NUM_THR_STR, FL_TIMING, FILIN, &
     &                        FILOUT, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      CALL EXIT ( 0 )
      END  SUBROUTINE  CREATE_FFTW_PLAN_MAIN  !#!  
