      PROGRAM    CREATE_FFTW_PLAN_MAIN
! ************************************************************************
! *                                                                      *
! *   Program CREATE_FFTW_PLAN_MAIN creates the so-called wisdom file -- *
! *   parameter for the tune-ip of the FFTW library that performs        *
! *   fast Fourier transform.                                            *
! *                                                                      *
! * # 01-APR-2006 CREATE_FFTW_PLAN_MAIN v1.0 (c) L. Petrov 10-JUN-2009 # *
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
      INTEGER*4  IUER, NBUF, IND(2,MIND), J1, IL, &
     &           LIND, IRET, NUM_THR
      ADDRESS__TYPE ADR_RE_4, ADR_RE_8, ADR_CM_8, ADR_CM_16, MEM_ADR, MEM_LEN
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
           IF ( STR == 'MEASURE' ) THEN
                FFTW_METHOD = FFTW_MEASURE
              ELSE IF ( STR == 'PATIENT' ) THEN
                FFTW_METHOD = FFTW_PATIENT
              ELSE IF ( STR == 'EXHAUSTIVE' ) THEN
                FFTW_METHOD = FFTW_EXHAUSTIVE
              ELSE 
                CALL ERR_LOG ( 6001, -2, 'CREATE_FFTW_PLAN_MAIN', 'Wrong '// &
     &              'method: only MEASURE, PATIENT and EXAUSTIVE are allowed' )
                CALL EXIT ( 1 )
           END IF
           CALL GETARG ( 2, NUM_THR_STR ) 
           CALL GETARG ( 3, FILIN  ) 
           CALL GETARG ( 4, FILOUT ) 
           CALL TRAN ( 11, STR, STR )
      END IF
!
      IUER = -1
      CALL CREATE_FFTW_PLAN ( FFTW_METHOD, NUM_THR_STR, FILIN, FILOUT, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      CALL EXIT ( 0 )
      END  PROGRAM  CREATE_FFTW_PLAN_MAIN  !#!  
