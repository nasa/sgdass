      PROGRAM    FITS_TIM_AVR_MAIN
! ************************************************************************
! *                                                                      *
! *   Program FITS_TIM_AVR takes as an input fits files with calibrated  *
! *   visibilities in fits format and averages them over time.           *
! *   The output is written in the file in fits format. Time averaging   *
! *   in seconds is specified. FITS_TIM_AVR_MAIN starts averaging from   *
! *   the first visibility till it fills a chunks of tim_av_sec long.    *
! *   Then it coherently averages, updates weights, writes down starts   *
! *   again.                                                             *
! *                                                                      *
! *   Usage:  fits_tim_avr input_uva tim_av_sec output_uva               *
! *   where the first argument is the input file, the second argument is *
! *   the averaging interval in seconds, and the third argument is the   *
! *   name of the output file in fits format.                            *
! *                                                                      *
! * ### 19-DEC-2020 FITS_TIM_AVR_MAIN v1.0 (c) L. Petrov 20-JAN-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PIMA__STACK_SIZE_IN_GIGABYTES * GB )
      TYPE     ( PIMA__TYPE ), POINTER :: PIM(:)
      CHARACTER  FILIN*128, FILOUT*128, STR*128, STR1*128
      REAL*8     TIM_AVR
      LOGICAL*1  LEX
      INTEGER*4  J1, IOS, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, UNLINK
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
!
! --- Parse arguments
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: fits_tim_avr input_uva tim_av_sec output_uva'
           CALL EXIT ( 1 ) 
        ELSE
           CALL GETARG ( 1, FILIN  )
           INQUIRE ( FILE=FILIN, EXIST=LEX ) 
           IF ( .NOT. LEX ) THEN
                IUER = -1
                CALL ERR_LOG ( 1901, IUER, 'FTS_TIM_AVR_MAIN', 'Cannot '// &
     &              'find input file '//FILIN ) 
                CALL EXIT ( 1 ) 
           END IF
           CALL GETARG ( 2, STR    )
           IF ( INDEX ( STR, '.' ) < 1 ) THEN
                STR = TRIM(STR)//'.0'
           END IF
           READ ( UNIT=STR, FMT='(F10.2)', IOSTAT=IOS ) TIM_AVR
           IF ( IOS .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 1902, IUER, 'FTS_TIM_AVR_MAIN', 'Wrong '// &
     &              '2nd argument '//TRIM(STR)//' -- a real number is expected' )
                CALL EXIT ( 1 ) 
           END IF
!
           CALL GETARG ( 3, FILOUT )
      END IF     
!
! --- Allocate memory
!
      ALLOCATE ( PIM(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( SIZEOF(PIM), STR )
           IUER = -1
           CALL ERR_LOG ( 1903, IUER, 'FTS_TIM_AVR_MAIN', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for PIMA '// &
     &         'internal data structures. What is going on? Do you really '// &
     &         'have so few memory?' )
           CALL EXIT ( 1 )
      END IF
!
! --- Perform time averaging
!
      IUER = -1
      CALL FITS_TIM_AVR ( PIM(1), FILIN, TIM_AVR, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 1904, IUER, 'FTS_TIM_AVR_MAIN', 'Failure '// &
     &         'in an attempt to average the FITS uva file '//FILIN )
           CALL EXIT ( 1 ) 
      END IF
!
! --- Deallocate memory
!
      IUER = -1
      CALL PIMA_EXIT ( PIM(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 1905, IUER, 'FTS_TIM_AVR_MAIN', 'Trap of '// &
     &         'internal control: failure to close open files and '// &
     &         'deallocate dynamic memory used by PIMA' )
           CALL EXIT ( 1 )
      END IF
      DEALLOCATE ( PIM )
      CALL EXIT ( 0 )
!
      RETURN 
      END  PROGRAM  FITS_TIM_AVR_MAIN  !#!#
