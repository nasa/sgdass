      SUBROUTINE INIT_FFT_MKL ( NTHREADS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  INIT_FFT_MKL  initilizes the interface to fast FFT        *
! *   routines supplised by the Intel proprietary MKL library. Upon      *
! *   calling  INIT_FFT_MKL, all subsequent calls to routines of         *
! *   the FAST_FFT library will be served bu MKL subroutines.            *
! *                                                                      *
! *   NB: in order to compile this routine you need to create file       *
! *   mkl_dfti by this command:                                          *
! *                                                                      *
! *   cd ${path_to_intel}/intel/mkl/include                              *
! *   ${fortran_compiler} -c mkl_dfti.f90                                *
! *                                                                      *
! *   Besides, you need to add -I ${path_to_intel}/intel/mkl/include to  *
! *   the compiler options in order to find the mkl_dfti module.         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     NTHREADS ( INTEGER*4 ) -- The number of threads to be used.      *
! *                               Value 0 means no threads. Value -1     *
! *                               means to get the number of threads     *
! *                               from the environment vairable          *
! *                               OMP_NUM_THREADS. NB: FFTW wisdom       *
! *                               created for a non-thread enviroment    *
! *                               will be discarded if the number of     *
! *                               threads > 1.                           *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! * ### 11-JUN-2009  INIT_FFT_MKL   v1.0 (c)  L. Petrov  11-JUN-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  NTHREADS, IUER
      CHARACTER  NUM_THR_STR*32, STR*32
      INTEGER*4  IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      INTEGER*4   FFT_MODE, FFT_NUM_THR, FFT_DEBUG, LAST_DIMS(2)
      INTEGER*8   SFFTW_PLAN, DFFTW_PLAN
      REAL*8      FFTW_PAR_TIMEOUT 
      CHARACTER   LAST_OP*10
      COMMON    / FFT_PARS / DFFTW_PLAN, SFFTW_PLAN, FFTW_PAR_TIMEOUT, &
     &                       FFT_MODE, FFT_NUM_THR, FFT_DEBUG, &
     &                       LAST_DIMS, LAST_OP
#ifdef MKL
      FFT_MODE = -1
      CALL ERR_LOG ( 0, IUER )
!
      CALL GETENVAR ( 'FFT_DEBUG', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(I12)', IOSTAT=IER ) FFT_DEBUG
           IF ( IER .NE. 0 ) FFT_DEBUG = 0
         ELSE 
           FFT_DEBUG = 0
      END IF
!
! --- Initialize threads
!
      IF ( NTHREADS == -1 ) THEN
!
! -------- Check environment variable OMP_NUM_THREADS
!
           CALL GETENVAR ( 'OMP_NUM_THREADS', NUM_THR_STR )
           IF ( ILEN(NUM_THR_STR) > 0 ) THEN
!
! ------------- Well, OMP_NUM_THREADS was setupt. Read it.
!
                CALL CHIN ( NUM_THR_STR, FFT_NUM_THR )
              ELSE 
                FFT_NUM_THR = 0
           END IF
         ELSE 
           FFT_NUM_THR = NTHREADS
      END IF
      IF ( FFT_DEBUG > 0  .AND.  FFT_NUM_THR > 1 ) THEN
           WRITE  ( 6, * ) FFT_NUM_THR, ' threads were initialized for MKL'
         ELSE IF ( FFT_DEBUG > 0  .AND.  FFT_NUM_THR .LE. 1 ) THEN
           WRITE  ( 6, * ) 'No threads mode was initialized for MKL'
      END IF
      CALL ERR_LOG ( 0, IUER )
#else
      CALL ERR_LOG ( 7501, IUER, 'INIT_MKL', 'Support of Intel MKL was '// &
     &              'not compiled in. Please, recompile your application '// &
     &              'with -D MKL option' )
#endif
      RETURN
      END  SUBROUTINE  INIT_FFT_MKL  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION INIT_FFTW ( WISDOM_FILE, FFTW_MODE, FFTW_TIMEOUT, &
     &                     NTHREADS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  INIT_FFTW  reads the so-called WISDOM_FILE with           *
! *   parameters of the tune-up of the FFTW library that implements      *
! *   fast Fourier transoform and performs initiazation of threads.      *
! *   It reads OMP_NUM_THREADS and determines the number of threads.     *
! *   If OMP_NUM_THREADS is not defiend, one thread will be used.        *
! *                                                                      *
! *   The wisdom file should be created by a separate program            *
! *   create_fftw_wisdom.                                                *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * WISDOM_FILE ( CHARACTER ) -- Name of the so-called wisdom file that  *
! *                              contains tune-up parameters of FFTW     *
! *                              library. The file name may be blank --  *
! *                              in that case no wisdom is acquiured.    *
! *   FFTW_MODE ( INTEGER*4 ) -- Used mode of FFTW. The following modes  *
! *                              are supported:                          *
! *                              0 -- ESTIMATE -- provides a reasonable  *
! *                                   speed for execution and very fast  *
! *                                   plan evaluation. Recommended for   *
! *                                   using unless, an evidence exists   *
! *                                   that other modes will be faster.   *
! *                              1 -- MEASURE -- may faster than         *
! *                                   ESTIMATE, but may be 100 times     *
! *                                   slower for startup. Recommended    *
! *                                   only with a wisdom file.           *
! *                              2 -- PATIENT -- may faster than         *
! *                                   ESTIMATE, nut may be 1000 times    *
! *                                   slower for startup. Recommended    *
! *                                   only with a wisdom file.           *
! *                              3 -- EXHAUSTIVE -- may faster than      *
! *                                   ESTIMATE, but it may take 10000    *
! *                                   times slower for a startup.        *
! *                                   You should be crazy if you try it  *
! *                                   without a wisdom file.             *
! * FFTW_TIMEOUT ( REAL*8    ) -- Maximum amount of time in seconds for  *
! *                               creation of plan. FFTW may be so slow  *
! *                               in desifing which plan to use that you *
! *                               will certainly want to limit this time.*
! *                               0.01 is good value for dimensions      *
! *                               less than 65536.                       *
! *     NTHREADS ( INTEGER*4 ) -- The number of threads to be used.      *
! *                               Value 0 means no threads. Value -1     *
! *                               means to get the number of threads     *
! *                               from the environment vairable          *
! *                               OMP_NUM_THREADS. NB: FFTW wisdom       *
! *                               created for a non-thread enviroment    *
! *                               will be discarded if the number of     *
! *                               threads > 1.                           *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! * _________________________ Value: ___________________________________ *
! *                                                                      *
! *  INIT_FFTW ( INTEGER*4 ) -- returns the number of threads            *
! *                             initializied.                            *
! *                                                                      *
! *  ### 19-MAY-2009   INIT_FFTW   v1.2 (c)  L. Petrov  19-FEB-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fftw3.f'
      INTEGER*4  INIT_FFTW
      CHARACTER  WISDOM_FILE*(*)
      INTEGER*4  FFTW_MODE, NTHREADS, IUER
      REAL*8     FFTW_TIMEOUT
!
      INTEGER*4   FFT_MODE, FFT_NUM_THR, FFT_DEBUG, LAST_DIMS(2)
      INTEGER*8   SFFTW_PLAN, DFFTW_PLAN
      REAL*8      FFTW_PAR_TIMEOUT 
      CHARACTER   LAST_OP*10
      COMMON    / FFT_PARS / DFFTW_PLAN, SFFTW_PLAN, FFTW_PAR_TIMEOUT, &
     &                       FFT_MODE, FFT_NUM_THR, FFT_DEBUG, &
     &                       LAST_DIMS, LAST_OP
!
      CHARACTER  STR*128, FFTW_VERSION_STR*11, FFTWF_VERSION_STR*11, &
     &           WIS_VERSION_STR*16, NUM_THR_STR*32
      INTEGER*4  MAX_THR
      PARAMETER  ( MAX_THR = 1024 )
      LOGICAL*1  LEX
      INTEGER*4  IRET, STREAM, UNIX_DATE, IP, IS, IP_R4, IP_R8, &
     &           SIZE_I4, LUN, IER
      INTEGER*8  SIZE_I8, IADR
      CHARACTER, ALLOCATABLE :: BUF(:)*1
      ADDRESS__TYPE, EXTERNAL :: FFTW_VERSION, FFTWF_VERSION
      INTEGER*4, EXTERNAL :: FFTW_IMPORT_WISDOM_FROM_STRING,  &
     &                       FFTWF_IMPORT_WISDOM_FROM_STRING, &
     &                       I_LEN, ILEN, FILE_INFO, READ, FINDEX, LINDEX
!
      IF ( FFTW_MODE == 0 ) THEN
           FFT_MODE = FFTW_ESTIMATE
        ELSE IF ( FFTW_MODE == 1 ) THEN
           FFT_MODE = FFTW_MEASURE
        ELSE IF ( FFTW_MODE == 2 ) THEN
           FFT_MODE = FFTW_PATIENT
        ELSE IF ( FFTW_MODE == 3 ) THEN
           FFT_MODE = FFTW_EXHAUSTIVE
        ELSE 
           CALL CLRCH ( STR )
           CALL INCH  ( FFTW_MODE, STR ) 
           CALL ERR_LOG ( 7511, IUER, 'INIT_FFTW', 'Wrong value of '// &
     &         'parameter FFTW_MODE: '//STR(1:I_LEN(STR))//' one of '// &
     &         '0, 1, 2, or 3 were expected' )
           RETURN
      END IF
      IF ( FFTW_TIMEOUT < 0.0D0 .OR. FFTW_TIMEOUT > 10*86400.0D0 ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR(1:15), FMT='(1PD15.7)' ) FFTW_TIMEOUT
           CALL ERR_LOG ( 7512, IUER, 'INIT_FFTW', 'Wrong value of '// &
     &         'parameter FFTW_TIMEOUT: '//STR(1:I_LEN(STR))// &
     &         ' a REAL*8 value in the range [0, 864000.0] sec was expected' )
           RETURN
      END IF
!
      CALL GETENVAR ( 'FFT_DEBUG', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(I12)', IOSTAT=IER ) FFT_DEBUG
           IF ( IER .NE. 0 ) FFT_DEBUG = 0
         ELSE 
           FFT_DEBUG = 0
      END IF
!
! --- Initialize threads
!
      IF ( NTHREADS == -1 ) THEN
!
! -------- Check environment variable OMP_NUM_THREADS
!
           CALL GETENVAR ( 'OMP_NUM_THREADS', NUM_THR_STR )
           IF ( ILEN(NUM_THR_STR) > 0 ) THEN
!
! ------------- Well, OMP_NUM_THREADS was setupt. Read it.
!
                CALL CHIN ( NUM_THR_STR, FFT_NUM_THR )
              ELSE 
                FFT_NUM_THR = 0
           END IF
         ELSE 
           FFT_NUM_THR = NTHREADS
      END IF
      IF ( FFT_NUM_THR > 0 ) THEN
           CALL DFFTW_INIT_THREADS ( IRET )
           CALL SFFTW_INIT_THREADS ( IRET )
           IF ( IRET .NE. 1 ) THEN
                CALL ERR_LOG ( 7511, IUER, 'INIT_FFTW', 'Failure in '// &
     &              'DFFTW_INIT_THREADS' )
                RETURN
           END IF
!
! -------- Well. The environment variable has been defined. Then decode it
!
           IF ( FFT_NUM_THR > 0  .AND.  FFT_NUM_THR .LE. MAX_THR ) THEN
!
! ------------- ... and set the number of threads
!
                IF ( FFT_DEBUG > 0 ) THEN
                     WRITE  ( 6, * ) FFT_NUM_THR, ' threads were initialized for FFTW'
                END IF
              ELSE 
                IF ( FFT_DEBUG > 0 ) THEN
                     WRITE  ( 6, * ) 'Single thread mode was initialized for FFTW'
                END IF
                FFT_NUM_THR = 1
           END IF
           CALL DFFTW_PLAN_WITH_NTHREADS ( FFT_NUM_THR )
           CALL SFFTW_PLAN_WITH_NTHREADS ( FFT_NUM_THR )
         ELSE 
           FFT_NUM_THR = 0
           IF ( FFT_DEBUG > 0 ) THEN
                WRITE  ( 6, * ) 'No-thread mode for FFTW was initialized'
           END IF
      END IF
#ifndef MKL
      IF ( ILEN(WISDOM_FILE) == 0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Check whether the wisdom file does exist
!
      INQUIRE ( FILE=WISDOM_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7513, IUER, 'INIT_FFTW', 'Cannot find wisdom '// &
     &         'file '//WISDOM_FILE(1:I_LEN(WISDOM_FILE))//' without that '// &
     &         'wisdom I am a complete dumbhead :-(' )
           RETURN
      END IF
!
! --- Get the information input the wisdom file. In particular, 
! --- we are interested in the file size.
!
      IS = FILE_INFO ( WISDOM_FILE(1:I_LEN(WISDOM_FILE))//CHAR(0), UNIX_DATE, &
                       SIZE_I8 )
      IF ( IS < 0 ) THEN
           CALL ERR_LOG ( 7514, IUER, 'INIT_FFTW', 'Error during '// &
     &         'an attempt to get iformation about FFTW wisdom file '// &
     &          WISDOM_FILE )
           RETURN
      END IF
!
! --- Allocate dynamic memory for accommodating the wisdom file
!
      ALLOCATE ( BUF(SIZE_I8+1), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( SIZE_I8+1, STR )
           CALL ERR_LOG ( 7515, IUER, 'INIT_FFTW', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for the buffer for wisdom' )
           RETURN 
      END IF
!
! --- Open the wisdom file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( WISDOM_FILE, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7516, IUER, 'INIT_FFTW', 'Error during '// &
     &         'an attempt to open FFTW wisdom file '//WISDOM_FILE )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
! --- Read the wisdom into the buffer BUF
!
      IS = READ ( %VAL(LUN), %REF(BUF), %VAL(SIZE_I8) )
      IF ( IS .NE. SIZE_I8 ) THEN
           CALL CLRCH ( STR )
           IF ( IS == -1 ) THEN
                CALL GERROR ( STR  )
           END IF
           CALL ERR_LOG ( 7517, IUER, 'INIT_FFTW', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to read FFTW wisdom '// &
     &         'file '//WISDOM_FILE )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
! --- Make the buffer null-terminated
!
      BUF(SIZE_I8+1) = CHAR(0)
!
      SIZE_I4 = SIZE_I8
!
! --- Wisdom file has two wisdoms: one for REAL*8 operations, another for 
! --- REAL*4 operations. 
! 
! --- Search for the REAL*8 wisdom offset
!
      IP_R8 = FINDEX ( %REF(BUF), 'fftw_wisdom', &
     &                 %VAL(SIZE_I4), %VAL(LEN('_wisdom')) )
      IF ( IP_R8 > 0 ) THEN
           IP_R8 = LINDEX ( %REF(BUF), '(', %VAL(IP_R8), %VAL(LEN('(')) )
      END IF
! 
! --- Search for the REAL*4 wisdom offset
!
      IP_R4 = FINDEX ( %REF(BUF), 'fftwf_wisdom', &
     &                 %VAL(SIZE_I4), %VAL(LEN('_wisdom')) )
      IF ( IP_R4 > 0 ) THEN
           IP_R4 = LINDEX ( %REF(BUF), '(', %VAL(IP_R4), %VAL(LEN('(')) )
      END IF
!
! --- REAL*4 and REAL*8 wisdom may be in a different order. 
! --- NB: REAL*4 and REAL*8 wisdom should be read by separate routines!
!
      IF ( IP_R4 == 0  .AND.  IP_R8 > 0 ) THEN
!
! -------- This file has only REAL*8 wisdom. Import it!
!
           IS = FFTW_IMPORT_WISDOM_FROM_STRING ( %VAL(LOC(BUF(1))) )
           IF ( IS .NE. 1 ) THEN
                CALL STRNCPY ( STR, %VAL(LOC(FFTW_VERSION)) )
                IP = INDEX ( STR(6:), '-' ) + 4
                FFTW_VERSION_STR = STR(1:IP) 
                CALL ERR_LOG ( 7518, IUER, 'INIT_FFTW', 'Failure to '// &
     &              'import the REAL*8 wisdom from file '// &
     &               WISDOM_FILE(1:I_LEN(WISDOM_FILE))//' -- I know not '// &
     &              'every person is in a position to acquire wisdom -- '// &
     &              'but I never thought this would be YOU. Please try again!' )
                DEALLOCATE ( BUF )
                RETURN 
           END IF
        ELSE IF ( IP_R4 > 0  .AND.  IP_R8 == 0 ) THEN
!
! -------- This file has only REAL*4 wisdom. Import it!
!
           IS = FFTWF_IMPORT_WISDOM_FROM_STRING ( %VAL(LOC(BUF(1)))  )
           IF ( IS .NE. 1 ) THEN
                CALL STRNCPY ( STR, %VAL(LOC(FFTWF_VERSION)) )
                IP = INDEX ( STR(6:), '-' ) + 4
                FFTWF_VERSION_STR = STR(1:IP) 
                CALL STRNCPY ( WIS_VERSION_STR, %VAL(LOC(BUF(2))) )
                IP = INDEX ( WIS_VERSION_STR, ' ' )
                CALL CLRCH ( WIS_VERSION_STR(IP:) )
                CALL ERR_LOG ( 7519, IUER, 'INIT_FFTW', 'Failure to '// &
     &              'import the REAL*4 wisdom from file '//TRIM(WISDOM_FILE)// &
     &              ' -- fftw library version: '//TRIM(FFTWF_VERSION_STR)// &
     &              ' wisdom fftw version: '//TRIM(WIS_VERSION_STR)//' -- '// &
     &              'please check whether the wisdom file corrupted and '// &
     &              'whether its version matches to the library version' )
                DEALLOCATE ( BUF )
                RETURN 
           END IF
        ELSE IF ( IP_R8 > 0  .AND.  IP_R4 > IP_R8 ) THEN
!
! -------- This file has both REAL*8 and REAL*4 wisdom, and the REAL*4
! -------- wisdom follows the REAL*8 wisdom. Let us first imprt 
! -------- REAL*4 wisdom...
!
           IS = FFTWF_IMPORT_WISDOM_FROM_STRING ( %VAL(LOC(BUF(IP_R4))) )
           IF ( IS .NE. 1 ) THEN
                CALL STRNCPY ( STR, %VAL(LOC(FFTWF_VERSION)) )
                IP = INDEX ( STR(6:), '-' ) + 4
                FFTWF_VERSION_STR = STR(1:IP) 
                CALL STRNCPY ( WIS_VERSION_STR, %VAL(LOC(BUF(2))) )
                IP = INDEX ( WIS_VERSION_STR, ' ' )
                CALL CLRCH ( WIS_VERSION_STR(IP:) )
                CALL ERR_LOG ( 7520, IUER, 'INIT_FFTW', 'Failure to '// &
     &              'import the REAL*4 wisdom from file '//TRIM(WISDOM_FILE)// &
     &              ' -- fftw library version: '//TRIM(FFTWF_VERSION_STR)// &
     &              ' wisdom fftw version: '//TRIM(WIS_VERSION_STR)//' -- '// &
     &              'please check whether the wisdom file corrupted and '// &
     &              'whether its version matches to the library version' )
                DEALLOCATE ( BUF )
                RETURN 
           END IF
!
! -------- ... and then import REAL*8 wisdom
!
           IF ( IP_R8 > 1 ) BUF(IP_R8-1) = CHAR(0)
           IS = FFTW_IMPORT_WISDOM_FROM_STRING ( %VAL(LOC(BUF(1))) )
           IF ( IS .NE. 1 ) THEN
                CALL STRNCPY ( STR, %VAL(LOC(FFTW_VERSION)) )
                IP = INDEX ( STR(6:), '-' ) + 4
                FFTW_VERSION_STR = STR(1:IP) 
                CALL STRNCPY ( WIS_VERSION_STR, %VAL(LOC(BUF(2))) )
                IP = INDEX ( WIS_VERSION_STR, ' ' )
                CALL CLRCH ( WIS_VERSION_STR(IP:) )
                CALL ERR_LOG ( 7521, IUER, 'INIT_FFTW', 'Failure to '// &
     &              'import the REAL*8 wisdom from file '//TRIM(WISDOM_FILE)// &
     &              ' -- fftw library version: '//TRIM(FFTW_VERSION_STR)// &
     &              ' wisdom fftw version: '//TRIM(WIS_VERSION_STR)//' -- '// &
     &              'please check whether the wisdom file corrupted and '// &
     &              'whether its version matches to the library version' )
                DEALLOCATE ( BUF )
                RETURN 
           END IF
        ELSE IF ( IP_R8 > 0  .AND.  IP_R8 > IP_R4 ) THEN
!
! -------- This file has both REAL*4 and REAL*8 wisdom, and the REAL*8
! -------- wisdom follows the REAL*4 wisdom. Let us first imprt 
! -------- REAL*8 wisdom...
!
           IS = FFTW_IMPORT_WISDOM_FROM_STRING ( %VAL(LOC(BUF(IP_R8)))  )
           IF ( IS .NE. 1 ) THEN
                CALL STRNCPY ( STR, %VAL(LOC(FFTW_VERSION)) )
                IP = INDEX ( STR(6:), '-' ) + 4
                FFTW_VERSION_STR = STR(1:IP) 
                IP = INDEX ( WIS_VERSION_STR, ' ' )
                CALL STRNCPY ( WIS_VERSION_STR, %VAL(LOC(BUF(2))) )
                IP = INDEX ( WIS_VERSION_STR, ' ' )
                CALL CLRCH ( WIS_VERSION_STR(IP:) )
                CALL ERR_LOG ( 7522, IUER, 'INIT_FFTW', 'Failure to '// &
     &              'import the REAL*8 wisdom from file '//TRIM(WISDOM_FILE)// &
     &              ' -- fftw library version: '//TRIM(FFTWF_VERSION_STR)// &
     &              ' wisdom fftw version: '//TRIM(WIS_VERSION_STR)//' -- '// &
     &              'please check whether the wisdom file corrupted and '// &
     &              'whether its version matches to the library version' )
                DEALLOCATE ( BUF )
                RETURN 
           END IF
!
! -------- ... and then import REAL*4 wisdom
!
           IF ( IP_R4 > 1 ) BUF(IP_R4-1) = CHAR(0)
           IS = FFTWF_IMPORT_WISDOM_FROM_STRING ( %VAL(LOC(BUF(1))) )
           IF ( IS .NE. 1 ) THEN
                CALL STRNCPY ( STR, %VAL(LOC(FFTWF_VERSION)) )
                IP = INDEX ( STR(6:), '-' ) + 4
                FFTWF_VERSION_STR = STR(1:IP) 
                CALL STRNCPY ( WIS_VERSION_STR, %VAL(LOC(BUF(2))) )
                IP = INDEX ( WIS_VERSION_STR, ' ' )
                CALL CLRCH ( WIS_VERSION_STR(IP:) )
                CALL ERR_LOG ( 7523, IUER, 'INIT_FFTW', 'Failure to '// &
     &              'import the REAL*4 wisdom from file '//TRIM(WISDOM_FILE)// &
     &              ' -- fftw library version: '//TRIM(FFTWF_VERSION_STR)// &
     &              ' wisdom fftw version: '//TRIM(WIS_VERSION_STR)//' -- '// &
     &              'please check whether the wisdom file corrupted and '// &
     &              'whether its version matches to the library version' )
                DEALLOCATE ( BUF )
                RETURN 
           END IF
        ELSE IF ( IP_R8 == 0  .AND.  IP_R8 == IP_R4 ) THEN
!
! -------- Oh, no! This file does not have any wisdom at all!!
!
           CALL ERR_LOG ( 7524, IUER, 'INIT_FFTW', 'Sorry, no wisdom '// &
     &         'was found in the '//WISDOM_FILE(1:I_LEN(WISDOM_FILE))// &
     &         ' -- only silliness. Please try to figure out how this has '// &
     &         'happened. Does it mean, no wisdom remained in the world?'// &
     &         ' Or the the file is corrupted? Or you tried to use the file '// &
     &         'that is not generated by FFTW?' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      DEALLOCATE ( BUF )
#endif
!
      SFFTW_PLAN = 0
      DFFTW_PLAN = 0
      INIT_FFTW  = FFT_NUM_THR
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION   INIT_FFTW  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFT_1D_C16 ( DIM, IEXP, ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  FFT_1D_C16  performs fast Fourier transform in place,     *
! *   either forward ( IEXP = 1 ), or backward ( IEXP = -1 ) under       *
! *   the complex double precision array ARR of dimension DIM.           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  DIM ( INTEGER*4 ) -- Dimension of the array. NB: dimension that     *
! *                       is the power of 2 yileds the best performance. *
! * IEXP ( INTEGER*4 ) -- Direction of the transform:                    *
! *                        1 -- forward  (direct)  FFT.                  *
! *                       -1 -- backward (reverse) FFT.                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  ARR ( COMPLEX*16 ) -- Array to be transformed.                      *
! *                        Input for forward FFT:  array ordered in time.*      
! *                        Output of the backward FFT in this order:     *
! *                        [1, dim/2-1] -- increase in frequency order   *
! *                                      starting from the zero frequency*
! *                        [dim/2, dim] -- increase in frequency         *
! *                                        starting from -n/2 through 1/n*
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
! *  ### 04-AUG-2007   FFT_1D_C16  v2.0 (c) L. Petrov  23-JAN-2011 ###   *
! *                                                                      *
! ************************************************************************
#ifdef MKL
      USE        MKL_DFTI
      IMPLICIT   NONE
      TYPE     ( DFTI_DESCRIPTOR ), POINTER :: DESC_HANDLE
      INTEGER*4  STATUS, NUM_THR_OLD 
      INTEGER*4, EXTERNAL :: OMP_GET_NUM_THREADS, OMP_GET_MAX_THREADS
#else
      IMPLICIT   NONE
#endif
      INCLUDE    'fftw3.f'
      INTEGER*4   DIM, IEXP, IUER
      COMPLEX*16  ARR(DIM)
      CHARACTER   STR*128
!
      INTEGER*4   FFT_MODE, FFT_NUM_THR, FFT_DEBUG, LAST_DIMS(2)
      INTEGER*8   SFFTW_PLAN, DFFTW_PLAN
      REAL*8      FFTW_PAR_TIMEOUT 
      CHARACTER   LAST_OP*10
!
      COMMON    / FFT_PARS / DFFTW_PLAN, SFFTW_PLAN, FFTW_PAR_TIMEOUT, &
     &                       FFT_MODE, FFT_NUM_THR, FFT_DEBUG, &
     &                       LAST_DIMS, LAST_OP
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN
!
      IF ( FFT_MODE == -1 ) THEN
#ifdef MKL
!
! ======== This code is for the case when MKL is used
!
           STATUS = DFTICREATEDESCRIPTOR ( DESC_HANDLE, DFTI_DOUBLE, &
     &                                     DFTI_COMPLEX, 1, DIM )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1111, IUER, 'FFT_1D_C16', 'Error in '// &
     &              'DFTICREATEDESCRIPTOR: '//STR )
                RETURN 
           END IF 
!
           IF ( FFT_NUM_THR > -1 ) THEN
!
! ------------- Check the current maximum number of threads
!
                NUM_THR_OLD = OMP_GET_MAX_THREADS()
                IF ( FFT_DEBUG > 0 ) THEN
                     WRITE ( 6, * ) 'FFT_1D_C16: NUM_THR_OLD = ', &
     &                       NUM_THR_OLD, ' FFT_NUM_THR = ', FFT_NUM_THR
                END IF
                IF ( NUM_THR_OLD .NE. FFT_NUM_THR ) THEN
!
! ------------------ Temporarily reset the number of threads
!
                     CALL OMP_SET_NUM_THREADS ( MAX(1, FFT_NUM_THR)  )
                     IF ( FFT_DEBUG > 0 ) THEN
                          WRITE ( 6, * ) 'FFT_1D_C16: THe number of threads '// &
     &                                   'is reset to ', FFT_NUM_THR
                     END IF
                END IF
           END IF
!
           STATUS = DFTICOMMITDESCRIPTOR ( DESC_HANDLE )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1112, IUER, 'FFT_1D_C16', 'Error in '// &
     &              'DFTICOMMITDESCRIPTOR: '//STR )
                RETURN
           END IF 
!
           IF ( IEXP == 1 ) THEN
                STATUS = DFTICOMPUTEFORWARD ( DESC_HANDLE, ARR )
                IF ( STATUS .NE. 0 ) THEN
                     STR = DFTIERRORMESSAGE ( STATUS )
                     CALL ERR_LOG ( 1113, IUER, 'FFT_1D_C16', 'Error in '// &
     &                   'DFTICOMPUTEFORWARD: '//STR )
                     RETURN 
                END IF 
             ELSE 
                STATUS = DFTICOMPUTEBACKWARD ( DESC_HANDLE, ARR )
                IF ( STATUS .NE. 0 ) THEN
                     STR = DFTIERRORMESSAGE ( STATUS )
                     CALL ERR_LOG ( 1114, IUER, 'FFT_1D_C16', 'Error in '// &
     &                   'DFTICOMPUTEBACKWARD: '//STR )
                     RETURN 
                END IF 
          END IF 
!    
          STATUS = DFTIFREEDESCRIPTOR( DESC_HANDLE )
          IF ( STATUS .NE. 0 ) THEN
               STR = DFTIERRORMESSAGE ( STATUS )
               CALL ERR_LOG ( 1115, IUER, 'FFT_1D_C16', 'Error in '// &
     &             'DFTIFREEDESCRIPTOR: '//STR )
               RETURN 
          END IF 
!
          IF ( NUM_THR_OLD .NE. FFT_NUM_THR ) THEN
!
! ------------ Set the number of threads to the old value
!
               CALL OMP_SET_NUM_THREADS ( NUM_THR_OLD )
               IF ( FFT_DEBUG > 0 ) THEN
                    WRITE ( 6, * ) 'FFT_1D_C16: The number of threads '// &
     &                             'is set back to ', NUM_THR_OLD
               END IF
          END IF
!
          CALL ERR_LOG ( 0, IUER )
          RETURN 
#else
          CALL ERR_LOG ( 1116, IUER, 'FFT_1D_C16', 'Support of Intel MKL '// &
     &        'was not compiled in. Please, recompile your application '// &
     &        'with -D MKL option supplied to the compiler' )
          RETURN 
#endif
        ELSE 
!
! ====== This code is for the case when FFTW is used
!
          IF ( DFFTW_PLAN .NE. 0            .AND. &
     &         LAST_DIMS(1) == DIM          .AND. &
     &         LAST_DIMS(2) == 0            .AND. &
     &         IEXP         == 1            .AND. &
     &         LAST_OP      == '1D_C16_FOR'       ) THEN
!
! ------------ If plan for this type of transform and for these dimensions
! ------------ has alread been create -- use it!
!
               CONTINUE 
             ELSE IF ( DFFTW_PLAN .NE. 0    .AND. &
     &         LAST_DIMS(1) == DIM          .AND. &
     &         LAST_DIMS(2) == 0            .AND. &
     &         IEXP         == -1           .AND. &
     &         LAST_OP      == '1D_C16_BAC'       ) THEN
!
! ------------ If plan for this type of transform and for these dimensions
! ------------ has alread been create -- use it!
!
               CONTINUE 
             ELSE 
               IF ( DFFTW_PLAN .NE. 0 ) THEN
!
! ----------------- Destroy previously created plan in order to prevent
! ----------------- memory leakage
!
                    CALL FFTW_DESTROY_PLAN ( %VAL(DFFTW_PLAN) )
               END IF
               CALL FFTW_SET_TIMELIMIT     ( %VAL(FFTW_PAR_TIMEOUT) )
               IF ( IEXP == 1 ) THEN
!
! ----------------- Make a new plan for forward transform using wisdom
!
                   CALL DFFTW_PLAN_DFT_1D ( DFFTW_PLAN, DIM, ARR, ARR, &
     &                                      FFTW_FORWARD, &
     &                                      FFT_MODE + FFTW_WISDOM_ONLY )
                   IF ( DFFTW_PLAN == 0 ) THEN
                        IF ( FFT_DEBUG > 0 ) THEN
                              WRITE ( 6, * ) 'FFT_1D_C16_FOR: wisdom is '// &
     &                                       'unavailable, DIM = ', DIM, &
     &                                       ' FFT_MODE = ', FFT_MODE
                              IF ( FFT_DEBUG == 8 ) CALL EXIT ( 1 )
                        END IF
!
! --------------------- Fallback to estimate mode
!
                        CALL DFFTW_PLAN_DFT_1D ( DFFTW_PLAN, DIM, ARR, ARR, &
     &                                           FFTW_FORWARD, FFTW_ESTIMATE )
                   END IF
!
! ---------------- Store the type of the transform and dimensions
!
                   LAST_DIMS(1) = DIM
                   LAST_DIMS(2) = 0
                   LAST_OP = '1D_C16_FOR'
                 ELSE IF ( IEXP == -1 ) THEN
!
! ---------------- Make a new plan for backward transform using wisdom
!
                   CALL DFFTW_PLAN_DFT_1D ( DFFTW_PLAN, DIM, ARR, ARR, &
     &                                      FFTW_BACKWARD, &
     &                                      FFT_MODE + FFTW_WISDOM_ONLY )
                   IF ( DFFTW_PLAN == 0 ) THEN
                        IF ( FFT_DEBUG > 0 ) THEN
                              WRITE ( 6, * ) 'FFT_1D_C16_BAC: wisdom is '// &
     &                                       'unavailable, DIM = ', DIM, &
     &                                       ' FFT_MODE = ', FFT_MODE
                              IF ( FFT_DEBUG == 8 ) CALL EXIT ( 1 )
                        END IF
!
! --------------------- Fallback to estimate mode
!
                        CALL DFFTW_PLAN_DFT_1D ( DFFTW_PLAN, DIM, ARR, ARR, &
     &                                           FFTW_BACKWARD, FFTW_ESTIMATE )
                   END IF
!
! ---------------- Store the type of the transform and dimensions
!
                   LAST_DIMS(1) = DIM
                   LAST_DIMS(2) = 0
                   LAST_OP = '1D_C16_BAC'
                  ELSE 
                    CALL CLRCH ( STR ) 
                    CALL INCH  ( IEXP, STR )
                    CALL ERR_LOG ( 1127, IUER, 'FFT_1D_C16', 'Unsupported '// &
     &                  'value of the sign before exponent: '// &
     &                   STR(1:I_LEN(STR))//' only 1 or -1 are supported' ) 
                    RETURN 
               END IF
          END IF
!
          CALL DFFTW_EXECUTE_DFT ( DFFTW_PLAN, ARR, ARR )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFT_1D_C16 !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFT_1D_C8 ( DIM, IEXP, ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  FFT_1D_C8  performs fast Fourier transform in place,      *
! *   either forward ( IEXP = 1 ), or backward ( IEXP = -1 ) under       *
! *   the complex single precision array ARR of dimension DIM.           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  DIM ( INTEGER*4 ) -- Dimension of the array. NB: dimension that     *
! *                       is the power of 2 yileds the best performance. *
! * IEXP ( INTEGER*4 ) -- Direction of the transform:                    *
! *                        1 -- forward  (direct)  FFT.                  *
! *                       -1 -- backward (reverse) FFT.                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  ARR ( COMPLEX*8 )  -- Array to be transformed.                      *
! *                        Input for forward FFT:  array ordered in time.*      
! *                        Output of the backward FFT in this order:     *
! *                        [1, dim/2-1] -- increase in frequency order   *
! *                                      starting from the zero frequency*
! *                        [dim/2, dim] -- increase in frequency         *
! *                                        starting from -n/2 through 1/n*
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
! *  ### 04-AUG-2007   FFT_1D_C8  v2.0 (c)  L. Petrov  23-JAN-2011  ###  *
! *                                                                      *
! ************************************************************************
#ifdef MKL
      USE        MKL_DFTI
      IMPLICIT   NONE
      TYPE     ( DFTI_DESCRIPTOR ), POINTER :: DESC_HANDLE
      INTEGER*4  STATUS, NUM_THR_OLD 
      INTEGER*4, EXTERNAL :: OMP_GET_NUM_THREADS, OMP_GET_MAX_THREADS
#else
      IMPLICIT   NONE
#endif
      INCLUDE    'fftw3.f'
      INTEGER*4   DIM, IEXP, IUER
      COMPLEX*8   ARR(DIM)
      CHARACTER   STR*128
!
      INTEGER*4   FFT_MODE, FFT_NUM_THR, FFT_DEBUG, LAST_DIMS(2)
      INTEGER*8   SFFTW_PLAN, DFFTW_PLAN
      REAL*8      FFTW_PAR_TIMEOUT 
      CHARACTER   LAST_OP*10
!
      COMMON    / FFT_PARS / DFFTW_PLAN, SFFTW_PLAN, FFTW_PAR_TIMEOUT, &
     &                       FFT_MODE, FFT_NUM_THR, FFT_DEBUG, &
     &                       LAST_DIMS, LAST_OP
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN
!
      IF ( FFT_MODE == -1 ) THEN
#ifdef MKL
!
! ======== This code is for the case when MKL is used
!
!
           STATUS = DFTICREATEDESCRIPTOR ( DESC_HANDLE, DFTI_SINGLE, &
     &                                     DFTI_COMPLEX, 1, DIM )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1121, IUER, 'FFT_1D_C8', 'Error in '// &
     &              'DFTICREATEDESCRIPTOR: '//STR )
                RETURN 
           END IF 
!
           IF ( FFT_NUM_THR > -1 ) THEN
!
! ------------- Check the current maximum number of threads
!
                NUM_THR_OLD = OMP_GET_MAX_THREADS()
                IF ( FFT_DEBUG > 0 ) THEN
                     WRITE ( 6, * ) 'FFT_1D_C8: NUM_THR_OLD = ', &
     &                       NUM_THR_OLD, ' FFT_NUM_THR = ', FFT_NUM_THR
                END IF
                IF ( NUM_THR_OLD .NE. FFT_NUM_THR ) THEN
!
! ------------------ Temporarily reset the number of threads
!
                     CALL OMP_SET_NUM_THREADS ( MAX(1, FFT_NUM_THR)  )
                     IF ( FFT_DEBUG > 0 ) THEN
                          WRITE ( 6, * ) 'FFT_1D_C8: THe number of threads '// &
     &                                   'is reset to ', FFT_NUM_THR
                     END IF
                END IF
           END IF
!
!@           STATUS = DFTISETVALUE ( DESC_HANDLE, DFTI_PLACEMENT, DFTI_INPLACE )
!@           IF ( STATUS .NE. 0 ) THEN
!@                STR = DFTIERRORMESSAGE ( STATUS )
!@                CALL ERR_LOG ( 1182, IUER, 'FFT_1D_C8', 'Error in '// &
!@     &              'DFTSETVALUE: '//STR )
!@                RETURN 
!@           END IF 
!
           STATUS = DFTICOMMITDESCRIPTOR ( DESC_HANDLE )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1122, IUER, 'FFT_1D_C8', 'Error in '// &
     &              'DFTICOMMITDESCRIPTOR: '//STR )
                RETURN 
           END IF 
!
           IF ( IEXP == 1 ) THEN
                STATUS = DFTI_COMPUTE_FORWARD_C ( DESC_HANDLE, ARR )
!@@                STATUS = DFTICOMPUTEFORWARD ( DESC_HANDLE, ARR )
                IF ( STATUS .NE. 0 ) THEN
                     STR = DFTIERRORMESSAGE ( STATUS )
                     CALL ERR_LOG ( 1123, IUER, 'FFT_1D_C8', 'Error in '// &
     &                   'DFTICOMPUTEFORWARD: '//STR )
                     RETURN 
                END IF 
             ELSE 
!@@                STATUS = DFTICOMPUTEBACKWARD ( DESC_HANDLE, ARR )
                STATUS = DFTI_COMPUTE_BACKWARD_C ( DESC_HANDLE, ARR )
                IF ( STATUS .NE. 0 ) THEN
                     STR = DFTIERRORMESSAGE ( STATUS )
                     CALL ERR_LOG ( 1124, IUER, 'FFT_1D_C8', 'Error in '// &
     &                   'DFTICOMPUTEBACKWARD: '//STR )
                     RETURN 
                END IF 
          END IF 
!    
          STATUS = DFTIFREEDESCRIPTOR( DESC_HANDLE )
          IF ( STATUS .NE. 0 ) THEN
               STR = DFTIERRORMESSAGE ( STATUS )
               CALL ERR_LOG ( 1125, IUER, 'FFT_1D_C8', 'Error in '// &
     &             'DFTIFREEDESCRIPTOR: '//STR )
               RETURN 
          END IF 
!
          IF ( NUM_THR_OLD .NE. FFT_NUM_THR ) THEN
!
! ------------ Set the number of threads to the old value
!
               CALL OMP_SET_NUM_THREADS ( NUM_THR_OLD )
               IF ( FFT_DEBUG > 0 ) THEN
                    WRITE ( 6, * ) 'FFT_1D_C8: The number of threads '// &
     &                             'is set back to ', NUM_THR_OLD
               END IF
          END IF
!
          CALL ERR_LOG ( 0, IUER )
          RETURN 
#else
          CALL ERR_LOG ( 1126, IUER, 'FFT_1D_C8', 'Support of Intel MKL '// &
     &        'was not compiled in. Please, recompile your application '// &
     &        'with -D MKL option supplied to the compiler' )
          RETURN 
#endif
        ELSE 
          IF ( SFFTW_PLAN .NE. 0            .AND. &
     &         LAST_DIMS(1) == DIM          .AND. &
     &         LAST_DIMS(2) == 0            .AND. &
     &         IEXP         == 1            .AND. &
     &         LAST_OP      == '1D_C8_FORW'       ) THEN
!
! ------------ If plan for this type of transform and for these dimensions
! ------------ has alread been create -- use it!
!
               CONTINUE 
             ELSE IF ( SFFTW_PLAN .NE. 0    .AND. &
     &         LAST_DIMS(1) == DIM          .AND. &
     &         LAST_DIMS(2) == 0            .AND. &
     &         IEXP         == -1           .AND. &
     &         LAST_OP      == '1D_C8_BACK'       ) THEN
!
! ------------ If plan for this type of transform and for these dimensions
! ------------ has alread been create -- use it!
!
               CONTINUE 
             ELSE 
               IF ( SFFTW_PLAN .NE. 0 ) THEN
!
! ----------------- Destroy previously created plan in order to prevent
! ----------------- memory leakage
!
                    CALL FFTW_DESTROY_PLAN ( %VAL(SFFTW_PLAN) )
               END IF
               CALL FFTW_SET_TIMELIMIT     ( %VAL(FFTW_PAR_TIMEOUT) )
               IF ( IEXP == 1 ) THEN
!
! ----------------- Make a new plan for forward transform using wisdom
!
                   CALL SFFTW_PLAN_DFT_1D ( SFFTW_PLAN, DIM, ARR, ARR, &
     &                                      FFTW_FORWARD, &
     &                                      FFT_MODE + FFTW_WISDOM_ONLY )
                   IF ( SFFTW_PLAN == 0 ) THEN
                        IF ( FFT_DEBUG > 0 ) THEN
                              WRITE ( 6, * ) 'FFT_1D_C8_FORW: wisdom is '// &
     &                                       'unavailable, DIM = ', DIM, &
     &                                       ' FFT_MODE = ', FFT_MODE
                              IF ( FFT_DEBUG == 8 ) CALL EXIT ( 1 )
                        END IF
!
! --------------------- Fallback to estimate mode
!
                        CALL SFFTW_PLAN_DFT_1D ( SFFTW_PLAN, DIM, ARR, ARR, &
     &                                           FFTW_FORWARD, FFTW_ESTIMATE )
                   END IF
!
! ---------------- Store the type of the transform and dimensions
!
                   LAST_DIMS(1) = DIM
                   LAST_DIMS(2) = 0
                   LAST_OP = '1D_C8_FORW'
                 ELSE IF ( IEXP == -1 ) THEN
!
! ----------------- Make a new plan for backward transform using wisdom
!
                   CALL SFFTW_PLAN_DFT_1D ( SFFTW_PLAN, DIM, ARR, ARR, &
     &                                      FFTW_BACKWARD, &
     &                                      FFT_MODE + FFTW_WISDOM_ONLY )
                   IF ( SFFTW_PLAN == 0 ) THEN
                        IF ( FFT_DEBUG > 0 ) THEN
                              WRITE ( 6, * ) 'FFT_1D_C8_BACK: wisdom is '// &
     &                                       'unavailable, DIM = ', DIM, &
     &                                       ' FFT_MODE = ', FFT_MODE
                              IF ( FFT_DEBUG == 8 ) CALL EXIT ( 1 )
                        END IF
!
! --------------------- Fallback to estimate mode
!
                        CALL SFFTW_PLAN_DFT_1D ( SFFTW_PLAN, DIM, ARR, ARR, &
     &                                           FFTW_BACKWARD, FFTW_ESTIMATE )
                   END IF
!
! ---------------- Store the type of the transform and dimensions
!
                   LAST_DIMS(1) = DIM
                   LAST_DIMS(2) = 0
                   LAST_OP = '1D_C8_BACK'
                  ELSE 
                    CALL CLRCH ( STR ) 
                    CALL INCH  ( IEXP, STR )
                    CALL ERR_LOG ( 1127, IUER, 'FFT_1D_C8', 'Unsupported '// &
     &                  'value of the sign before exponent: '// &
     &                   STR(1:I_LEN(STR))//' only 1 or -1 are supported' ) 
                    RETURN 
               END IF
          END IF
!
          CALL SFFTW_EXECUTE_DFT ( SFFTW_PLAN, ARR, ARR )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFT_1D_C8 !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFT_2D_C16 ( DIM1, DIM2, IEXP, ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  FFT_2D_C16  performs fast Fourier transform in place,     *
! *   either forward ( IEXP = 1 ), or backward ( IEXP = -1 ) under       *
! *   the complex double precision 2-dimensional array ARR.              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * DIM1 ( INTEGER*4 ) -- 1st dimension of the array. NB: dimension that *
! *                       is the power of 2 yileds the best performance. *
! * DIM2 ( INTEGER*4 ) -- 2nd dimension of the array. NB: dimension that *
! *                       is the power of 2 yileds the best performance. *
! * IEXP ( INTEGER*4 ) -- Direction of the transform:                    *
! *                        1 -- forward  (direct)  FFT.                  *
! *                       -1 -- backward (reverse) FFT.                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  ARR ( COMPLEX*16 ) -- Array to be transformed.                      *
! *                        Input for forward FFT:  array ordered in time.*      
! *                        Output of the backward FFT in this order:     *
! *                        [1, dim/2-1] -- increase in frequency order   *
! *                                      starting from the zero frequency*
! *                        [dim/2, dim] -- increase in frequency         *
! *                                        starting from -n/2 through 1/n*
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
! *  ### 04-AUG-2007   FFT_2D_C16  v2.0 (c)  L. Petrov  23-JAN-2011 ###  *
! *                                                                      *
! ************************************************************************
#ifdef MKL
      USE        MKL_DFTI
      IMPLICIT   NONE
      TYPE     ( DFTI_DESCRIPTOR ), POINTER :: DESC_HANDLE
      INTEGER*4  LENGHTS(2), STATUS, NUM_THR_OLD 
      INTEGER*4, EXTERNAL :: OMP_GET_NUM_THREADS, OMP_GET_MAX_THREADS
#else
      IMPLICIT   NONE
#endif
      INCLUDE    'fftw3.f'
      INTEGER*4   DIM1, DIM2, IEXP, IUER
      COMPLEX*16  ARR(DIM1,DIM2)
      CHARACTER   STR*128
!
      INTEGER*4   FFT_MODE, FFT_NUM_THR, FFT_DEBUG, LAST_DIMS(2)
      INTEGER*8   SFFTW_PLAN, DFFTW_PLAN
      REAL*8      FFTW_PAR_TIMEOUT 
      CHARACTER   LAST_OP*10
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN
!
      COMMON    / FFT_PARS / DFFTW_PLAN, SFFTW_PLAN, FFTW_PAR_TIMEOUT, &
     &                       FFT_MODE, FFT_NUM_THR, FFT_DEBUG, &
     &                       LAST_DIMS, LAST_OP
!
      IF ( FFT_MODE == -1 ) THEN
#ifdef MKL
!
! ======== This code is for the case when MKL is used
!
           LENGHTS(1) = DIM1
           LENGHTS(2) = DIM2
           STATUS = DFTICREATEDESCRIPTOR ( DESC_HANDLE, DFTI_DOUBLE, &
     &                                     DFTI_COMPLEX, 2, LENGHTS )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1131, IUER, 'FFT_2D_C16', 'Error in '// &
     &              'DFTICREATEDESCRIPTOR: '//STR )
                RETURN 
           END IF 
!
           IF ( FFT_NUM_THR > -1 ) THEN
!
! ------------- Check the current maximum number of threads
!
                NUM_THR_OLD = OMP_GET_MAX_THREADS()
                IF ( FFT_DEBUG > 0 ) THEN
                     WRITE ( 6, * ) 'FFT_2D_C16: NUM_THR_OLD = ', &
     &                       NUM_THR_OLD, ' FFT_NUM_THR = ', FFT_NUM_THR
                END IF
                IF ( NUM_THR_OLD .NE. FFT_NUM_THR ) THEN
!
! ------------------ Temporarily reset the number of threads
!
                     CALL OMP_SET_NUM_THREADS ( MAX(1, FFT_NUM_THR)  )
                     IF ( FFT_DEBUG > 0 ) THEN
                          WRITE ( 6, * ) 'FFT_2D_C16: THe number of threads '// &
     &                                   'is reset to ', FFT_NUM_THR
                     END IF
                END IF
           END IF
!
           STATUS = DFTICOMMITDESCRIPTOR ( DESC_HANDLE )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1132, IUER, 'FFT_2D_C16', 'Error in '// &
     &              'DFTICOMMITDESCRIPTOR: '//STR )
                RETURN 
           END IF 
!
           IF ( IEXP == 1 ) THEN
                STATUS = DFTI_COMPUTE_FORWARD_Z ( DESC_HANDLE, ARR )
                IF ( STATUS .NE. 0 ) THEN
                     STR = DFTIERRORMESSAGE ( STATUS )
                     CALL ERR_LOG ( 1133, IUER, 'FFT_2D_C16', 'Error in '// &
     &                   'DFTI_COMPUTE_FORWARD_Z: '//STR )
                     RETURN 
                END IF 
             ELSE 
                STATUS = DFTI_COMPUTE_BACKWARD_Z ( DESC_HANDLE, ARR )
                IF ( STATUS .NE. 0 ) THEN
                     STR = DFTIERRORMESSAGE ( STATUS )
                     CALL ERR_LOG ( 1134, IUER, 'FFT_2D_C16', 'Error in '// &
     &                   'DFTI_COMPUTE_BACKWARD_Z: '//STR )
                     RETURN 
                END IF 
          END IF 
!    
          STATUS = DFTIFREEDESCRIPTOR( DESC_HANDLE )
          IF ( STATUS .NE. 0 ) THEN
               STR = DFTIERRORMESSAGE ( STATUS )
               CALL ERR_LOG ( 1135, IUER, 'FFT_2D_C16', 'Error in '// &
     &             'DFTIFREEDESCRIPTOR: '//STR )
               RETURN 
          END IF 
!
          IF ( NUM_THR_OLD .NE. FFT_NUM_THR ) THEN
!
! ------------ Set the number of threads to the old value
!
               CALL OMP_SET_NUM_THREADS ( NUM_THR_OLD )
               IF ( FFT_DEBUG > 0 ) THEN
                    WRITE ( 6, * ) 'FFT_2D_C16: The number of threads '// &
     &                             'is set back to ', NUM_THR_OLD
               END IF
          END IF
!
          CALL ERR_LOG ( 0, IUER )
          RETURN 
#else
          CALL ERR_LOG ( 1136, IUER, 'FFT_2D_C16', 'Support of Intel MKL '// &
     &        'was not compiled in. Please, recompile your application '// &
     &        'with -D MKL option supplied to the compiler' )
          RETURN 
#endif
        ELSE 
!
! ======= This code is for the case when FFTW is used
!
!
          IF ( DFFTW_PLAN .NE. 0            .AND. &
     &         LAST_DIMS(1) == DIM1         .AND. &
     &         LAST_DIMS(2) == DIM2         .AND. &
     &         IEXP         == 1            .AND. &
     &         LAST_OP      == '2D_C16_FOR'       ) THEN
!
! ------------ If plan for this type of transform and for these dimensions
! ------------ has alread been create -- use it!
!
               CONTINUE 
            ELSE IF ( DFFTW_PLAN .NE. 0            .AND. &
     &         LAST_DIMS(1) == DIM1         .AND. &
     &         LAST_DIMS(2) == DIM2         .AND. &
     &         IEXP         == -1           .AND. &
     &         LAST_OP      == '2D_C16_BAC'       ) THEN
!
! ------------ If plan for this type of transform and for these dimensions
! ------------ has alread been create -- use it!
!
               CONTINUE 
             ELSE 
               IF ( DFFTW_PLAN .NE. 0 ) THEN
!
! ----------------- Destroy previously created plan in order to prevent
! ----------------- memory leakage
!
                    CALL FFTW_DESTROY_PLAN ( %VAL(DFFTW_PLAN) )
               END IF
               CALL FFTW_SET_TIMELIMIT     ( %VAL(FFTW_PAR_TIMEOUT) )
               IF ( IEXP == 1 ) THEN
!
! ----------------- Make a new plan for forward transform using wisdom
!
                    CALL DFFTW_PLAN_DFT_2D ( DFFTW_PLAN, DIM1, DIM2, ARR, ARR, &
     &                                       FFTW_FORWARD, FFT_MODE + FFTW_WISDOM_ONLY )
                    IF ( DFFTW_PLAN == 0 ) THEN
                         IF ( FFT_DEBUG > 0 ) THEN
                              WRITE ( 6, * ) '2D_C16_FOR: wisdom is '// &
     &                                       'unavailable, '// &
     &                                       'DIM1/DIM2 = ', DIM1, DIM2, &
     &                                       ' FFT_MODE = ', FFT_MODE
                              IF ( FFT_DEBUG == 8 ) CALL EXIT ( 1 )
                         END IF
!
! ---------------------- Fallback to estimate mode
!
                         CALL DFFTW_PLAN_DFT_2D ( DFFTW_PLAN, DIM1, DIM2, &
     &                                            ARR, ARR, FFTW_FORWARD, &
     &                                            FFTW_ESTIMATE )
                    END IF
!
! ----------------- Store the type of the transform and dimensions
!
                    LAST_DIMS(1) = DIM1
                    LAST_DIMS(2) = DIM2
                    LAST_OP = '2D_C16_FOR'       
                  ELSE IF ( IEXP == -1 ) THEN
!
! ----------------- Make a new plan for inverse transform using wisdom
!
                    CALL DFFTW_PLAN_DFT_2D ( DFFTW_PLAN, DIM1, DIM2, ARR, ARR, &
     &                                       FFTW_BACKWARD, FFT_MODE + FFTW_WISDOM_ONLY )
                    IF ( DFFTW_PLAN == 0 ) THEN
                         IF ( FFT_DEBUG > 0 ) THEN
                              WRITE ( 6, * ) '2D_C16_BAC: wisdom is '// &
     &                                       'unavailable, '// &
     &                                       'DIM1/DIM2 = ', DIM1, DIM2, &
     &                                       ' FFT_MODE = ', FFT_MODE
                              IF ( FFT_DEBUG == 8 ) CALL EXIT ( 1 )
                         END IF
!
! ---------------------- Fallback to estimate mode
!
                         CALL DFFTW_PLAN_DFT_2D ( DFFTW_PLAN, DIM1, DIM2, &
     &                                            ARR, ARR, FFTW_BACKWARD, &
     &                                            FFTW_ESTIMATE )
                    END IF
!
! ----------------- Store the type of the transform and dimensions
!
                    LAST_DIMS(1) = DIM1
                    LAST_DIMS(2) = DIM2
                    LAST_OP = '2D_C16_BAC'       
                  ELSE 
                    CALL CLRCH ( STR ) 
                    CALL INCH  ( IEXP, STR )
                    CALL ERR_LOG ( 1127, IUER, 'FFT_2D_C16', 'Unsupported '// &
     &                  'value of the sign before exponent: '// &
     &                   STR(1:I_LEN(STR))//' only 1 or -1 are supported' ) 
                    RETURN 
               END IF
          END IF
!
! ------- Execute the FFT transform
!
          CALL DFFTW_EXECUTE_DFT ( DFFTW_PLAN, ARR, ARR )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFT_2D_C16 !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFT_2D_C8 ( DIM1, DIM2, IEXP, ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  FFT_2D_C8  performs fast Fourier transform in place,      *
! *   either forward ( IEXP = 1 ), or backward ( IEXP = -1 ) under       *
! *   the complex single precision 2-dimensional array ARR.              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * DIM1 ( INTEGER*4 ) -- 1st dimension of the array. NB: dimension that *
! *                       is the power of 2 yileds the best performance. *
! * DIM2 ( INTEGER*4 ) -- 2nd dimension of the array. NB: dimension that *
! *                       is the power of 2 yileds the best performance. *
! * IEXP ( INTEGER*4 ) -- Direction of the transform:                    *
! *                        1 -- forward  (direct)  FFT.                  *
! *                       -1 -- backward (reverse) FFT.                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  ARR ( COMPLEX*8 )  -- Array to be transformed.                      *
! *                        Input for forward FFT:  array ordered in time.*      
! *                        Output of the backward FFT in this order:     *
! *                        [1, dim/2-1] -- increase in frequency order   *
! *                                      starting from the zero frequency*
! *                        [dim/2, dim] -- increase in frequency         *
! *                                        starting from -n/2 through 1/n*
! *                                                                      *
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
! *  ### 04-AUG-2007   FFT_2D_C8   v2.0 (c) L. Petrov  23-JAN-2011  ###  *
! *                                                                      *
! ************************************************************************
#ifdef MKL
      USE        MKL_DFTI
      IMPLICIT   NONE
      TYPE     ( DFTI_DESCRIPTOR ), POINTER :: DESC_HANDLE
      INTEGER*4  LENGHTS(2), NUM_THR_OLD
      INTEGER*4  STATUS
      INTEGER*4, EXTERNAL :: OMP_GET_NUM_THREADS, OMP_GET_MAX_THREADS
#else
      IMPLICIT   NONE
#endif
      INCLUDE    'fftw3.f'
      INTEGER*4   DIM1, DIM2, IEXP, IUER
      COMPLEX*8   ARR(DIM1,DIM2)
      CHARACTER   STR*128
!
      INTEGER*4   FFT_MODE, FFT_NUM_THR, FFT_DEBUG, LAST_DIMS(2)
      INTEGER*8   SFFTW_PLAN, DFFTW_PLAN
      REAL*8      FFTW_PAR_TIMEOUT 
      CHARACTER   LAST_OP*10
!
      COMMON    / FFT_PARS / DFFTW_PLAN, SFFTW_PLAN, FFTW_PAR_TIMEOUT, &
     &                       FFT_MODE, FFT_NUM_THR, FFT_DEBUG, &
     &                       LAST_DIMS, LAST_OP
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN
!
      IF ( FFT_MODE == -1 ) THEN
#ifdef MKL
!
! ======== This code is for the case when MKL is used
!
           LENGHTS(1) = DIM1
           LENGHTS(2) = DIM2
           STATUS = DFTICREATEDESCRIPTOR ( DESC_HANDLE, DFTI_SINGLE, &
     &                                     DFTI_COMPLEX, 2, LENGHTS )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1141, IUER, 'FFT_2D_C8', 'Error in '// &
     &              'DFTICREATEDESCRIPTOR: '//STR )
                RETURN 
           END IF 
!
           IF ( FFT_NUM_THR > -1 ) THEN
!
! ------------- Check the current maximum number of threads
!
                NUM_THR_OLD = OMP_GET_MAX_THREADS()
                IF ( FFT_DEBUG > 0 ) THEN
                     WRITE ( 6, * ) 'FFT_2D_C8: NUM_THR_OLD = ', &
     &                       NUM_THR_OLD, ' FFT_NUM_THR = ', FFT_NUM_THR
                END IF
                IF ( NUM_THR_OLD .NE. FFT_NUM_THR ) THEN
!
! ------------------ Temporarily reset the number of threads
!
                     CALL OMP_SET_NUM_THREADS ( MAX(1, FFT_NUM_THR)  )
                     IF ( FFT_DEBUG > 0 ) THEN
                          WRITE ( 6, * ) 'FFT_2D_C8: THe number of threads '// &
     &                                   'is reset to ', FFT_NUM_THR
                     END IF
                END IF
           END IF
!
           STATUS = DFTICOMMITDESCRIPTOR ( DESC_HANDLE )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1142, IUER, 'FFT_2D_C8', 'Error in '// &
     &              'DFTICOMMITDESCRIPTOR: '//STR )
                RETURN 
           END IF 
!
           IF ( IEXP == 1 ) THEN
                STATUS = DFTI_COMPUTE_FORWARD_C ( DESC_HANDLE, ARR )
                IF ( STATUS .NE. 0 ) THEN
                     STR = DFTIERRORMESSAGE ( STATUS )
                     CALL ERR_LOG ( 1143, IUER, 'FFT_2D_C8', 'Error in '// &
     &                   'DFTI_COMPUTE_FORWARD_C '//STR )
                     RETURN 
                END IF 
             ELSE 
                STATUS = DFTI_COMPUTE_BACKWARD_C ( DESC_HANDLE, ARR )
                IF ( STATUS .NE. 0 ) THEN
                     STR = DFTIERRORMESSAGE ( STATUS )
                     CALL ERR_LOG ( 1144, IUER, 'FFT_2D_C8', 'Error in '// &
     &                   'DFTI_COMPUTE_BACKWARD_C: '//STR )
                     RETURN 
                END IF 
          END IF 
!    
          STATUS = DFTIFREEDESCRIPTOR( DESC_HANDLE )
          IF ( STATUS .NE. 0 ) THEN
               STR = DFTIERRORMESSAGE ( STATUS )
               CALL ERR_LOG ( 1145, IUER, 'FFT_2D_C8', 'Error in '// &
     &             'DFTIFREEDESCRIPTOR: '//STR )
               RETURN 
          END IF 
!
          IF ( NUM_THR_OLD .NE. FFT_NUM_THR ) THEN
!
! ------------ Set the number of threads to the old value
!
               CALL OMP_SET_NUM_THREADS ( NUM_THR_OLD )
               IF ( FFT_DEBUG > 0 ) THEN
                    WRITE ( 6, * ) 'FFT_2D_C8: The number of threads '// &
     &                             'is set back to ', NUM_THR_OLD
               END IF
          END IF
!
          CALL ERR_LOG ( 0, IUER )
          RETURN 
#else
          CALL ERR_LOG ( 1136, IUER, 'FFT_2D_C8', 'Support of Intel MKL '// &
     &        'was not compiled in. Please, recompile your application '// &
     &        'with -D MKL option supplied to the compiler' )
          RETURN 
#endif
        ELSE 
!
! ======= This code is for the case when FFTW is used
!
!
          IF ( SFFTW_PLAN .NE. 0            .AND. &
     &         LAST_DIMS(1) == DIM1         .AND. &
     &         LAST_DIMS(2) == DIM2         .AND. &
     &         IEXP         == 1            .AND. &
     &         LAST_OP      == '2D_C8_FORW'       ) THEN
!
! ------------ If plan for this type of transform and for these dimensions
! ------------ has alread been create -- use it!
!
               CONTINUE 
            ELSE IF ( SFFTW_PLAN .NE. 0            .AND. &
     &         LAST_DIMS(1) == DIM1         .AND. &
     &         LAST_DIMS(2) == DIM2         .AND. &
     &         IEXP         == -1           .AND. &
     &         LAST_OP      == '2D_C8_BACK'       ) THEN
!
! ------------ If plan for this type of transform and for these dimensions
! ------------ has alread been create -- use it!
!
               CONTINUE 
             ELSE 
               IF ( SFFTW_PLAN .NE. 0 ) THEN
!
! ----------------- Destroy previously created plan in order to prevent
! ----------------- memory leakage
!
                    CALL FFTW_DESTROY_PLAN ( %VAL(SFFTW_PLAN) )
               END IF
               CALL FFTW_SET_TIMELIMIT    ( %VAL(FFTW_PAR_TIMEOUT) )
               IF ( IEXP == 1 ) THEN
!
! ----------------- Make a new plan for forward transform using wisdom
!
                    CALL SFFTW_PLAN_DFT_2D ( SFFTW_PLAN, DIM1, DIM2, ARR, ARR, &
     &                                       FFTW_FORWARD, FFT_MODE + FFTW_WISDOM_ONLY )
                    IF ( SFFTW_PLAN == 0 ) THEN
                         IF ( FFT_DEBUG > 0 ) THEN
                              WRITE ( 6, * ) '2D_C8_FORW: wisdom is '// &
     &                                       'unavailable, '// &
     &                                       'DIM1/DIM2 = ', DIM1, DIM2, &
     &                                       ' FFT_MODE = ', FFT_MODE
                              IF ( FFT_DEBUG == 8 ) CALL EXIT ( 1 )
                         END IF
!
! ---------------------- Fallback to estimate mode
!
                         CALL SFFTW_PLAN_DFT_2D ( SFFTW_PLAN, DIM1, DIM2, &
     &                                            ARR, ARR, FFTW_FORWARD, &
     &                                            FFTW_ESTIMATE )
                    END IF
!
! ----------------- Store the type of the transform and dimensions
!
                    LAST_DIMS(1) = DIM1
                    LAST_DIMS(2) = DIM2
                    LAST_OP = '2D_C8_FORW'       
                  ELSE IF ( IEXP == -1 ) THEN
!
! ----------------- Make a new plan for inverse transform using wisdom
!
                    CALL SFFTW_PLAN_DFT_2D ( SFFTW_PLAN, DIM1, DIM2, ARR, ARR, &
     &                                       FFTW_BACKWARD, FFT_MODE + FFTW_WISDOM_ONLY )
                    IF ( SFFTW_PLAN == 0 ) THEN
                         IF ( FFT_DEBUG > 0 ) THEN
                              WRITE ( 6, * ) '2D_C8_BACK: wisdom is '// &
     &                                       'unavailable, '// &
     &                                       'DIM1/DIM2 = ', DIM1, DIM2, &
     &                                       ' FFT_MODE = ', FFT_MODE
                              IF ( FFT_DEBUG == 8 ) CALL EXIT ( 1 )
                         END IF
!
! ---------------------- Fallback to estimate mode
!
                         CALL SFFTW_PLAN_DFT_2D ( SFFTW_PLAN, DIM1, DIM2, &
     &                                            ARR, ARR, FFTW_BACKWARD, &
     &                                            FFTW_ESTIMATE )
                    END IF
!
! ----------------- Store the type of the transform and dimensions
!
                    LAST_DIMS(1) = DIM1
                    LAST_DIMS(2) = DIM2
                    LAST_OP = '2D_C8_BACK'       
                  ELSE 
                    CALL CLRCH ( STR ) 
                    CALL INCH  ( IEXP, STR )
                    CALL ERR_LOG ( 1137, IUER, 'FFT_2D_C8', 'Unsupported '// &
     &                  'value of the sign before exponent: '// &
     &                   STR(1:I_LEN(STR))//' only 1 or -1 are supported' ) 
                    RETURN 
               END IF
          END IF
!
! ------- Execute the FFT transform
!
          CALL SFFTW_EXECUTE_DFT ( SFFTW_PLAN, ARR, ARR )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFT_2D_C8 !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFT_1D_R2C_R8 ( DIM, ARR_R8, ARR_C16, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine FFT_1D_R2C_R8 performs forward fast Fourier transform      *
! *   under the real*8 array ARR_R8 of dimension DIM. Results is         *
! *   complex*16 array ARR_C16 of n/2+1 for non-negative frequencies.    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     DIM ( INTEGER*4  ) -- Dimension of the array input.              *
! *                           NB: dimension that is the power of         *
! *                           2 yileds the best performance.             *
! *  ARR_R8 ( REAL*8     ) -- Array to be transformed. Should be time    *
! *                           ordered.                                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * ARR_C16 ( COMPLEX*16 ) -- Fourier transform of the input array.      *
! *                           Dimension: DIM/2+1. Frequencies start      *
! *                           from 0 and run through dim/2               *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! * ### 04-AUG-2007   FFT_1D_R2C_R8  v1.0 (c) L. Petrov 23-JAN-2011 ###  *
! *                                                                      *
! ************************************************************************
#ifdef MKL
      USE        MKL_DFTI
      IMPLICIT   NONE
      TYPE     ( DFTI_DESCRIPTOR ), POINTER :: DESC_HANDLE
      INTEGER*4  NUM_THR_OLD, STATUS
      INTEGER*4, EXTERNAL :: OMP_GET_NUM_THREADS, OMP_GET_MAX_THREADS
#else
      IMPLICIT   NONE
#endif
      INCLUDE    'fftw3.f'
      INTEGER*4   DIM, IEXP, IUER
      REAL*8      ARR_R8(DIM)
      COMPLEX*16  ARR_C16(DIM/2+1)
      CHARACTER   STR*128
!
      INTEGER*4   FFT_MODE, FFT_NUM_THR, FFT_DEBUG, LAST_DIMS(2)
      INTEGER*8   SFFTW_PLAN, DFFTW_PLAN
      REAL*8      FFTW_PAR_TIMEOUT 
      CHARACTER   LAST_OP*10
!
      COMMON    / FFT_PARS / DFFTW_PLAN, SFFTW_PLAN, FFTW_PAR_TIMEOUT, &
     &                       FFT_MODE, FFT_NUM_THR, FFT_DEBUG, &
     &                       LAST_DIMS, LAST_OP
!
      IF ( FFT_MODE == -1 ) THEN
#ifdef MKL
!
! ======== This code is for the case when MKL is used
!
           STATUS = DFTICREATEDESCRIPTOR ( DESC_HANDLE, DFTI_DOUBLE, &
     &                                     DFTI_REAL, 1, DIM )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1151, IUER, 'FFT_1D_R2C_R8', 'Error in '// &
     &              'DFTICREATEDESCRIPTOR: '//STR )
                RETURN 
           END IF 
           IF ( FFT_NUM_THR > -1 ) THEN
!
! ------------- Check the current maximum number of threads
!
                NUM_THR_OLD = OMP_GET_MAX_THREADS()
                IF ( FFT_DEBUG > 0 ) THEN
                     WRITE ( 6, * ) 'FFT_1D_R2C_R8: NUM_THR_OLD = ', &
     &                       NUM_THR_OLD, ' FFT_NUM_THR = ', FFT_NUM_THR
                END IF
                IF ( NUM_THR_OLD .NE. FFT_NUM_THR ) THEN
!
! ------------------ Temporarily reset the number of threads
!
                     CALL OMP_SET_NUM_THREADS ( MAX(1, FFT_NUM_THR)  )
                     IF ( FFT_DEBUG > 0 ) THEN
                          WRITE ( 6, * ) 'FFT_1D_R2C_R8: THe number of threads '// &
     &                                   'is reset to ', FFT_NUM_THR
                     END IF
                END IF
           END IF
!
           STATUS = DFTISETVALUE ( DESC_HANDLE, DFTI_PLACEMENT, DFTI_NOT_INPLACE )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1152, IUER, 'FFT_1D_R2C_R8', 'Error in '// &
     &              'DFTSETVALUE: '//STR )
                RETURN 
           END IF 
!
           STATUS = DFTICOMMITDESCRIPTOR ( DESC_HANDLE )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1153, IUER, 'FFT_1D_R2C_R8', 'Error in '// &
     &              'DFTICOMMITDESCRIPTOR: '//STR )
                RETURN 
           END IF 
!
           STATUS = DFTICOMPUTEFORWARD ( DESC_HANDLE, ARR_R8, ARR_C16 )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1154, IUER, 'FFT_1D_R2C_R8', 'Error in '// &
     &              'DFTI_COMPUTE_FORWARD_DZ: '//STR )
                RETURN 
           END IF 
!    
           STATUS = DFTIFREEDESCRIPTOR( DESC_HANDLE )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1155, IUER, 'FFT_1D_R2C_R8', 'Error in '// &
     &              'DFTIFREEDESCRIPTOR: '//STR )
                RETURN 
           END IF 
!
           IF ( NUM_THR_OLD .NE. FFT_NUM_THR ) THEN
!
! ------------- Set the number of threads to the old value
!
                CALL OMP_SET_NUM_THREADS ( NUM_THR_OLD )
                IF ( FFT_DEBUG > 0 ) THEN
                     WRITE ( 6, * ) 'FFT_1D_R2C_R8: The number of threads '// &
     &                              'is set back to ', NUM_THR_OLD
                END IF
           END IF
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
#else
           CALL ERR_LOG ( 1156, IUER, 'FFT_1D_R2C_R8', 'Support of Intel MKL '// &
     &         'was not compiled in. Please, recompile your application '// &
     &         'with -D MKL option supplied to the compiler' )
           RETURN 
#endif
        ELSE 
!
! ====== This code is for the case when FFTW is used
!
          IF ( DFFTW_PLAN .NE. 0            .AND. &
     &         LAST_DIMS(1) == DIM          .AND. &
     &         LAST_DIMS(2) == 0            .AND. &
     &         LAST_OP      == '1D_R2C_R8 '       ) THEN
!
               CONTINUE 
             ELSE 
               IF ( DFFTW_PLAN .NE. 0 ) THEN
                    CALL FFTW_DESTROY_PLAN ( %VAL(DFFTW_PLAN) )
               END IF
               CALL FFTW_SET_TIMELIMIT    ( %VAL(FFTW_PAR_TIMEOUT) )
               CALL DFFTW_PLAN_DFT_R2C_1D ( DFFTW_PLAN, DIM, ARR_R8, ARR_C16, &
     &                                      FFT_MODE + FFTW_WISDOM_ONLY )
               IF ( DFFTW_PLAN == 0 ) THEN
                    IF ( FFT_DEBUG > 0 ) THEN
                         WRITE ( 6, * ) 'FFT_1D_R2C_R8: wisdom is '// &
     &                                  'unavailable, DIM = ', DIM, &
     &                                  ' FFT_MODE = ', FFT_MODE
                         IF ( FFT_DEBUG == 8 ) CALL EXIT ( 1 )
                    END IF
                    CALL DFFTW_PLAN_DFT_R2C_1D ( DFFTW_PLAN, DIM, ARR_R8, ARR_C16, &
     &                                           FFTW_ESTIMATE )
               END IF
!
               LAST_DIMS(1) = DIM
               LAST_DIMS(2) = 0
               LAST_OP = '1D_R2C_R8 '
          END IF
!
          CALL DFFTW_EXECUTE_DFT_R2C ( DFFTW_PLAN, ARR_R8, ARR_C16 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFT_1D_R2C_R8  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFT_1D_R2C_R4 ( DIM, ARR_R4, ARR_C8, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  FFT_1D_R2C_R4  performs forward fast Fourier transform    *
! *   under the real*4 array ARR_R4 of dimension DIM. Results is         *
! *   complex*8 array ARR_C8 of n/2+1 for non-negative frequencies.      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    DIM ( INTEGER*4  ) -- Dimension of the array input. NB: dimension *
! *                          that is the power of 2 yileds the best      *
! *                          performance.                                *
! * ARR_R4 ( REAL*4     ) -- Real array to be transformed. Should be     *
! *                          time ordered.                               *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  ARR_C8 ( COMPLEX*8 ) -- Fourier transform of the input array.       *
! *                          Dimension: DIM/2+1. Frequencies start from  *
! *                          0 and run through dim/2                     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! * ### 04-AUG-2007   FFT_1D_R2C_R4  v2.0 (c)  L. Petrov 23-JAN-2011 ### *
! *                                                                      *
! ************************************************************************
#ifdef MKL
      USE        MKL_DFTI
      IMPLICIT   NONE
      TYPE     ( DFTI_DESCRIPTOR ), POINTER :: DESC_HANDLE
      INTEGER*4  NUM_THR_OLD, STATUS
      INTEGER*4, EXTERNAL :: OMP_GET_NUM_THREADS, OMP_GET_MAX_THREADS
#else
      IMPLICIT   NONE
#endif
      INCLUDE    'fftw3.f'
      INTEGER*4   DIM, IEXP, IUER
      REAL*4      ARR_R4(DIM)
      COMPLEX*8   ARR_C8(DIM/2+1)
      CHARACTER   STR*128
!
      INTEGER*4   FFT_MODE, FFT_NUM_THR, FFT_DEBUG, LAST_DIMS(2)
      INTEGER*8   SFFTW_PLAN, DFFTW_PLAN
      REAL*8      FFTW_PAR_TIMEOUT 
      CHARACTER   LAST_OP*10
!
      COMMON    / FFT_PARS / DFFTW_PLAN, SFFTW_PLAN, FFTW_PAR_TIMEOUT, &
     &                       FFT_MODE, FFT_NUM_THR, FFT_DEBUG, &
     &                       LAST_DIMS, LAST_OP
!
      IF ( FFT_MODE == -1 ) THEN
#ifdef MKL
!
! ======== This code is for the case when MKL is used
!
           STATUS = DFTICREATEDESCRIPTOR ( DESC_HANDLE, DFTI_SINGLE, &
     &                                     DFTI_REAL, 1, DIM )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1161, IUER, 'FFT_1D_R2C_R4', 'Error in '// &
     &              'DFTICREATEDESCRIPTOR: '//STR )
                RETURN 
           END IF 
           IF ( FFT_NUM_THR > -1 ) THEN
!
! ------------- Check the current maximum number of threads
!
                NUM_THR_OLD = OMP_GET_MAX_THREADS()
                IF ( FFT_DEBUG > 0 ) THEN
                     WRITE ( 6, * ) 'FFT_1D_R2C_R4: NUM_THR_OLD = ', &
     &                       NUM_THR_OLD, ' FFT_NUM_THR = ', FFT_NUM_THR
                END IF
                IF ( NUM_THR_OLD .NE. FFT_NUM_THR ) THEN
!
! ------------------ Temporarily reset the number of threads
!
                     CALL OMP_SET_NUM_THREADS ( MAX(1, FFT_NUM_THR)  )
                     IF ( FFT_DEBUG > 0 ) THEN
                          WRITE ( 6, * ) 'FFT_1D_R2C_R4: THe number of threads '// &
     &                                   'is reset to ', FFT_NUM_THR
                     END IF
                END IF
           END IF
!
           STATUS = DFTISETVALUE ( DESC_HANDLE, DFTI_PLACEMENT, DFTI_NOT_INPLACE )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1162, IUER, 'FFT_1D_R2C_R4', 'Error in '// &
     &              'DFTSETVALUE: '//STR )
                RETURN 
           END IF 
!
           STATUS = DFTICOMMITDESCRIPTOR ( DESC_HANDLE )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1163, IUER, 'FFT_1D_R2C_R4', 'Error in '// &
     &              'DFTICOMMITDESCRIPTOR: '//STR )
                RETURN 
           END IF 
!
           STATUS = DFTICOMPUTEFORWARD ( DESC_HANDLE, ARR_R4, ARR_C8 )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1164, IUER, 'FFT_1D_R2C_R4', 'Error in '// &
     &              'DFTI_COMPUTE_FORWARD_DZ: '//STR )
                RETURN 
           END IF 
!    
           STATUS = DFTIFREEDESCRIPTOR( DESC_HANDLE )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1165, IUER, 'FFT_1D_R2C_R4', 'Error in '// &
     &              'DFTIFREEDESCRIPTOR: '//STR )
                RETURN 
           END IF 
!
           IF ( NUM_THR_OLD .NE. FFT_NUM_THR ) THEN
!
! ------------- Set the number of threads to the old value
!
                CALL OMP_SET_NUM_THREADS ( NUM_THR_OLD )
                IF ( FFT_DEBUG > 0 ) THEN
                     WRITE ( 6, * ) 'FFT_1D_R2C_R4: The number of threads '// &
     &                              'is set back to ', NUM_THR_OLD
                END IF
           END IF
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
#else
           CALL ERR_LOG ( 1166, IUER, 'FFT_1D_R2C_R4', 'Support of Intel MKL '// &
     &         'was not compiled in. Please, recompile your application '// &
     &         'with -D MKL option supplied to the compiler' )
           RETURN 
#endif
        ELSE 
          IF ( SFFTW_PLAN .NE. 0            .AND. &
     &         LAST_DIMS(1) == DIM          .AND. &
     &         LAST_DIMS(2) == 0            .AND. &
     &         LAST_OP      == '1D_R2C_R4 '       ) THEN
!
               CONTINUE 
             ELSE 
               IF ( SFFTW_PLAN .NE. 0 ) THEN
                    CALL FFTW_DESTROY_PLAN ( %VAL(SFFTW_PLAN) )
               END IF
               CALL FFTW_SET_TIMELIMIT    ( %VAL(FFTW_PAR_TIMEOUT) )
               CALL SFFTW_PLAN_DFT_R2C_1D ( SFFTW_PLAN, DIM, ARR_R4, ARR_C8, &
     &                                      FFT_MODE + FFTW_WISDOM_ONLY )
               IF ( SFFTW_PLAN == 0 ) THEN
                    IF ( FFT_DEBUG > 0 ) THEN
                         WRITE ( 6, * ) 'FFT_1D_R2C_R4: wisdom is '// &
     &                                  'unavailable, DIM = ', DIM, &
     &                                  ' FFT_MODE = ', FFT_MODE
                         IF ( FFT_DEBUG == 8 ) CALL EXIT ( 1 )
                    END IF
                    CALL SFFTW_PLAN_DFT_R2C_1D ( SFFTW_PLAN, DIM, ARR_R4, ARR_C8, &
     &                                           FFTW_ESTIMATE )
               END IF
               LAST_DIMS(1) = DIM
               LAST_DIMS(2) = 0
               LAST_OP = '1D_R2C_R4 '
          END IF
!
          CALL SFFTW_EXECUTE_DFT_R2C ( SFFTW_PLAN, ARR_R4, ARR_C8 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFT_1D_R2C_R4 !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFT_1D_C2R_C16 ( DIM, ARR_C16, ARR_R8, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  FFT_1D_C2R_C16  performs backward (inverse) fast Fourier  *
! *   transform under the complex*16 array ARR_C16 of the 1-dimensional  *
! *   Hermtian-conjugated spectrum of dimension DIM. ARR_C16 keeps only  *
! *   dim/2+1 elements for non-negative frequencies. Result, the real*8  *
! *   array in time order, is put in the output array ARR_R8.            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     DIM ( INTEGER*4  ) -- Dimension of the array input.              *
! *                           NB: dimension that is the power of 2       *
! *                           yileds the best performance.               *
! * ARR_C16 ( COMPLEX*16 ) -- Array to be transformed. A portion of the  *
! *                           Hermitian-conjugated spectrum for          *
! *                           non-negative frequencies. Dimension:       *
! *                           DIM/2+1. Frequencies start from 0 and run  *
! *                           through dim/2.                             *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  ARR_R8 ( REAL*8     ) -- Array to be transformed. Should be time    *
! *                           ordered.                                   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! * ### 04-AUG-2007  FFTW_1D_C2R_C16  v2.0 (c) L. Petrov 23-JAN-2011 ### *
! *                                                                      *
! ************************************************************************
!
#ifdef MKL
      USE        MKL_DFTI
      IMPLICIT   NONE
      TYPE     ( DFTI_DESCRIPTOR ), POINTER :: DESC_HANDLE
      INTEGER*4  NUM_THR_OLD, STATUS
      INTEGER*4, EXTERNAL :: OMP_GET_NUM_THREADS, OMP_GET_MAX_THREADS
      CHARACTER  STR*128
#else
      IMPLICIT   NONE
#endif
      INCLUDE    'fftw3.f'
      INTEGER*4   DIM, IEXP, IUER
      COMPLEX*16  ARR_C16(DIM/2+1)
      REAL*8      ARR_R8(DIM)
!
      INTEGER*4   FFT_MODE, FFT_NUM_THR, FFT_DEBUG, LAST_DIMS(2)
      INTEGER*8   SFFTW_PLAN, DFFTW_PLAN
      REAL*8      FFTW_PAR_TIMEOUT 
      CHARACTER   LAST_OP*10
!
      COMMON    / FFT_PARS / DFFTW_PLAN, SFFTW_PLAN, FFTW_PAR_TIMEOUT, &
     &                       FFT_MODE, FFT_NUM_THR, FFT_DEBUG, &
     &                       LAST_DIMS, LAST_OP
!
      IF ( FFT_MODE == -1 ) THEN
#ifdef MKL
!
! ======== This code is for the case when MKL is used
!
           STATUS = DFTICREATEDESCRIPTOR ( DESC_HANDLE, DFTI_DOUBLE, &
     &                                     DFTI_REAL, 1, DIM )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1171, IUER, 'FFT_1D_C2R_C16', 'Error in '// &
     &              'DFTICREATEDESCRIPTOR: '//STR )
                RETURN 
           END IF 
           IF ( FFT_NUM_THR > -1 ) THEN
!
! ------------- Check the current maximum number of threads
!
                NUM_THR_OLD = OMP_GET_MAX_THREADS()
                IF ( FFT_DEBUG > 0 ) THEN
                     WRITE ( 6, * ) 'FFT_1D_C2R_C16: NUM_THR_OLD = ', &
     &                       NUM_THR_OLD, ' FFT_NUM_THR = ', FFT_NUM_THR
                END IF
                IF ( NUM_THR_OLD .NE. FFT_NUM_THR ) THEN
!
! ------------------ Temporarily reset the number of threads
!
                     CALL OMP_SET_NUM_THREADS ( MAX(1, FFT_NUM_THR)  )
                     IF ( FFT_DEBUG > 0 ) THEN
                          WRITE ( 6, * ) 'FFT_1D_C2R_C16: THe number of '// &
     &                                   'threads is reset to ', FFT_NUM_THR
                     END IF
                END IF
           END IF
!
           STATUS = DFTISETVALUE ( DESC_HANDLE, DFTI_PLACEMENT, DFTI_NOT_INPLACE )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1172, IUER, 'FFT_1D_C2R_C16', 'Error in '// &
     &              'DFTSETVALUE: '//STR )
                RETURN 
           END IF 
!
           STATUS = DFTICOMMITDESCRIPTOR ( DESC_HANDLE )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1173, IUER, 'FFT_1D_C2R_C16', 'Error in '// &
     &              'DFTICOMMITDESCRIPTOR: '//STR )
                RETURN 
           END IF 
!
           STATUS = DFTICOMPUTEBACKWARD ( DESC_HANDLE, ARR_C16, ARR_R8 )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1174, IUER, 'FFT_1D_C2R_C16', 'Error in '// &
     &              'DFTI_COMPUTE_FORWARD_DZ: '//STR )
                RETURN 
           END IF 
!    
           STATUS = DFTIFREEDESCRIPTOR( DESC_HANDLE )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1175, IUER, 'FFT_1D_C2R_C16', 'Error in '// &
     &              'DFTIFREEDESCRIPTOR: '//STR )
                RETURN 
           END IF 
!
           IF ( NUM_THR_OLD .NE. FFT_NUM_THR ) THEN
!
! ------------- Set the number of threads to the old value
!
                CALL OMP_SET_NUM_THREADS ( NUM_THR_OLD )
                IF ( FFT_DEBUG > 0 ) THEN
                     WRITE ( 6, * ) 'FFT_1D_C2R_C16: The number of threads '// &
     &                              'is set back to ', NUM_THR_OLD
                END IF
           END IF
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
#else
           CALL ERR_LOG ( 1176, IUER, 'FFT_1D_C2R_C16', 'Support of Intel MKL '// &
     &         'was not compiled in. Please, recompile your application '// &
     &         'with -D MKL option supplied to the compiler' )
           RETURN 
#endif
        ELSE 
!$OMP CRITICAL
          IF ( DFFTW_PLAN .NE. 0            .AND. &
     &         LAST_DIMS(1) == DIM          .AND. &
     &         LAST_DIMS(2) == 0            .AND. &
     &         LAST_OP      == '1D_C2R_C16'       ) THEN
!
! ------------ If plan for this type of transform and for these dimensions
! ------------ has alread been create -- use it!
!
               CONTINUE 
             ELSE 
               IF ( DFFTW_PLAN .NE. 0 ) THEN
!
! ----------------- Destroy previoysly created plan in order to prevent
! ----------------- memory leakage
!
                    CALL FFTW_DESTROY_PLAN ( %VAL(DFFTW_PLAN) )
               END IF
               CALL FFTW_SET_TIMELIMIT    ( %VAL(FFTW_PAR_TIMEOUT) )
!
! ------------ Make a new plan using wisdom
!
               CALL DFFTW_PLAN_DFT_C2R_1D ( DFFTW_PLAN, DIM, ARR_C16, ARR_R8, &
     &                                      FFT_MODE + FFTW_WISDOM_ONLY )
               IF ( DFFTW_PLAN == 0 ) THEN
                    IF ( FFT_DEBUG > 0 ) THEN
                         WRITE ( 6, * ) 'FFT_1D_C2R_C16: wisdom is '// &
     &                                  'unavailable, DIM = ', DIM, &
     &                                  ' FFT_MODE = ', FFT_MODE
                         IF ( FFT_DEBUG == 8 ) CALL EXIT ( 1 )
                    END IF
!
! ----------------- Fallback to estimate mode
!
                    CALL DFFTW_PLAN_DFT_C2R_1D ( DFFTW_PLAN, DIM, ARR_C16, &
     &                                           ARR_R8, FFTW_ESTIMATE )
               END IF
!
! ------------ Store the type of the transform and dimensions
!
               LAST_DIMS(1) = DIM
               LAST_DIMS(2) = 0
               LAST_OP = '1D_C2R_C16'
          END IF
!$OMP END CRITICAL
!
! ------- Execute the FFT transform
!
          CALL DFFTW_EXECUTE_DFT_C2R ( DFFTW_PLAN, ARR_C16, ARR_R8 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFT_1D_C2R_C16  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFT_1D_C2R_C8 ( DIM, ARR_C8, ARR_R4, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  FFT_1D_C2R_C8  performs backward (inverse) fast Fourier   *
! *   transform under the complex*8 array ARR_C8 of the 1-dimensional    *
! *   Hermtian-conjugated spectrum of dimension DIM. ARR_C8 keeps only   *
! *   dim/2+1 elements for non-negative frequencies. Result, the real*8  *
! *   array in time order, is put in the output array ARR_R8.            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    DIM ( INTEGER*4 ) -- Dimension of the array input. NB: dimension  *
! *                         that is the power of 2 yileds the best       *
! *                         performance.                                 *
! * ARR_C8 ( COMPLEX*8 ) -- Array to be transformed. A portion of the    *
! *                         Hermitian-conjugated spectrum for            *
! *                         non-negative frequencies. Dimension: DIM/2+1.*
! *                         Frequencies start from 0 and run through     *
! *                         dim/2.                                       *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * ARR_R4 ( REAL*4    ) -- Array to be transformed. Should be time      *
! *                         ordered.                                     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! * ### 04-AUG-2007   FFT_1D_C2R_C8  v2.0 (c) L. Petrov 23-JAN-2011 ###  *
! *                                                                      *
! ************************************************************************
#ifdef MKL
      USE        MKL_DFTI
      IMPLICIT   NONE
      TYPE     ( DFTI_DESCRIPTOR ), POINTER :: DESC_HANDLE
      INTEGER*4  NUM_THR_OLD, STATUS
      INTEGER*4, EXTERNAL :: OMP_GET_NUM_THREADS, OMP_GET_MAX_THREADS
      CHARACTER  STR*128
#else
      IMPLICIT   NONE
#endif
      INCLUDE    'fftw3.f'
      INTEGER*4   DIM, IEXP, IUER
      COMPLEX*8   ARR_C8(DIM/2+1)
      REAL*4      ARR_R4(DIM)
!
      INTEGER*4   FFT_MODE, FFT_NUM_THR, FFT_DEBUG, LAST_DIMS(2)
      INTEGER*8   SFFTW_PLAN, DFFTW_PLAN
      REAL*8      FFTW_PAR_TIMEOUT 
      CHARACTER   LAST_OP*10
!
      COMMON    / FFT_PARS / DFFTW_PLAN, SFFTW_PLAN, FFTW_PAR_TIMEOUT, &
     &                       FFT_MODE, FFT_NUM_THR, FFT_DEBUG, &
     &                       LAST_DIMS, LAST_OP
!
      IF ( FFT_MODE == -1 ) THEN
#ifdef MKL
!
! ======== This code is for the case when MKL is used
!
           STATUS = DFTICREATEDESCRIPTOR ( DESC_HANDLE, DFTI_SINGLE, &
     &                                     DFTI_REAL, 1, DIM )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1181, IUER, 'FFT_1D_C2R_C8', 'Error in '// &
     &              'DFTICREATEDESCRIPTOR: '//STR )
                RETURN 
           END IF 
           IF ( FFT_NUM_THR > -1 ) THEN
!
! ------------- Check the current maximum number of threads
!
                NUM_THR_OLD = OMP_GET_MAX_THREADS()
                IF ( FFT_DEBUG > 0 ) THEN
                     WRITE ( 6, * ) 'FFT_1D_C2R_C8: NUM_THR_OLD = ', &
     &                       NUM_THR_OLD, ' FFT_NUM_THR = ', FFT_NUM_THR
                END IF
                IF ( NUM_THR_OLD .NE. FFT_NUM_THR ) THEN
!
! ------------------ Temporarily reset the number of threads
!
                     CALL OMP_SET_NUM_THREADS ( MAX(1, FFT_NUM_THR)  )
                     IF ( FFT_DEBUG > 0 ) THEN
                          WRITE ( 6, * ) 'FFT_1D_C2R_C8: THe number of threads '// &
     &                                   'is reset to ', FFT_NUM_THR
                     END IF
                END IF
           END IF
!
           STATUS = DFTISETVALUE ( DESC_HANDLE, DFTI_PLACEMENT, DFTI_NOT_INPLACE )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1182, IUER, 'FFT_1D_C2R_C8', 'Error in '// &
     &              'DFTSETVALUE: '//STR )
                RETURN 
           END IF 
!
           STATUS = DFTICOMMITDESCRIPTOR ( DESC_HANDLE )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1183, IUER, 'FFT_1D_C2R_C8', 'Error in '// &
     &              'DFTICOMMITDESCRIPTOR: '//STR )
                RETURN 
           END IF 
!
           STATUS = DFTICOMPUTEBACKWARD ( DESC_HANDLE, ARR_C8, ARR_R4 )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1184, IUER, 'FFT_1D_C2R_C8', 'Error in '// &
     &              'DFTI_COMPUTE_FORWARD_DZ: '//STR )
                RETURN 
           END IF 
!    
           STATUS = DFTIFREEDESCRIPTOR( DESC_HANDLE )
           IF ( STATUS .NE. 0 ) THEN
                STR = DFTIERRORMESSAGE ( STATUS )
                CALL ERR_LOG ( 1185, IUER, 'FFT_1D_C2R_C8', 'Error in '// &
     &              'DFTIFREEDESCRIPTOR: '//STR )
                RETURN 
           END IF 
!
           IF ( NUM_THR_OLD .NE. FFT_NUM_THR ) THEN
!
! ------------- Set the number of threads to the old value
!
                CALL OMP_SET_NUM_THREADS ( NUM_THR_OLD )
                IF ( FFT_DEBUG > 0 ) THEN
                     WRITE ( 6, * ) 'FFT_1D_C2R_C8: The number of threads '// &
     &                              'is set back to ', NUM_THR_OLD
                END IF
           END IF
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
#else
           CALL ERR_LOG ( 1186, IUER, 'FFT_1D_C2R_C8', 'Support of Intel MKL '// &
     &         'was not compiled in. Please, recompile your application '// &
     &         'with -D MKL option supplied to the compiler' )
           RETURN 
#endif
        ELSE 
          IF ( SFFTW_PLAN .NE. 0            .AND. &
     &         LAST_DIMS(1) == DIM          .AND. &
     &         LAST_DIMS(2) == 0            .AND. &
     &         LAST_OP      == '1D_C2R_C8 '       ) THEN
!
! ------------ If plan for this type of transform and for these dimensions
! ------------ has alread been create -- use it!
!
               CONTINUE 
             ELSE 
               IF ( SFFTW_PLAN .NE. 0 ) THEN
!
! ----------------- Destroy previoysly created plan in order to prevent
! ----------------- memory leakage
!
                    CALL FFTW_DESTROY_PLAN ( %VAL(SFFTW_PLAN) )
               END IF
               CALL FFTW_SET_TIMELIMIT    ( %VAL(FFTW_PAR_TIMEOUT) )
!
! ------------ Make a new plan using wisdom
!
               CALL SFFTW_PLAN_DFT_C2R_1D ( SFFTW_PLAN, DIM, ARR_C8, ARR_R4, &
     &                                      FFT_MODE + FFTW_WISDOM_ONLY )
               IF ( SFFTW_PLAN == 0 ) THEN
                    IF ( FFT_DEBUG > 0 ) THEN
                         WRITE ( 6, * ) 'FFT_1D_C2R_C8: wisdom is '// &
     &                                  'unavailable, DIM = ', DIM, &
     &                                  ' FFT_MODE = ', FFT_MODE
                         IF ( FFT_DEBUG == 8 ) CALL EXIT ( 1 )
                    END IF
!
! ----------------- Fallback to estimate mode
!
                    CALL SFFTW_PLAN_DFT_C2R_1D ( SFFTW_PLAN, DIM, ARR_C8, ARR_R4, &
     &                                           FFTW_ESTIMATE )
               END IF
!
! ------------ Store the type of the transform and dimensions
!
               LAST_DIMS(1) = DIM
               LAST_DIMS(2) = 0
               LAST_OP = '1D_C2R_C8 '
          END IF
!
! ------- Execute the FFT transform
!
          CALL SFFTW_EXECUTE_DFT_C2R ( SFFTW_PLAN, ARR_C8, ARR_R4 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  FFT_1D_C2R_C8 !#!#
