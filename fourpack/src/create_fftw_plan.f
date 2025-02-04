      SUBROUTINE CREATE_FFTW_PLAN ( FFTW_METHOD, NUM_THR_STR, FL_TIMING, &
     &                              FILIN, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  CREATE_FFTW_PLAN  creates the so-called wisdom file --    *
! *   parameter for the tune-ip of the FFTW library that performs        *
! *   fast Fourier transform.                                            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * FFTW_METHOD ( INTEGER*4 ) -- Method for geteting the wisdom:         *
! *                              MEASURE                                 *
! *                              PATIENT ( recommended )                 *
! *                              EXHAUSTIVE                              *
! * NUM_THR_STR ( CHARACTER ) -- The number of threads. NB: single-thread*
! *                              wisdom are ignored for multi-thread     *
! *                              plans.                                  *
! *   FL_TIMING ( LOGICAL*1 ) -- Flag, whether to print time.            *
! *       FILIN ( CHARACTER ) -- Input file with the tuned procedure     *
! *                              and dimensions.                         *
! *                              Format: space separated:                *
! *                              procedure  f, fr2c, fc2r, d, dr2c, dc2r *
! *                              dim1       first dimension;             *
! *                              dim2       (optional) second dimension. *
! *      FILOUT ( CHARACTER ) -- Name of the output wisdome file.        *
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
! * ## 12-JUN-2009  CREATE_FFTW_PLAN   v2.2 (c) L. Petrov 19-FEB-2022 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fftw3.f'
      INTEGER*4  FFTW_METHOD, IUER
      LOGICAL*1  FL_TIMING
      CHARACTER  NUM_THR_STR*(*), FILIN*128, FILOUT*128
!
      INTEGER*4  MBUF, MIND, MAX_DIM 
      PARAMETER  ( MAX_DIM = 128*1024, MBUF = 32*1024, MIND = 32 )
      LOGICAL*1  LEX
      CHARACTER  BUF(MBUF)*128, WIS(MBUF)*128, REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      CHARACTER  STR*80
      INTEGER*4  MAX_THR
      PARAMETER  ( MAX_THR = 1024 )
      INTEGER*4  NBUF, IND(2,MIND), J1, IP, IL, NP, LIND, IRET, &
     &           NUM_THR, INIT_CODE, NWIS, IER
      ADDRESS__TYPE ADR_RE_4, ADR_RE_8, ADR_CM_8, ADR_CM_16, MEM_ADR, MEM_LEN
      ADDRESS__TYPE DIM1, DIM2, DIM3
      CHARACTER  WIS_VERSION_STR*11, FFTWF_VERSION_STR*11, FFTW_VERSION_STR*11
      REAL*8     FFTW_TIMEOUT
      PARAMETER  ( FFTW_TIMEOUT = 7*86400.0D0 )
      INTEGER*8  PLAN_FFTW(2)
      ADDRESS__TYPE, EXTERNAL :: FFTW_VERSION, FFTWF_VERSION
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Check whether the input file exists
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6131, IUER, 'CREATE_FFTW_PLAN', 'Cannot find '// &
     &                   'input request file '//FILIN )
           RETURN 
      END IF
!
! --- Read the input file with wisdom compilation requests
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILIN, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6132, IUER, 'CREATE_FFTW_PLAN', 'Error in '// &
     &         'reading input request file '//FILIN )
           RETURN 
      END IF
!
      INQUIRE ( FILE=FILOUT, EXIST=LEX ) 
      IF ( ILEN(NUM_THR_STR) > 0  .AND.  NUM_THR_STR .NE. '0' ) THEN
!
! -------- Get the number of threads
!
           CALL CHIN ( NUM_THR_STR, NUM_THR )
         ELSE
           NUM_THR = 0
      END IF
      IF ( LEX ) THEN
!
! -------- The file with plan exist. Read it and and check whether the version matches
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT ( FILOUT, MBUF, WIS, NWIS, IER ) 
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6133, IUER, 'CREATE_FFTW_PLAN', 'Error in '// &
     &              'an attempt to read the plan file '//FILOUT )
                RETURN 
           END IF
           IP = INDEX ( WIS(1), ' ' )
           WIS_VERSION_STR = WIS(1)(2:IP)
           IF ( INDEX ( WIS_VERSION_STR, 'fftwf' ) > 0 ) THEN
                CALL STRNCPY ( STR, %VAL(LOC(FFTWF_VERSION)) )
                IP = INDEX ( STR(6:), '-' ) + 4
                FFTWF_VERSION_STR = STR(1:IP) 
                IF ( FFTWF_VERSION_STR .NE. WIS_VERSION_STR ) THEN
                     LEX = .FALSE.
                     WRITE ( 6, '(A)' ) 'Remove an old wisdom file '//TRIM(FILOUT)
                     WRITE ( 6, '(A)' ) 'FFTWF version in the wisdom file: '//WIS_VERSION_STR
                     WRITE ( 6, '(A)' ) 'FFTWF library version:            '//FFTWF_VERSION_STR
                END IF
              ELSE
                CALL STRNCPY ( STR, %VAL(LOC(FFTW_VERSION)) )
                IP = INDEX ( STR(6:), '-' ) + 4
                FFTW_VERSION_STR = STR(1:IP) 
                IF ( FFTW_VERSION_STR .NE. WIS_VERSION_STR ) THEN
                     LEX = .FALSE.
                     WRITE ( 6, '(A)' ) 'Remove an old wisdom file '//TRIM(FILOUT)
                     WRITE ( 6, '(A)' ) 'FFTW version in the wisdom file: '//WIS_VERSION_STR
                     WRITE ( 6, '(A)' ) 'FFTW library version:            '//FFTW_VERSION_STR
                END IF
           END IF
      END IF
      IF ( LEX ) THEN
!
! -------- The file with plan exist. Read it and inilialize threads
!
           IF ( FFTW_METHOD == FFTW_MEASURE ) THEN
                INIT_CODE = 1
              ELSE IF ( FFTW_METHOD == FFTW_PATIENT ) THEN
                INIT_CODE = 2
              ELSE IF ( FFTW_METHOD == FFTW_EXHAUSTIVE ) THEN
                INIT_CODE = 3
              ELSE 
                INIT_CODE = 2
           END IF
           CALL ERR_PASS  ( IUER, IER )
           CALL INIT_FFTW ( FILOUT, INIT_CODE, FFTW_TIMEOUT, NUM_THR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6134, IUER, 'CREATE_FFTW_PLAN', 'Error in '// &
     &              'an attempt to read the plan file '// &
     &               FILOUT(1:I_LEN(FILOUT))//' and initialize FFTW' )
                RETURN 
           END IF 
        ELSE 
!
! -------- The file with plan does not exist
! -------- Check whether the threaded version is requested
!
           IF ( NUM_THR > 0 ) THEN
!
! ------------- Initilize threads
!
                CALL DFFTW_INIT_THREADS ( IRET )
                CALL SFFTW_INIT_THREADS ( IRET )
                IF ( IRET .NE. 1 ) THEN
                     CALL ERR_LOG ( 6135, IUER, 'CREATE_FFTW_PLAN', &
     &                   'Failure in DFFTW_INIT_THREADS' )
                     RETURN 
                END IF
!
! ------------- Set the number of threads
!
                IF ( NUM_THR > 1  .AND.  NUM_THR .LE. MAX_THR ) THEN
                     CALL DFFTW_PLAN_WITH_NTHREADS ( NUM_THR )
                     CALL SFFTW_PLAN_WITH_NTHREADS ( NUM_THR )
                     WRITE ( 6, * ) NUM_THR, ' threads are used'
                  ELSE 
                     CALL DFFTW_PLAN_WITH_NTHREADS ( 1 )
                     CALL SFFTW_PLAN_WITH_NTHREADS ( 1 )
                     WRITE ( 6, * ) 'A single-thread wisdom is being created'
                END IF
             ELSE 
               WRITE ( 6, * ) 'A nonthreaded wisdom is being created'
           END IF
      END IF
!
      DO 410 J1=1,NBUF
         IF ( ILEN(BUF(J1))     ==  0  ) GOTO 410
         IF (      BUF(J1)(1:1) == '#' ) GOTO 410
!
! ------ Parse the line with the request
!
         IUER = -1
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, IUER )
         IF ( LIND < 2 ) GOTO 410
         READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I8)' ) DIM1 
         IF ( LIND .GE. 3 ) THEN
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(I8)' ) DIM2
         END IF
         IF ( LIND == 2  .AND. ( BUF(J1)(IND(1,1):IND(2,1)) == 'f' .OR. &
     &                           BUF(J1)(IND(1,1):IND(2,1)) == 'b'    ) ) THEN
!
! ----------- 1D REAL*4 complex Fourier transform
!
              IF ( .NOT. FL_TIMING ) THEN
                    WRITE ( 6, 110 ) J1, NBUF, 'f', DIM1
                    CALL FLUSH ( 6 ) 
              END IF
 110          FORMAT ( '   O! Wisdom is being acquired ...  ', I6, &
     &                 ' ( ',I6,')  ',A,'  dim1: ', I10 )
              DIM2 = 1
              DIM3 = 1
              IUER = -1
              CALL GRAB_MEM ( IUER, MEM_LEN, MEM_ADR, 1, &
     &                        8*DIM1, ADR_CM_8 )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH  ( STR )
#ifdef ADR_32BIT                   
                   CALL IINCH  ( MEM_LEN, STR )
#else
                   CALL IINCH8 ( MEM_LEN, STR )
#endif
                   CALL ERR_LOG ( 6136, IUER, 'CREATE_FFTW_PLAN', &
     &                 'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of memory' )
                   RETURN 
              END IF
              CALL NOUT_R4 ( 2*DIM1, %VAL(ADR_CM_8) )
!
              CALL SFFTW_PLAN_DFT_1D ( PLAN_FFTW(1), DIM1, &
     &                                 %VAL(ADR_CM_8), %VAL(ADR_CM_8), &
     &                                 FFTW_FORWARD, FFTW_METHOD )
              CALL SFFTW_PLAN_DFT_1D ( PLAN_FFTW(1), DIM1, &
     &                                 %VAL(ADR_CM_8), %VAL(ADR_CM_8), &
     &                                 FFTW_BACKWARD, FFTW_METHOD )
              IF ( FL_TIMING ) THEN
                   CALL CPU_TIMER ( %VAl(0) )
                   CALL SFFTW_EXECUTE_DFT ( PLAN_FFTW(1), %VAL(ADR_CM_8), &
     &                                      %VAL(ADR_CM_8)  )
                   CALL CPU_TIMER ( STR )
                   WRITE  ( 6, 120 ) J1, NBUF, 'f', DIM1, STR(3:27)
 120               FORMAT ( 2X, I4,' ( ', I4, ' ) Type: ', A4, &
     &                      ' Dim: ', I9, ' Time: ', A ) 
!
                 ELSE 
                   CALL ERR_PASS ( IUER, IER ) 
                   CALL WRITE_WISDOM ( FILOUT, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6137, IUER, 'CREATE_FFTW_PLAN', &
     &                      'Failure to write wisdom into the output file '// &
     &                       FILOUT )
                        RETURN 
                   END IF
              END IF
              CALL FREE ( MEM_ADR )
            ELSE IF ( LIND == 3  .AND. BUF(J1)(IND(1,1):IND(2,1)) == 'f' ) THEN
!
! ----------- 2D REAL*4 complex Fourier transform
!
              DIM3 = 1
              IF ( .NOT. FL_TIMING ) THEN
                   WRITE ( 6, 130 ) J1, NBUF, 'f', DIM1, DIM2
 130               FORMAT ( ' O! Wisdom is being acquired ...  ', I6, &
     &                      ' ( ',I6,') ',A,'  dim1: ', I7, '  dim2: ',I7  )
                   CALL FLUSH ( 6 ) 
              END IF
!
              CALL GRAB_MEM ( IUER, MEM_LEN, MEM_ADR, 1, &
     &                        8*DIM1*DIM2, ADR_CM_8 )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH  ( STR )
#ifdef ADR_32BIT                   
                   CALL IINCH  ( MEM_LEN, STR )
#else
                   CALL IINCH8 ( MEM_LEN, STR )
#endif
                   CALL ERR_LOG ( 6138, IUER, 'CREATE_FFTW_PLAN', &
     &                 'Failure to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &                 'of memory' )
                   RETURN 
              END IF
!
              CALL NOUT_R4 ( 2*DIM1*DIM2, %VAL(ADR_CM_8) )
              CALL SFFTW_PLAN_DFT_2D ( PLAN_FFTW(1), DIM1, DIM2, &
     &                                 %VAL(ADR_CM_8), %VAL(ADR_CM_8), &
     &                                 FFTW_FORWARD, FFTW_METHOD )
              CALL SFFTW_PLAN_DFT_2D ( PLAN_FFTW(1), DIM1, DIM2, &
     &                                 %VAL(ADR_CM_8), %VAL(ADR_CM_8), &
     &                                 FFTW_BACKWARD, FFTW_METHOD )
!
              IF ( FL_TIMING ) THEN
                   CALL CPU_TIMER ( %VAl(0) )
                   CALL SFFTW_EXECUTE_DFT ( PLAN_FFTW(1), %VAL(ADR_CM_8), &
     &                                      %VAL(ADR_CM_8)  )
                   CALL CPU_TIMER ( STR )
                   WRITE  ( 6, 140 ) J1, NBUF, 'f', DIM1, DIM2, STR(3:27)
 140               FORMAT ( 2X, I4,' ( ', I4, ' ) Type: ', A4, &
     &                      ' Dim: ', I7, 1X, I7, ' Time: ', A ) 
                 ELSE 
                  CALL ERR_PASS ( IUER, IER ) 
                  CALL WRITE_WISDOM ( FILOUT, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 6139, IUER, 'CREATE_FFTW_PLAN', &
         &                 'Failure to write wisdom into the output file '// &
         &                  FILOUT )
                       RETURN 
                  END IF
              END IF
              CALL FREE ( MEM_ADR )
            ELSE IF ( LIND == 2  .AND. BUF(J1)(IND(1,1):IND(2,1)) == 'fr2c' ) THEN
!
! ----------- 1D REAL*4 to complex forward Fourier transform
!
              IF ( .NOT. FL_TIMING ) THEN
                   WRITE ( 6, 110 ) J1, NBUF, 'fr2c', DIM1
                   CALL FLUSH ( 6 ) 
              END IF
              DIM2 = 1
              DIM3 = 1
              CALL GRAB_MEM ( IUER, MEM_LEN, MEM_ADR, 2, &
     &                        4*DIM1,        ADR_RE_4,   &
     &                        8*(DIM1/2+1),  ADR_CM_8 )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH  ( STR )
#ifdef ADR_32BIT                   
                   CALL IINCH  ( MEM_LEN, STR )
#else
                   CALL IINCH8 ( MEM_LEN, STR )
#endif
                   CALL ERR_LOG ( 6140, IUER, 'CREATE_FFTW_PLAN', &
     &                 'Failure to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &                 'of memory' )
                   RETURN 
              END IF
!
              CALL NOUT_R4 (    DIM1,      %VAL(ADR_RE_4) )
              CALL NOUT_R4 ( 2*(DIM1/2+1), %VAL(ADR_CM_8) )
!
              CALL SFFTW_PLAN_DFT_R2C_1D ( PLAN_FFTW(1), DIM1, &
     &                                     %VAL(ADR_RE_4), %VAL(ADR_CM_8), &
     &                                     FFTW_METHOD )
!
              IF ( FL_TIMING ) THEN
                   CALL CPU_TIMER ( %VAl(0) )
                   CALL SFFTW_EXECUTE_DFT_R2C ( PLAN_FFTW(1), %VAL(ADR_RE_4), &
     &                                           %VAL(ADR_CM_8)  )
                   CALL CPU_TIMER ( STR )
                   WRITE  ( 6, 120 ) J1, NBUF, 'fr2c', DIM1, STR(3:27)
                 ELSE 
                  CALL ERR_PASS ( IUER, IER ) 
                  CALL WRITE_WISDOM ( FILOUT, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 6141, IUER, 'CREATE_FFTW_PLAN', &
         &                 'Failure to write wisdom into the output file '// &
         &                  FILOUT )
                       RETURN 
                  END IF
              END IF
              CALL FREE ( MEM_ADR )
           ELSE IF ( LIND == 2  .AND. BUF(J1)(IND(1,1):IND(2,1)) == 'fc2r' ) THEN
!
! ----------- 1D COMPLEX*8 to REAL*4 backward Fourier transform
!
              IF ( .NOT. FL_TIMING ) THEN
                   WRITE ( 6, 110 ) J1, NBUF, 'fc2r', DIM1
                   CALL FLUSH ( 6 ) 
              END IF
              DIM2 = 1
              DIM3 = 1
              CALL GRAB_MEM ( IUER, MEM_LEN, MEM_ADR, 2, &
     &                        4*DIM1,       ADR_RE_4,    &
     &                        8*(DIM1/2+1), ADR_CM_8 )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH  ( STR )
#ifdef ADR_32BIT                   
                   CALL IINCH  ( MEM_LEN, STR )
#else
                   CALL IINCH8 ( MEM_LEN, STR )
#endif
                   CALL ERR_LOG ( 6142, IUER, 'CREATE_FFTW_PLAN', &
     &                 'Failure to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &                 'of memory' )
                   RETURN 
              END IF
!
              CALL NOUT_R4 (    DIM1,      %VAL(ADR_RE_4) )
              CALL NOUT_R4 ( 2*(DIM1/2+1), %VAL(ADR_CM_8) )
!
              CALL SFFTW_PLAN_DFT_C2R_1D ( PLAN_FFTW(1), DIM1, &
     &                                     %VAL(ADR_CM_8), %VAL(ADR_RE_4), &
     &                                     FFTW_METHOD )
!
              IF ( FL_TIMING ) THEN
                   CALL CPU_TIMER ( %VAl(0) )
                   CALL SFFTW_EXECUTE_DFT_C2R ( PLAN_FFTW(1), %VAL(ADR_CM_8), &
     &                                          %VAL(ADR_RE_4) )
                   CALL CPU_TIMER ( STR )
                   WRITE  ( 6, 120 ) J1, NBUF, 'fc2r', DIM1, STR(3:27)
                ELSE 
                  CALL ERR_PASS ( IUER, IER ) 
                  CALL WRITE_WISDOM ( FILOUT, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 6143, IUER, 'CREATE_FFTW_PLAN', &
     &                 'Failure to write wisdom into the output file '// &
     &                  FILOUT )
                       RETURN 
                  END IF
              END IF
              CALL FREE ( MEM_ADR )
           ELSE IF ( LIND == 2  .AND. BUF(J1)(IND(1,1):IND(2,1)) == 'd' ) THEN
!
! ----------- 1D REAL*8 complex Fourier transform
!
              IF ( .NOT. FL_TIMING ) THEN
                   WRITE ( 6, 110 ) J1, NBUF, 'd', DIM1
                   CALL FLUSH ( 6 ) 
              END IF
              DIM2 = 1
              DIM3 = 1
!
              IUER = -1
              CALL GRAB_MEM ( IUER, MEM_LEN, MEM_ADR, 1,   &
     &                        16*DIM1, ADR_CM_16 )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH  ( STR )
#ifdef ADR_32BIT                   
                   CALL IINCH  ( MEM_LEN, STR )
#else
                   CALL IINCH8 ( MEM_LEN, STR )
#endif
                   CALL ERR_LOG ( 6144, IUER, 'CREATE_FFTW_PLAN', &
     &                 'Failure to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &                 'of memory' )
                   RETURN 
              END IF
!
              CALL NOUT_R8 ( 2*DIM1, %VAL(ADR_CM_16) )
!
              CALL DFFTW_PLAN_DFT_1D ( PLAN_FFTW(2), DIM1, &
     &                                 %VAL(ADR_CM_16), %VAL(ADR_CM_16), &
     &                                 FFTW_FORWARD, FFTW_METHOD )
              CALL DFFTW_PLAN_DFT_1D ( PLAN_FFTW(2), DIM1, &
     &                                 %VAL(ADR_CM_16), %VAL(ADR_CM_16), &
     &                                 FFTW_BACKWARD, FFTW_METHOD )
              CALL FREE ( MEM_ADR )
!
              IF ( FL_TIMING ) THEN
                   CALL CPU_TIMER ( %VAl(0) )
                   CALL DFFTW_EXECUTE_DFT ( PLAN_FFTW(2), %VAL(ADR_CM_16), &
     &                                      %VAL(ADR_CM_16)  )
                   CALL CPU_TIMER ( STR )
                   WRITE  ( 6, 120 ) J1, NBUF, 'd   ', DIM1, STR(3:27)
                 ELSE 
                   CALL ERR_PASS ( IUER, IER ) 
                   CALL WRITE_WISDOM ( FILOUT, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6145, IUER, 'CREATE_FFTW_PLAN', &
     &                      'Failure to write wisdom into the output file '// &
     &                       FILOUT )
                        RETURN 
                  END IF
              END IF
            ELSE IF ( LIND == 3  .AND. BUF(J1)(IND(1,1):IND(2,1)) == 'd' ) THEN
!
! ----------- 2D REAL*8 complex Fourier transform
!
              IF ( .NOT. FL_TIMING ) THEN
                    WRITE ( 6, 130 ) J1, NBUF, 'd', DIM1, DIM2
                    CALL FLUSH ( 6 ) 
              END IF
              DIM3 = 1
!
              IUER = -1
              CALL GRAB_MEM ( IUER, MEM_LEN, MEM_ADR, 1,   &
     &                        16*DIM1*DIM2, ADR_CM_16 )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH  ( STR )
#ifdef ADR_32BIT                   
                   CALL IINCH  ( MEM_LEN, STR )
#else
                   CALL IINCH8 ( MEM_LEN, STR )
#endif
                   CALL ERR_LOG ( 6146, IUER, 'CREATE_FFTW_PLAN', &
     &                 'Failure to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &                 'of memory' )
                   RETURN 
              END IF
!
              CALL NOUT_R8 ( 2*DIM1*DIM2, %VAL(ADR_CM_16) )
!
              CALL DFFTW_PLAN_DFT_2D ( PLAN_FFTW(2), DIM1, DIM2, &
     &                                 %VAL(ADR_CM_16), %VAL(ADR_CM_16), &
     &                                 FFTW_FORWARD, FFTW_METHOD )
              CALL DFFTW_PLAN_DFT_2D ( PLAN_FFTW(2), DIM1, DIM2, &
     &                                 %VAL(ADR_CM_16), %VAL(ADR_CM_16), &
     &                                 FFTW_BACKWARD, FFTW_METHOD )
!
              IF ( FL_TIMING ) THEN
                   CALL CPU_TIMER ( %VAl(0) )
                   CALL DFFTW_EXECUTE_DFT ( PLAN_FFTW(2), %VAL(ADR_CM_16), &
     &                                      %VAL(ADR_CM_16)  )
                   CALL CPU_TIMER ( STR )
                   WRITE  ( 6, 140 ) J1, NBUF, 'd', DIM1, DIM2, STR(3:27)
                 ELSE 
                   CALL ERR_PASS ( IUER, IER ) 
                   CALL WRITE_WISDOM ( FILOUT, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6147, IUER, 'CREATE_FFTW_PLAN', &
     &                      'Failure to write wisdom into the output file '// &
     &                       FILOUT )
                        RETURN 
                   END IF
              END IF
              CALL FREE ( MEM_ADR )
           ELSE IF ( LIND == 2  .AND. BUF(J1)(IND(1,1):IND(2,1)) == 'dr2c' ) THEN
!
! ----------- 1D REAL*8 to complex*16 forwards Fourier transform
!
              IF ( .NOT. FL_TIMING ) THEN
                   WRITE ( 6, 110 ) J1, NBUF, 'dr2c', DIM1
                   CALL FLUSH ( 6 ) 
              END IF
!
              DIM2 = 1
              DIM3 = 1
!
              IUER = -1
              CALL GRAB_MEM ( IUER, MEM_LEN, MEM_ADR, 2, &
     &                        8*DIM1,        ADR_RE_8,   &
     &                        16*(DIM1/2+1), ADR_CM_16   )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH  ( STR )
#ifdef ADR_32BIT                   
                   CALL IINCH  ( MEM_LEN, STR )
#else
                   CALL IINCH8 ( MEM_LEN, STR )
#endif
                   CALL ERR_LOG ( 6148, IUER, 'CREATE_FFTW_PLAN', &
     &                 'Failure to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &                 'of memory' )
                   RETURN 
              END IF
!
              CALL NOUT_R8 (    DIM1,      %VAL(ADR_RE_8)  )
              CALL NOUT_R8 ( 2*(DIM1/2+1), %VAL(ADR_CM_16) )
!
              CALL DFFTW_PLAN_DFT_R2C_1D ( PLAN_FFTW(2), DIM1, &
     &                                     %VAL(ADR_RE_8), %VAL(ADR_CM_16), &
     &                                     FFTW_METHOD )
              CALL FREE ( MEM_ADR )
!
!
              IF ( FL_TIMING ) THEN
                   CALL CPU_TIMER ( %VAl(0) )
                   CALL DFFTW_EXECUTE_DFT_R2C ( PLAN_FFTW(2),   &
     &                                          %VAL(ADR_RE_8), &
     &                                          %VAL(ADR_CM_16) )
                   CALL CPU_TIMER ( STR )
                   WRITE  ( 6, 120 ) J1, NBUF, 'dr2c', DIM1, STR(3:27)
                 ELSE 
                   CALL ERR_PASS ( IUER, IER ) 
                   CALL WRITE_WISDOM ( FILOUT, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6149, IUER, 'CREATE_FFTW_PLAN', &
     &                      'Failure to write wisdom into the output file '// &
     &                       FILOUT )
                        RETURN 
                   END IF
              END IF
           ELSE IF ( LIND == 2  .AND. BUF(J1)(IND(1,1):IND(2,1)) == 'dc2r' ) THEN
!
! ----------- 1D complex*16 to REAL*8 backward Fourier transform
!
              IF ( .NOT. FL_TIMING ) THEN
                   WRITE ( 6, 110 ) J1, NBUF, 'dc2r', DIM1
                   CALL FLUSH ( 6 ) 
              END IF
              DIM2 = 1
              DIM3 = 1
!
              IUER = -1
              CALL GRAB_MEM ( IUER, MEM_LEN, MEM_ADR, 2, &
     &                        8*DIM1,        ADR_RE_8,   &
     &                        16*(DIM1/2+1), ADR_CM_16   )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH  ( STR )
#ifdef ADR_32BIT                   
                   CALL IINCH  ( MEM_LEN, STR )
#else
                   CALL IINCH8 ( MEM_LEN, STR )
#endif
                   CALL ERR_LOG ( 6150, IUER, 'CREATE_FFTW_PLAN', &
     &                 'Failure to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &                 'of memory' )
                   RETURN 
              END IF
!
              CALL NOUT_R8 (    DIM1,      %VAL(ADR_RE_8)  )
              CALL NOUT_R8 ( 2*(DIM1/2+1), %VAL(ADR_CM_16) )
!
              CALL DFFTW_PLAN_DFT_C2R_1D ( PLAN_FFTW(2), DIM1, &
     &                                     %VAL(ADR_CM_16), %VAL(ADR_RE_8), &
     &                                     FFTW_METHOD )
!
              IF ( FL_TIMING ) THEN
                   CALL CPU_TIMER ( %VAl(0) )
                   CALL DFFTW_EXECUTE_DFT_C2R ( PLAN_FFTW(2),    &
     &                                          %VAL(ADR_CM_16), &
     &                                          %VAL(ADR_RE_8)   )
                   CALL CPU_TIMER ( STR )
                   WRITE  ( 6, 120 ) J1, NBUF, 'dc2r', DIM1, STR(3:27)
                 ELSE 
                   CALL ERR_PASS ( IUER, IER ) 
                   CALL WRITE_WISDOM ( FILOUT, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6151, IUER, 'CREATE_FFTW_PLAN', &
     &                      'Failure to write wisdom into the output file '// &
     &                       FILOUT )
                        RETURN 
                   END IF
              END IF
              CALL FREE ( MEM_ADR )
         END IF
 410  CONTINUE 
      WRITE ( 6, '(A)' ) ' '
      WRITE ( 6, '(A)' ) 'Output file: '//FILOUT(1:I_LEN(FILOUT))
!
      CALL ERR_LOG ( 0,  IUER )
      RETURN
      END   SUBROUTINE  CREATE_FFTW_PLAN  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WRITE_WISDOM ( FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  WRITE_WISDOM 
! *                                                                      *
! *  ### 12-JUN-2009  WRITE_WISDOM  v1.2 (c)  L. Petrov  20-FEB-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  FILOUT*(*)
      CHARACTER  STR*128
      INTEGER*4  IL, PID, IS, IUER
      ADDRESS__TYPE :: FSTR
      CHARACTER  FILOUT_TMP*128, PID_STR*8
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GETPID, RENAME
      ADDRESS__TYPE, EXTERNAL :: FOPEN, FCLOSE
!
      PID = GETPID()
      CALL INCH ( PID, PID_STR )
      CALL CHASHR (    PID_STR )
      CALL BLANK_TO_ZERO ( PID_STR )
      FILOUT_TMP = FILOUT(1:I_LEN(FILOUT))//'__'//PID_STR
!
! ---- Open the output file with wisdom
!
#ifdef SUN
      FSTR = FOPEN ( %VAL(LOC(FILOUT_TMP(1:I_LEN(FILOUT_TMP))//CHAR(0))), &
     &               'w'//CHAR(0) )
#else
      FSTR = FOPEN ( %REF(FILOUT_TMP(1:I_LEN(FILOUT_TMP))//CHAR(0)), &
     &               'w'//CHAR(0) )
#endif
      IF ( FSTR .LT. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6161, IUER, 'WRITE_WISDOM', 'Error during '// &
     &              'opening file '//FILOUT_TMP(1:I_LEN(FILOUT_TMP))// &
     &              '":  '//STR )
           RETURN 
      END IF
!
! --- Write wisdom into the file
!
      CALL FFTW_EXPORT_WISDOM_TO_FILE  ( %VAL(FSTR) )
      CALL FFTWF_EXPORT_WISDOM_TO_FILE ( %VAL(FSTR) )
!
! --- Close wisdom file
!
      IL = FCLOSE ( %VAL(FSTR) )
      IF ( IL .LT. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6162, IUER, 'WRITE_WISDOM', 'Error during '// &
     &              'closing file '//FILOUT_TMP(1:I_LEN(FILOUT_TMP))// &
     &              '":  '//STR )
           RETURN 
      END IF
!
#ifdef SUN
      IS = RENAME ( %VAL(LOC(FILOUT_TMP(1:I_LEN(FILOUT_TMP))//CHAR(0))), &
     &              %VAL(LOC(FILOUT(1:I_LEN(FILOUT))//CHAR(0))) )
#else
#ifdef GNU
      IS = RENAME ( %REF(FILOUT_TMP(1:I_LEN(FILOUT_TMP))//CHAR(0)), &
     &              %REF(FILOUT(1:I_LEN(FILOUT))//CHAR(0)) )
#else
      IS = RENAME ( FILOUT_TMP(1:I_LEN(FILOUT_TMP))//CHAR(0), &
     &              FILOUT(1:I_LEN(FILOUT))//CHAR(0) )
#endif
#endif
      IF ( IS .LT. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6163, IUER, 'WRITE_WISDOM', 'Error during '// &
     &              'renaming '//STR(1:I_LEN(STR))//' file '// &
     &               FILOUT_TMP(1:I_LEN(FILOUT_TMP))//' into '//FILOUT )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  WRITE_WISDOM  !#!  
