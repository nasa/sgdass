      SUBROUTINE SPHE_AJ ( FSH, DIM, NORM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPHE_AJ  precomputes coefficients A_j for the Driscoll    *
! *   and Healy (1994) spherical harmonics quadrature.                   *
! *                                                                      *
! *   Output is written in FSH%AJ                                        *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  FSH ( SPHE_TYPE ) -- Internal data structure that keeps internal    *
! *                       arrays with intermediate results and their     *
! *                       status for possible re-use.                    *
! *  DIM ( INTEGER*4 ) -- Dimension
! * NORM ( INTEGER*4 ) -- Normalization to be used when calculating      *
! *                       Legendre functions                             *
! *                       1 -- "geodesy";                                *
! *                       2 -- Schmidt;                                  *
! *                       3 -- unnormalized;                             *
! *                       4 -- orthonormalized;                          *
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
! *   Copyright (c) 2006-2011, Mark A. Wieczorek                         *
! *   All rights reserved.                                               *
! *                                                                      *
! * ### 21-AUG-2012   SPHE_AJ v1.0 modified by L. Petrov 22-AUG-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      TYPE     ( SPHE_TYPE ) :: FSH
      INTEGER*4  DIM, NORM, IUER
      INTEGER*4  J1, J2, NUM_THR_SAVED, NTHR, IER
      REAL*8     COEF, RAT, SUM1
      CHARACTER  STR*128, STR1*128
      LOGICAL*1  FL_ERROR 
      LOGICAL*4, EXTERNAL :: PROBE_READ_ADDRESS, OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
!
      IF ( .NOT. PROBE_READ_ADDRESS(FSH) ) THEN
           CALL ERR_LOG ( 6311, IUER, 'SPHE_AJ', 'Adddress for '// &
     &         'data structure FSH is not readable' )
           RETURN 
      END IF
      IF ( FSH%STATUS .NE. FSH__INIT .AND. FSH%STATUS .NE. FSH__ALLO ) THEN
           CALL ERR_LOG ( 6312, IUER, 'SPHE_AJ', 'Data structure '// &
     &         'FSH has not been initialized' )
           RETURN 
      END IF
      IF ( DIM < 1 .OR. DIM > 2*(FSH__MAX_DEG+1) ) THEN
           CALL CLRCH ( STR ) 
           CALL CLRCH ( STR1 )
           CALL INCH  ( DIM, STR )
           CALL INCH  ( 2*FSH__MAX_DEG, STR1 )
           CALL ERR_LOG ( 6313, IUER, 'SPHE_AJ', 'Wrong value '// &
     &         'of DIM: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, '//STR1(1:I_LEN(STR1))//'] was expected' )
           RETURN 
      END IF
!
      IF ( FSH%AJ_STATUS == FSH__COMP .AND. &
     &     FSH%AJ_DIM    == DIM       .AND. &
     &     FSH%AJ_NORM   == NORM            ) THEN
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
         ELSE 
           FL_ERROR = .FALSE.
!$OMP      CRITICAL
           IF ( ASSOCIATED ( FSH%AJ ) ) THEN
                DEALLOCATE ( FSH%AJ )
           END IF
           ALLOCATE ( FSH%AJ(DIM), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR  )
                CALL IINCH ( 8*DIM**2, STR )
                CALL ERR_LOG ( 6314, IUER, 'SPHE_AJ', 'Failure in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dymanic memory for array FSH%AJ' )
                FL_ERROR = .TRUE.
                GOTO 820
           END IF
           FSH%AJ= 0.0D0
!
           FSH%AJ_STATUS = FSH__ALLO
           FSH%AJ_DIM  = DIM
           FSH%AJ_NORM = NORM
 820       CONTINUE 
!$OMP      END CRITICAL
           IF ( FL_ERROR ) RETURN 
      END IF
      IF ( DIM > 128 .AND. .NOT. OMP_IN_PARALLEL() ) THEN
           NUM_THR_SAVED = OMP_GET_NUM_THREADS()
           CALL OMP_SET_NUM_THREADS ( %VAL(FSH%NUM_THR) )
           NTHR = FSH%NUM_THR
         ELSE 
           NTHR = 1
      END IF
!
      COEF = DSQRT(8.0D0)/ DBLE(DIM)* DSQRT(4.0D0*PI)
      FSH%AJ = 0.0D0
!$OMP PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&   PRIVATE ( J1, J2, SUM1, RAT ), &
!$OMP&             SCHEDULE ( STATIC )
      DO 410 J1=0, DIM-1
	 SUM1 = 0.0D0
         RAT = DBLE(J1)/ DBLE(DIM) 
         DO 420 J2=0,DIM/2-1
	    SUM1 = SUM1 + DSIN( DBLE(2*J2+1)* PI* RAT )/DBLE(2*J2+1)
 420     CONTINUE 
         FSH%AJ(J1+1) = SUM1* DSIN(PI*RAT)* COEF
 410  CONTINUE 
!$OMP END PARALLEL DO
      IF ( DIM > 128 .AND. .NOT. OMP_IN_PARALLEL() ) THEN
           CALL OMP_SET_NUM_THREADS ( %VAL(NUM_THR_SAVED) )
      END IF
!
      FSH%AJ_STATUS = FSH__COMP
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPHE_AJ  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPHE_AJ_X ( FSH, DIM, NORM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPHE_AJ  precomputes coefficients A_j for the Driscoll    *
! *   and Healy (1994) spherical harmonics quadrature.                   *
! *                                                                      *
! *   Output is written in FSH%AJ                                        *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  FSH ( SPHE_TYPE ) -- Internal data structure that keeps internal    *
! *                       arrays with intermediate results and their     *
! *                       status for possible re-use.                    *
! *  DIM ( INTEGER*4 ) -- Dimension
! * NORM ( INTEGER*4 ) -- Normalization to be used when calculating      *
! *                       Legendre functions                             *
! *                       1 -- "geodesy";                                *
! *                       2 -- Schmidt;                                  *
! *                       3 -- unnormalized;                             *
! *                       4 -- orthonormalized;                          *
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
! *   Copyright (c) 2006-2011, Mark A. Wieczorek                         *
! *   All rights reserved.                                               *
! *                                                                      *
! * ### 21-AUG-2012   SPHE_AJ v1.0 modified by L. Petrov 22-AUG-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
!
!#      INCLUDE   'aj_000004.i'
!#      INCLUDE   'aj_000008.i'
!#      INCLUDE   'aj_000016.i'
!#      INCLUDE   'aj_000032.i'
!#      INCLUDE   'aj_000064.i'
!#      INCLUDE   'aj_000128.i'
!#      INCLUDE   'aj_000180.i'
!#      INCLUDE   'aj_000256.i'
!#      INCLUDE   'aj_000360.i'
!#      INCLUDE   'aj_000512.i'
!#      INCLUDE   'aj_000720.i'
!#      INCLUDE   'aj_001024.i'
!#      INCLUDE   'aj_001440.i'
!#      INCLUDE   'aj_001800.i'
!#      INCLUDE   'aj_002048.i'
!#      INCLUDE   'aj_002880.i'
!#      INCLUDE   'aj_003600.i'
!#      INCLUDE   'aj_004096.i'
!#      INCLUDE   'aj_005000.i'
!#      INCLUDE   'aj_005760.i'
!#      INCLUDE   'aj_006144.i'
!#      INCLUDE   'aj_007200.i'
!#      INCLUDE   'aj_008192.i'
!#      INCLUDE   'aj_010000.i'
!#      INCLUDE   'aj_010240.i'
!#      INCLUDE   'aj_011520.i'
!#      INCLUDE   'aj_012288.i'
!#      INCLUDE   'aj_014336.i'
!#      INCLUDE   'aj_014400.i'
!#      INCLUDE   'aj_016384.i'
!#      INCLUDE   'aj_018432.i'
!#      INCLUDE   'aj_020000.i'
!#      INCLUDE   'aj_020480.i'
!#      INCLUDE   'aj_022528.i'
!#      INCLUDE   'aj_023040.i'
!#      INCLUDE   'aj_024576.i'
!#      INCLUDE   'aj_028800.i'
!#      INCLUDE   'aj_032768.i'
!#      INCLUDE   'aj_040000.i'
!#      INCLUDE   'aj_046080.i'
!#      INCLUDE   'aj_057600.i'
!#      INCLUDE   'aj_060000.i'
!#      INCLUDE   'aj_065536.i'
!#      INCLUDE   'aj_080000.i'
!#      INCLUDE   'aj_092160.i'
!#      INCLUDE   'aj_100000.i'
!#      INCLUDE   'aj_115200.i'
!#      INCLUDE   'aj_120000.i'
!#      INCLUDE   'aj_131072.i'
!
      TYPE     ( SPHE_TYPE ) :: FSH
      INTEGER*4  DIM, NORM, IUER
      INTEGER*4  J1, J2, NUM_THR_SAVED, NTHR, IER
      REAL*8     COEF, RAT, SUM1
      CHARACTER  STR*128, STR1*128
      LOGICAL*1  FL_ERROR 
      LOGICAL*4, EXTERNAL :: PROBE_READ_ADDRESS, OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
!
      IF ( .NOT. PROBE_READ_ADDRESS(FSH) ) THEN
           CALL ERR_LOG ( 6321, IUER, 'SPHE_AJ_X', 'Adddress for '// &
     &         'data structure FSH is not readable' )
           RETURN 
      END IF
      IF ( FSH%STATUS .NE. FSH__INIT .AND. FSH%STATUS .NE. FSH__ALLO ) THEN
           CALL ERR_LOG ( 6322, IUER, 'SPHE_AJ_X', 'Data structure '// &
     &         'FSH has not been initialized' )
           RETURN 
      END IF
      IF ( DIM < 1 .OR. DIM > 2*(FSH__MAX_DEG+1) ) THEN
           CALL CLRCH ( STR ) 
           CALL CLRCH ( STR1 )
           CALL INCH  ( DIM, STR )
           CALL INCH  ( 2*FSH__MAX_DEG, STR1 )
           CALL ERR_LOG ( 6323, IUER, 'SPHE_AJ_X', 'Wrong value '// &
     &         'of DIM: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, '//STR1(1:I_LEN(STR1))//'] was expected' )
           RETURN 
      END IF
!
      IF ( FSH%AJ_STATUS == FSH__COMP .AND. &
     &     FSH%AJ_DIM    == DIM       .AND. &
     &     FSH%AJ_NORM   == NORM            ) THEN
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
         ELSE 
           FL_ERROR = .FALSE.
!$OMP      CRITICAL
           IF ( ASSOCIATED ( FSH%AJ ) ) THEN
                DEALLOCATE ( FSH%AJ )
           END IF
           ALLOCATE ( FSH%AJ(DIM), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR  )
                CALL IINCH ( 8*DIM**2, STR )
                CALL ERR_LOG ( 6324, IUER, 'SPHE_AJ_X', 'Failure in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dymanic memory for array FSH%AJ' )
                FL_ERROR = .TRUE.
                GOTO 820
           END IF
           FSH%AJ= 0.0D0
!
           FSH%AJ_STATUS = FSH__ALLO
           FSH%AJ_DIM  = DIM
           FSH%AJ_NORM = NORM
 820       CONTINUE 
!$OMP      END CRITICAL
           IF ( FL_ERROR ) RETURN 
      END IF
!
      FSH%AJ = 0.0D0
!#      IF ( DIM == 4 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C000004(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 8 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C000008(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 16 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C000016(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 32 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C000032(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 64 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C000064(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 128 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C000128(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 180 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C000180(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 256 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C000256(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 360 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C000360(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 512 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C000512(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 720 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C000720(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 1024 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C001024(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 1440 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C001440(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 1800 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C001800(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 2048 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C002048(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 2880 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C002880(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 4096 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C004096(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 5000 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C005000(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 5760 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C005760(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 6144 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C006144(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 8192 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C008192(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 10000 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C010000(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 10240 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C010240(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 11520 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C011520(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 12288 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C012288(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 14336 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C014336(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 16384 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C016384(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 18432 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C018432(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 20000 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C020000(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 20480 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C020480(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 22528 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C022528(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 23040 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C023040(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 24576 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C024576(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 32768 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C032768(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 40000 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C040000(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 46080 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C046080(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 60000 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C060000(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 65536 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C065536(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 80000 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C080000(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 92160 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C092160(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 100000 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C100000(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 115200 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C115200(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 120000 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C120000(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#        ELSE IF ( DIM == 131072 ) THEN
!#           FSH%AJ(1:DIM) = AJ__C131072(1:DIM)
!#           FSH%AJ_STATUS = FSH__COMP
!#           CALL ERR_LOG ( 0, IUER )
!#           RETURN
!#      END IF
!
      IF ( DIM > 128 .AND. .NOT. OMP_IN_PARALLEL() ) THEN
           NUM_THR_SAVED = OMP_GET_NUM_THREADS()
           CALL OMP_SET_NUM_THREADS ( %VAL(FSH%NUM_THR) )
           NTHR = FSH%NUM_THR
         ELSE 
           NTHR = 1
      END IF
!
      COEF = SQRI(8)/ DBLE(DIM)* DSQRT(4.0D0*PI)
      FSH%AJ = 0.0D0
!
!$OMP PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&   PRIVATE ( J1, J2, SUM1, RAT ), &
!$OMP&             SCHEDULE ( STATIC )
      DO 410 J1=0,DIM-1
         SUM1 = 0.0D0
         RAT = DBLE(J1)/ DBLE(DIM) 
         DO 420 J2=0,DIM/2-1
	    SUM1 = SUM1 + DSIN( DBLE(2*J2+1)* PI* RAT )/DBLE(2*J2+1)
 420     CONTINUE 
         FSH%AJ(J1+1) = SUM1* DSIN(PI*RAT) * COEF
 410  CONTINUE 
!$OMP END PARALLEL DO
      IF ( DIM > 128 .AND. .NOT. OMP_IN_PARALLEL() ) THEN
           CALL OMP_SET_NUM_THREADS ( %VAL(NUM_THR_SAVED) )
      END IF
!
      FSH%AJ_STATUS = FSH__COMP
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPHE_AJ_X  !#!#
