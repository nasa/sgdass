      PROGRAM    AJ_COMP
      IMPLICIT   NONE 
      CHARACTER  DIR*128, FILOUT*128, STR*128
      INTEGER*4  ND
      PARAMETER  ( ND = 49 )
      INTEGER*4  DIM_ARR(ND)
      DATA       DIM_ARR / &
    &             20480, & ! 20k
    &                 4, & ! 
    &                 8, & ! 
    &                16, & ! 
    &                32, & ! 
    &                64, & ! 
    &               128, & ! 
    &               256, & ! 
    &               512, & ! 
    &              1024, & !  1k
    &              2048, & !  2k
    &              4096, & !  4k
    &              6144, & !  6k
    &              8192, & !  8k
    &             10240, & ! 10*1024 
    &             12288, & ! 12k
    &             14336, & ! 14k
    &             16384, & ! 16k
    &             18432, & ! 18k
    &             22528, & ! 22k
    &             24576, & ! 24k
    &             32768, & ! 32k
    &             65536, & ! 64k
    &            131072, & ! 128k
    &               180, & !      1 x 1
    &               360, & !    1/2 x 1/2 
    &               720, & !    1/4 x 1/4
    &              1440, & !    1/8 x 1/8
    &              2880, & !   1/16 x 1/16
    &              5760, & !   1/32 x 1/32
    &             11520, & !   1/64 x 1/64
    &             23040, & !  1/128 x 1/128
    &             46080, & !  1/256 x 1/256
    &             92160, & !  1/512 x 1/512
    &              1800, & !    0.1 x 0.1
    &              3600, & !   0.05 x 0.05
    &              7200, & !   0.02 x 0.02
    &             14400, & !   0.01 x 0.01
    &             28800, & !  0.005 x 0.005
    &             57600, & ! 0.0025 x 0.025
    &            115200, & !  0.002 x 0.002
    &              5000, & !
    &             10000, & !
    &             20000, & !
    &             40000, & !
    &             60000, & !
    &             80000, & !
    &            100000, & !
    &            120000  & !
    &                    /
!
      INTEGER*4  J1, J2, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      DIR = '/progs/fourpack_20150718/include'
!
!!      DO 410 J1=1,ND
      DO 410 J1=1,1
         CALL INCH   ( DIM_ARR(J1), STR(1:6) )
         CALL CHASHR ( STR(1:6) )
         CALL BLANK_TO_ZERO ( STR(1:6) )
         FILOUT = DIR(1:I_LEN(DIR))//'/aj_'//STR(1:6)//'.i'
         IUER = -1
         CALL SPHE_AJ_OUT ( DIM_ARR(J1), FILOUT, IUER )
 410  CONTINUE 
!
      END  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPHE_AJ_OUT ( DIM, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPHE_AJ  precomputes coefficients A_j for the Driscoll    *
! *   and Healy (1994) spherical harmonics quadrature.                   *
! *                                                                      *
! *   Output is written in FSH%AJ                                        *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  DIM ( INTEGER*4 ) -- Dimension.                                     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * FILOUT ( CHARACTER  ) -- Output file name.                           *
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
! * ### 14-OCT-2015   SPHE_AJ v1.0 modified by L. Petrov 14-OCT-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      INTEGER*4  DIM, NORM, IUER
      CHARACTER  FILOUT*(*), SGN*1
      INTEGER*4  J1, J2, LUN, IER
      REAL*8     COEF, RAT, SUM1, AJ(2*FSH__MAX_DEG+7)
      CHARACTER  STR*128, STR1*128
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_UNIT
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN' )
      CALL CLRCH  ( STR ) 
      CALL INCH   ( DIM, STR(1:6) )
      CALL CHASHR ( STR(1:6) )
      CALL INCH   ( DIM, STR(11:16) )
      CALL CHASHR ( STR(11:16) )
      CALL BLANK_TO_ZERO ( STR(1:6) )
      CALL INCH   ( DIM, STR(21:26) )
!
      WRITE ( LUN, '(A)' ) '!'
      WRITE ( LUN, '(A,I5)' ) '! --- AJ coefficients for dimension ', DIM
      WRITE ( LUN, '(A)' ) '!'
      WRITE ( LUN, '(A)' ) '      INTEGER*4  AJ__M'//STR(1:6)//', AJ__N'//STR(1:6)
      WRITE ( LUN, '(A)' ) '      PARAMETER  ( AJ__M'//STR(1:6)//' = '//STR(11:16)//' ) '
      WRITE ( LUN, '(A)' ) '      REAL*8     AJ__C'//STR(1:6)//'(0:AJ__M'//STR(1:6)//')'
      WRITE ( LUN, '(A)' ) '!'
      WRITE ( LUN, '(A)' ) '      DATA ( AJ__C'//STR(1:6)//'(AJ__N'//STR(1:6)// &
     &                                        '), AJ__N'//STR(1:6)// &
     &                                        '=0,AJ__M'//STR(1:6)//'  )   /  &'
!
      COEF = SQRI(8)/ DBLE(DIM)* DSQRT(4.0D0*PI)
      WRITE ( LUN, 110 ) 0.0D0, ',', CHAR(33), 0
 110  FORMAT ( '     &       ', 1PD23.17, A, '        &  ',A,' ', I6 )
      DO 410 J1=0,DIM-1
         SGN = ','
	 SUM1 = 0.0D0
         RAT = DBLE(J1)/ DBLE(DIM) 
         DO 420 J2=0,DIM/2-1
	    SUM1 = SUM1 + DSIN( DBLE(2*J2+1)* PI* RAT )/DBLE(2*J2+1)
 420     CONTINUE 
         AJ(J1+1) = SUM1* DSIN(PI*RAT) * COEF
         IF ( J1 == DIM-1 ) SGN = ' '
         WRITE ( LUN, 110 ) AJ(J1+1), SGN, CHAR(33), J1+1 ! %%%%%%%%%%%%%%%
 410  CONTINUE 
      WRITE ( LUN, '(A)' ) '     &                                       /'
      CLOSE ( UNIT=LUN )
      WRITE ( 6, * ) 'Written file '//FILOUT(1:I_LEN(FILOUT))
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPHE_AJ_OUT  !#!#
