        FUNCTION  RGAUSS ( IS, SIGMA )
! ************************************************************************
! *                                                                      *
! *   Routine RGAUSS returns a ranadom number with Gaussian              *
! *   distribution.                                                      *
! *                                                                      *
! *   Constant CONST was adjusted to fit the normal distribution using   *
! *   1 billion trials.                                                  *
! *                                                                      *
! *  ### 21-APR-1989     RAGUSS    v3.3 (c)  L. Petrov  20-NOV-2021 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        REAL*8     RGAUSS, SIGMA, CONST
        INTEGER*4  ISEED_SAVE
        COMMON  /  RANDOM__COMMON_SEED / ISEED_SAVE
        INTEGER*4  IS
        REAL*4     RAN
        INTEGER*4   NR
!!        PARAMETER ( CONST = 0.42963484233355448D0 ) ! GNU
!!        PARAMETER   ( CONST = 0.429609646422957D0   ) ! Intel
        PARAMETER   ( CONST = 0.43300702 ) ! GNU on 2021.11.20, GCC 11
        PARAMETER ( NR = 16 ) 
!@        PARAMETER ( CONST = 0.30498070675898703D0 ) ! GNU
!@        PARAMETER ( CONST = 0.304998728160074D0   ) ! Intel
!@        PARAMETER ( N = 32 ) 
!@        PARAMETER ( CONST = 0.21617926347854094D0 ) ! GNU
!@        PARAMETER ( CONST = 0.216087439249858D0   ) ! Intel
!@        PARAMETER ( N = 64 ) 
#ifdef GNU
        REAL*8     RAN_VAL
        INTEGER*4  J1, J2, N, IR, SEEDS(64)
#endif
!
        RGAUSS=0.D0
#ifdef GNU
        CALL RANDOM_SEED ( GET=SEEDS )
        IF ( ISEED_SAVE .NE. IS ) THEN
             CALL RANDOM_SEED ( SIZE=N )
             SEEDS(1) = IS
             DO 410 J1=2,N
                SEEDS(J1) = IEOR ( SEEDS(J1-1), ISHFTC(SEEDS(J1-1),1)  )
 410         CONTINUE 
             CALL RANDOM_SEED ( PUT=SEEDS )
             ISEED_SAVE = SEEDS(1)
        END IF
#endif
        DO 420 J2=1,NR
#ifdef GNU           
           CALL RANDOM_NUMBER ( RAN_VAL )
           RGAUSS = RGAUSS + 2.D0*RAN_VAL - 1.D0
#else
           RGAUSS = RGAUSS + 2.D0*RAN(IS) - 1.D0
#endif
  420   CONTINUE
        RGAUSS = RGAUSS*SIGMA*CONST
!
        RETURN
        END  FUNCTION  RGAUSS  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   RANDOM_NUMB ( IS, VAL_MIN, VAL_MAX )
! ************************************************************************
! *                                                                      *
! *   Routine RANDOM_NUMB returns a ranadom number, uniformly            *
! *   distributed in the range [VAL_MIN, VAL_MAX].                       *
! *                                                                      *
! *  ### 21-APR-1989  RANDOM_NUMB  v2.2 (c)  L. Petrov  29-JUN-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IS
      REAL*8     RANDOM_NUMB, VAL_MIN, VAL_MAX, EPS
      INTEGER*4  ISEED_SAVE
      COMMON  /  RANDOM__COMMON_SEED / ISEED_SAVE
      PARAMETER  ( EPS = 1.D-30 )
      INTEGER*4  J1, N, IR, SEEDS(64)
      REAL*8     RAN_VAL
#ifdef GNU
      CALL RANDOM_SEED ( GET=SEEDS )
      IF ( ISEED_SAVE .NE. IS ) THEN
           CALL RANDOM_SEED ( SIZE=N )
           SEEDS(1) = IS
           DO 410 J1=2,N
              SEEDS(J1) = IEOR ( SEEDS(J1-1), ISHFTC(SEEDS(J1-1),1)  )
 410       CONTINUE 
           CALL RANDOM_SEED ( PUT=SEEDS )
           ISEED_SAVE = SEEDS(1)
      END IF
#endif
#ifdef GNU           
      CALL RANDOM_NUMBER ( RAN_VAL )
#else
      RAN_VAL = RAN(IS)
#endif
      IF ( (VAL_MAX - VAL_MIN) > EPS ) THEN
           RANDOM_NUMB = VAL_MIN + RAN_VAL*(VAL_MAX - VAL_MIN)
         ELSE 
           RANDOM_NUMB = EPS
      END IF
      RETURN
      END  FUNCTION   RANDOM_NUMB  !#!#
!
!
! ------------------------------------------------------------------------

      BLOCK DATA RANDOM__COMMON_BLC
      INTEGER*4  ISEED_SAVE
      COMMON   / RANDOM__COMMON_SEED / ISEED_SAVE
      DATA       ISEED_SAVE / 0 /
      END BLOCK DATA RANDOM__COMMON_BLC
