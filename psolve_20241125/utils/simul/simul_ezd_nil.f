      SUBROUTINE SIMUL_EZD_NIL ( MJD, UTC, N, AZ, EL, DHSEG, DH, CN, H,  &
     &                       VN, VE, IS_NOI, EZD, IUER )
! ***************************************************************************************
! *                                                                                     *
! *  Function SIMUL_EZD_NIL first computes the cholesky decomposition of a full         *
! *  covariance  matrix, generates the white Gausian noise with zero mean, and          *
! *  computes the equivalent zenith delay.                                              *
! *                                                                                     *
! *  REFERENCES:                                                                        *
! *              - Nilsson et al. 2007 "Simulations of atmospheric path delays  using   *
! *                turbulence models"                                                   *
! *              - Böhm et al. 2007 "Simulation of zenith wet delays an clocks"        *
! *              - Treuhaft and Lanyi 1987 "The effect of the dynamic wet troposphere   *
! *                on radio interferometric measurements"                               *
! *                                                                                     *
! *   INPUT:									        *
! *          MJD     =  Modified Julian Days                  { INT }    (Nx1)          *
! *                                                                                     *
! *          UTC     =  UTC                                   { REAL }   (Nx1)          *
! *                                                                                     *
! *          NOBS    =  No. of observations                   { INT }                   *
! *                                                                                     *
! *          AZ      =  azimuth                               { REAL }   (Nx1)   [rad]  *
! *                                                                                     *
! *          EL      =  elevation                             { REAL }   (Nx1)   [rad]  *
! *                                                                                     *
! *          DHSEG   =  day sec segment                       { REAL }            [s]   *
! *                     time segments over which observations are to be correlated      *
! *                     in seconds.                                                     * 
! *                     if 7200s (2hrs) is not short enough it might be better to       *
! *                     use 3600s (1hr). If possible use larger values.                 *
! *                                                                                     *
! *          DH      =  height increment                      { REAL }           [m]    *
! *                     for numeric integration (e.g. 200 m)                            *
! *                                                                                     *
! *          CN      =  refractive index structure constant   { REAL }     [m^(-1/3)]   *
! *                                                                                     *
! *          H       =  effective height of the troposphere   { REAL }           [m]    *
! *                                                                                     *
! *          VN      =  north component of the wind vector    { REAL }          [m/s]   *
! *                                                                                     *
! *          VE      =  east component of the wind vector     { REAL }          [m/s]   *
! *                                                                                     *
! *          SIGMA   =  Standard deviaton of white noise      { REAL }                  *
! *                                                                                     *
! *          IUER    =  Error Handler                { INT, OPT }                       *
! *                     If IUER=0 no error message will be printed,                     *
! *                     even in the event of an error. However, for                     *
! *                     other possible values, i.e. IUER=-1,-2, & -3,                   *
! *                     the error message will print to screen. For                     *
! *                     the latter case, i.e. IUER=-3, after printing                   *
! *                     the program will terminate.                                     *
! *                                                                                     *
! *                                                                                     *
! *          COV     =  Full covariance Matrix                { REAL }  (NxN)   [s^2]   *
! *                                                                                     *
! *          CHO     =  Lower triangular decomposed Cov       { REAL }   (NxN)  [s]     *
! *                                                                                     *
! *   OUTPUT:                                                                           *
! *          GAU_NOI =  Random vector of white noise          { REAL }   (Nx1)  []      *
! *                                                                                     *
! *          EZD     = simulated equivalent zenith delay      { REAL }   (Nx1)  [s]     *
! *                                                                                     *
! *  ###  23-JUN-2021    SIMUL_EZD_NIL         v1.0 (c)  N. Habana     23-JUN-2021 ###  *
! *                                                                                     *
! ***************************************************************************************
      IMPLICIT   NONE
      INCLUDE    'astro_constants.i'
      INTEGER*4  IUER
      INTEGER*4  N, IS_NOI, MJD(N)
      REAL*8     UTC(N), AZ(N), EL(N)
      REAL*8     EZD(N)
      REAL*8,    ALLOCATABLE :: COV(:,:), CHO(:,:), GAU_NOI(:)
      REAL*8     DHSEG, DH, CN, H, VN, VE, EZD0
      REAL*8     CNALL, SIGMA
      CHARACTER  STR*128
      INTEGER*4  J0, J1, J2, J3, J4, J5, J6, IER
      REAL*8,    EXTERNAL :: RGAUSS
!
      SIGMA = 1.0D0
!
      ALLOCATE ( COV(N,N), CHO(N,N), GAU_NOI(N), STAT= IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 2*8*N*N + 8*N, STR )
           CALL ERR_LOG ( 7751, IUER, 'SIMUL_EZD_NIL', 'Error in '// &
     &         'computation of the covariance matrix of the colored noise' )
           RETURN 
      END IF
!
! --- Generate the Covariance Matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL COV_MAT_NHE ( MJD, UTC, N, AZ, EL, DHSEG, DH, CN, H,         &
     &                   VN, VE, COV, IER )
      IF ( IER .NE. 0 ) THEN
           DEALLOCATE ( COV, CHO, GAU_NOI )
           CALL ERR_LOG ( 7752, IUER, 'SIMUL_EZD_NIL', 'Error in '// &
     &         'computation of the covariance matrix of the colored noise' )
           RETURN 
      END IF
!
! --- Generate the random noise vector
!
      DO 210 J1 = 1, N
         GAU_NOI(J1) = RGAUSS ( IS_NOI, SIGMA )
 210  CONTINUE
!
! --- Cholesky decomposition
!
      CHO = COV
      CALL DPOTRF ( 'L', N, CHO, N, IER )
      IF ( IER .NE. 0 ) THEN
           DEALLOCATE ( COV, CHO, GAU_NOI )
           CALL ERR_LOG ( 7753, IUER, 'SIMUL_EZD_NIL', 'Error in '// &
     &         'Cholesky decomosition of the covariance matrix of '// &
     &         'colored noise' )
           RETURN 
      END IF
!
! --- 'Zero' out elements above the diagonal
!
      DO 310 J1 = 1, N
         DO 320 J2 = J1+1, N
            CHO(J1,J2) = 0.D0
 320     CONTINUE
 310  CONTINUE
!
! --- Compute the equivalent zenith delay by multiplying the 
! --- radnom noise vector with the Cholesky decomposition
!
      CALL MUL_MV_IV_V ( N, N, CHO, N, GAU_NOI, N, EZD, IER )
!
      DEALLOCATE ( COV, CHO, GAU_NOI )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SIMUL_EZD_NIL  !#!#
