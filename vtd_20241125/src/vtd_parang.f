      SUBROUTINE VTD_PARANG ( AVEC_CRS, AVEC_CRS_RATE, S_CRS, TRS_TO_CRS, &
     &                        PARAL_ANG, PARAL_ANG_RATE )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_PARANG  computes contribution to the phase of the     *
! *   received LEFT CIRULAR POLARIZED signal due to antenna feed-box     *
! *   rotation and its time derivative. NB: if the antenna receives the  *
! *   right cicrular polarization, as it usually done in geodetic        *
! *   experiments, the sign at phase should be reversed,                 *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      AVEC_CRS ( REAL*8    ) -- Unit vector to the direction of the   *
! *                                antenna's fixed axis in the celestial *
! *                                coordinate system.                    *
! * AVEC_CRS_RATE ( REAL*8    ) -- Rate of change of the antenna's axis  *
! *                                offset vector in the celestial        *
! *                                coordinate system.                    *
! *                                Dimension: 3. Units: meter/sec.       *
! *         S_CRS ( REAL*8    ) -- Unit vector of direction to the       *
! *                                source.                               *
! *    TRS_TO_CRS ( REAL*8    ) -- Matrix of trasnformation from the     *
! *                                terrestrial coordinate system to the  *
! *                                celestial coordinate system.          *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *      PARAL_ANG ( REAL*8   ) -- Parallacic angle for the left         *
! *                                circular polarization (rad).          *
! * PARAL_ANG_RATE ( REAL*8   ) -- Rate of change of the parallactic     *
! *                                angle for the left circular           *
! *                                polarization (rad/s).                 *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
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
! *  ### 14-JUN-2007   VTD_PARANG  v1.1 (c)  L. Petrov  10-SEP-2011 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      REAL*8     AVEC_CRS(3), AVEC_CRS_RATE(3), S_CRS(3), TRS_TO_CRS(3,3), &
     &           PARAL_ANG, PARAL_ANG_RATE
      REAL*8     A_PER(3), A_PER_RATE(3), Z_PER(3), Z_TRS(3), Z_CRS(3), &
     &           AS_VEC(3), AS_VEC_RATE(3), ZS_VEC(3), AA_RATE, RD, DP, SGN
      REAL*8     EPS, RD__MIN 
      PARAMETER  ( EPS     = 1.D-15 )
      PARAMETER  ( RD__MIN = 1.D-6  )
      REAL*8,    EXTERNAL :: DP_VV_V 
!
      Z_TRS(1) = 0.0D0
      Z_TRS(2) = 0.0D0
      Z_TRS(3) = 1.0D0
!
! --- Comppute the vector of the celestial pole in J2000.0 coordinate system
! --- It differs from 0,0,1 due to precession-nutation
!
      CALL MUL_MV_IV_V ( 3, 3, TRS_TO_CRS, 3, Z_TRS, 3, Z_CRS, -2 )
!
! --- Compute the vector A_PER, which is in the plane A_VEC, S_VEC and 
! --- orthogonal to S_VEC
!
      CALL VM83     ( AVEC_CRS, S_CRS, AS_VEC )
      CALL NORM_VEC ( 3, AS_VEC, RD )
      CALL VM83     ( S_CRS, AS_VEC, A_PER )
!
! --- Compute time derivative of vector A_PER
!
      CALL VM83     ( AVEC_CRS_RATE, S_CRS, AS_VEC_RATE )
      IF ( RD > RD__MIN ) THEN
           CALL MUL_VC_V ( 3, AS_VEC_RATE, 1.0D0/RD )
         ELSE 
           AS_VEC_RATE(1) = 0.0D0
           AS_VEC_RATE(2) = 0.0D0
           AS_VEC_RATE(3) = 0.0D0
      END IF
      AA_RATE = DP_VV_V  ( 3, AS_VEC, AS_VEC_RATE )
      CALL ADDC_VV  ( 3, 1.0D0, AS_VEC_RATE, -AA_RATE, AS_VEC, AS_VEC_RATE )
      CALL VM83     ( S_CRS, AS_VEC_RATE, A_PER_RATE )
!
! --- Compute the vector Z_PER, which is in the plane Z_VEC, S_VEC and 
! --- orthogonal to S_VEC
!
      CALL VM83     ( Z_CRS, S_CRS, ZS_VEC )
      CALL NORM_VEC ( 3, ZS_VEC, RD )
      CALL VM83     ( S_CRS, ZS_VEC, Z_PER )
!
! --- Determine the sign of the parallactic angle
!
      SGN = DP_VV_V ( 3, ZS_VEC, AVEC_CRS )
      IF ( SGN < 0.0D0 ) THEN
           SGN = -1.0D0
         ELSE 
           SGN = +1.0D0
      END IF
!
! --- Compute parallactic angle
!
      DP = DP_VV_V ( 3, Z_PER, A_PER )
      IF ( DP < -1.0D0 + EPS ) DP = -1.0D0 + EPS 
      IF ( DP >  1.0D0 - EPS ) DP =  1.0D0 - EPS 
      PARAL_ANG = SGN*DACOS ( DP ) 
!
! --- ... and its time derivative
!
      IF ( DP_VV_V ( 3, Z_PER, A_PER ) > 1.D0-EPS )  THEN
           PARAL_ANG_RATE =  0.0D0
         ELSE 
           PARAL_ANG_RATE = -DP_VV_V( 3, Z_PER, A_PER_RATE )/DSIN(PARAL_ANG)
      END IF
!      
      RETURN
      END  SUBROUTINE  VTD_PARANG  !#!#
