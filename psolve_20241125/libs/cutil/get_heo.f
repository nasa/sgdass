      SUBROUTINE GET_HEO ( T_SEC, HEO_EPOCH_SEC, UT1_M_TDB, L_HEO, HEO, &
     &                     VEC_HEO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_HEO computes the vector of perturbing rotation due to  *
! *   Harmonic Earth Orientation variations using the model presented    *
! *   as an expansion over quasi-harmonic series.                        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *          T_SEC ( REAL*8    ) -- Time in seconds elapsed since        *
! *                                 J2000.0 epoch ( 01 January 2000,     *
! *                                 12:00 ) for the moment for which     *
! *                                 the variations are computed.         *
! *  HEO_EPOCH_SEC ( REAL*8    ) -- Time in seconds elapsed since        *
! *                                 J2000.0 epoch ( 01 January 2000,     *
! *                                 12:00 ) of the amplitudes of the     *
! *                                 variations (which change with time). *
! *                                 since J2000.0 epoch.                 *
! *      UT1_M_TDB ( REAL*8    ) -- Value of the function UT1 minus TDB  *
! *                                 at the moment T_SEC. Units: sec.     *
! *          L_HEO ( INTEGER*4 ) -- The number of harmonic constituents. *
! *            HEO ( RECORD    ) -- Array of HEO records with harmonic   *
! *                                 EOP constituents. Dimension: L_HEO.  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   VEC_HEO ( REAL*8     ) -- The vector of the perturbation Earth     *
! *                             rotation due to harmonic variations.     *
! *                             Dimension: 3. Units: radians.            *
! *                             Components: X, Y, Z.                     *
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
! *  ### 01-OCT-2003    GET_HEO    v2.1 (c)  L. Petrov  11-MAR-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heo.i'
      REAL*8     T_SEC, HEO_EPOCH_SEC, UT1_M_TDB, VEC_HEO(3)
      INTEGER*4  L_HEO, IUER
      TYPE     ( HEO__STRUC ) HEO(L_HEO)
      REAL*8     ARG, DCOS_ARG, DSIN_ARG, PMC_AMP, PMS_AMP, E3C_AMP, E3S_AMP
      REAL*8     PI, PI2, P2I
      PARAMETER  ( PI=3.141592653589793D0, PI2=PI*2.D0, P2I = PI/2.0D0 )
      INTEGER*4  J1
!
! --- Initialization
!
      CALL NOUT_R8 ( 3, VEC_HEO )
!
! --- Summ all waves
!
      DO 410 J1=1,L_HEO
         ARG = (0.5D0*HEO(J1)%ACCL*T_SEC + HEO(J1)%FREQ)*T_SEC + &
     &         HEO(J1)%PHAS - UT1_M_TDB*PI2/86400.0D0
         DCOS_ARG = DCOS(ARG)
         DSIN_ARG = DSIN(ARG)
!
         IF ( HEO(J1)%USE_VEL ) THEN
              PMC_AMP = HEO(J1)%ROTANG(HEO__PMC,HEO__ANG) + &
     &                  HEO(J1)%ROTANG(HEO__PMC,HEO__VEL)*(T_SEC-HEO_EPOCH_SEC)
              PMS_AMP = HEO(J1)%ROTANG(HEO__PMS,HEO__ANG) + &
     &                  HEO(J1)%ROTANG(HEO__PMS,HEO__VEL)*(T_SEC-HEO_EPOCH_SEC)
              E3C_AMP = HEO(J1)%ROTANG(HEO__E3C,HEO__ANG) + &
     &                  HEO(J1)%ROTANG(HEO__E3C,HEO__VEL)*(T_SEC-HEO_EPOCH_SEC)
              E3S_AMP = HEO(J1)%ROTANG(HEO__E3S,HEO__ANG) + &
     &                  HEO(J1)%ROTANG(HEO__E3S,HEO__VEL)*(T_SEC-HEO_EPOCH_SEC)
            ELSE
              PMC_AMP = HEO(J1)%ROTANG(HEO__PMC,HEO__ANG)
              PMS_AMP = HEO(J1)%ROTANG(HEO__PMS,HEO__ANG)
              E3C_AMP = HEO(J1)%ROTANG(HEO__E3C,HEO__ANG)
              E3S_AMP = HEO(J1)%ROTANG(HEO__E3S,HEO__ANG)
         END IF
!
         VEC_HEO(1) = VEC_HEO(1) + PMC_AMP*DCOS_ARG + PMS_AMP*DSIN_ARG
         VEC_HEO(2) = VEC_HEO(2) + PMC_AMP*DSIN_ARG - PMS_AMP*DCOS_ARG
         VEC_HEO(3) = VEC_HEO(3) + E3C_AMP*DCOS_ARG + E3S_AMP*DSIN_ARG
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_HEO  #!#
