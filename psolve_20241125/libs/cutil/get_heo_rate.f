      SUBROUTINE GET_HEO_RATE ( T_SEC, HEO_EPOCH_SEC, UT1_M_TDB, L_HEO, HEO, &
     &                          VEC_HEO_RATE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_HEO_RATE computes the vector of perturbation the       *
! *   Earth's angular velocity due to harmonic variations in the Earth   *
! *   rotation presented as an expansion over quasi-harmonic series.     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *         T_SEC ( REAL*8    ) -- Time in seconds which has elapsed     *
! *                                since J2000.0 epoch ( 01 January      *
! *                                2000, 12:00 )                         *
! * HEO_EPOCH_SEC ( REAL*8    ) -- Time in seconds elapsed since         *
! *                                J2000.0 epoch ( 01 January 2000,      *
! *                                12:00 ) of the amplitudes of the      *
! *                                variations (which change with time).  *
! *                                since J2000.0 epoch.                  *
! *     UT1_M_TDB ( REAL*8    ) -- Value of the function UT1 minus TDB   *
! *                                at the moment T_SEC. Units: sec.      *
! *         L_HEO ( INTEGER*4 ) -- The number of harmonic constituents.  *
! *           HEO ( RECORD    ) -- Array of HEO records with harmonic    *
! *                                EOP constituents. Dimension: L_HEO.   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  VEC_HEO_RATE ( REAL*8    ) -- The vector of the perturbation        *
! *                                Earth's angular velocity due to       *
! *                                harmonic variations. Dimension: 3.    *
! *                                Units: rad/sec. Components: X, Y, Z.  *
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
! *  ### 02-OCT-2003  GET_HEO_RATE v3.0 (c)  L. Petrov  08-JUN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heo.i'
      REAL*8     T_SEC, HEO_EPOCH_SEC, UT1_M_TDB, VEC_HEO_RATE(3)
      INTEGER*4  L_HEO, IUER
      TYPE     ( HEO__STRUC ) HEO(L_HEO)
      REAL*8     ARG, DCOS_ARG, DSIN_ARG, TIM_INT
      INTEGER*4  J1
!
      CALL NOUT_R8 ( 3, VEC_HEO_RATE )
      TIM_INT = (T_SEC - HEO_EPOCH_SEC)
      DO 410 J1=1,L_HEO
         ARG = (0.5D0*HEO(J1)%ACCL*T_SEC + HEO(J1)%FREQ)*T_SEC + &
     &         HEO(J1)%PHAS - UT1_M_TDB*PI2/86400.0D0
         DCOS_ARG = DCOS(ARG)
         DSIN_ARG = DSIN(ARG)
!
         IF ( HEO(J1)%USE_VEL ) THEN
              VEC_HEO_RATE(1) = VEC_HEO_RATE(1) &
     &          + (                  HEO(J1)%ROTANG(HEO__PMC,HEO__VEL) &
     &              + HEO(J1)%FREQ * HEO(J1)%ROTANG(HEO__PMS,HEO__ANG) &
     &              + HEO(J1)%FREQ * HEO(J1)%ROTANG(HEO__PMS,HEO__VEL)*TIM_INT &
     &            )* DCOS_ARG &
     &          + (                  HEO(J1)%ROTANG(HEO__PMS,HEO__VEL) &
     &              - HEO(J1)%FREQ * HEO(J1)%ROTANG(HEO__PMC,HEO__ANG) &
     &              - HEO(J1)%FREQ * HEO(J1)%ROTANG(HEO__PMC,HEO__VEL)*TIM_INT &
     &            )* DSIN_ARG
!
              VEC_HEO_RATE(2) = VEC_HEO_RATE(2) &
     &          + ( -                HEO(J1)%ROTANG(HEO__PMS,HEO__VEL) &
     &              + HEO(J1)%FREQ * HEO(J1)%ROTANG(HEO__PMC,HEO__ANG) &
     &              + HEO(J1)%FREQ * HEO(J1)%ROTANG(HEO__PMC,HEO__VEL)*TIM_INT &
     &            )* DCOS_ARG &
     &          + (                  HEO(J1)%ROTANG(HEO__PMC,HEO__VEL) &
     &              + HEO(J1)%FREQ * HEO(J1)%ROTANG(HEO__PMS,HEO__ANG) &
     &              + HEO(J1)%FREQ * HEO(J1)%ROTANG(HEO__PMS,HEO__VEL)*TIM_INT &
     &            )* DSIN_ARG
!
              VEC_HEO_RATE(3) = VEC_HEO_RATE(3) &
     &          + (                  HEO(J1)%ROTANG(HEO__E3C,HEO__VEL) &
     &              + HEO(J1)%FREQ * HEO(J1)%ROTANG(HEO__E3S,HEO__ANG) &
     &              + HEO(J1)%FREQ * HEO(J1)%ROTANG(HEO__E3S,HEO__VEL)*TIM_INT &
     &            )* DCOS_ARG &
     &          + (                  HEO(J1)%ROTANG(HEO__E3S,HEO__VEL) &
     &              - HEO(J1)%FREQ * HEO(J1)%ROTANG(HEO__E3C,HEO__ANG) &
     &              - HEO(J1)%FREQ * HEO(J1)%ROTANG(HEO__E3C,HEO__VEL)*TIM_INT &
     &            )* DSIN_ARG
            ELSE
              VEC_HEO_RATE(1) = VEC_HEO_RATE(1) + &
     &                        (  - HEO(J1)%ROTANG(HEO__PMC,HEO__ANG)*DSIN_ARG &
     &                           + HEO(J1)%ROTANG(HEO__PMS,HEO__ANG)*DCOS_ARG &
     &                        ) * HEO(J1)%FREQ
              VEC_HEO_RATE(2) = VEC_HEO_RATE(2) + &
     &                        (  + HEO(J1)%ROTANG(HEO__PMC,HEO__ANG)*DCOS_ARG &
     &                           + HEO(J1)%ROTANG(HEO__PMS,HEO__ANG)*DSIN_ARG &
     &                        ) * HEO(J1)%FREQ
              VEC_HEO_RATE(3) = VEC_HEO_RATE(3) + &
     &                        (  - HEO(J1)%ROTANG(HEO__E3C,HEO__ANG)*DSIN_ARG &
     &                           + HEO(J1)%ROTANG(HEO__E3S,HEO__ANG)*DCOS_ARG &
     &                        ) * HEO(J1)%FREQ
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_HEO_RATE  #!#
