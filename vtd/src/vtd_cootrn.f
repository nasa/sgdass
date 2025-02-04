      SUBROUTINE VTD_COOTRN ( TRANSF_CODE, VTD, COO_IN, TIM_IN, &
     &                        COO_OUT, TIM_OUT )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine VTD_COOTRN executes a generalized Lorenz        *
! *   transformation from input coordinates system to the output         *
! *   refrence system.                                                   *
! *                                                                      *
! *  ### 02-JAN-2006   VTD_COOTRN  v1.0 (c)  L. Petrov  02-JAN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  TRANSF_CODE
      REAL*8     COO_IN(3), TIM_IN, COO_OUT(3), TIM_OUT
      REAL*8     DIST_SUN_EARTH, VE_SQ, PAR1, PAR2, PAR3, PAR4, PAR5
      REAL*8,    EXTERNAL :: DP_VV_V
!
      IF ( TRANSF_CODE == VTD__I92_TO_BRS ) THEN
           DIST_SUN_EARTH = DSQRT (   &
     &                             ( VTD%MOM%PLAN(1,VTD__COO,VTD__EART) -     &
     &                               VTD%MOM%PLAN(1,VTD__COO,VTD__SUN) )**2 + &
     &                             ( VTD%MOM%PLAN(2,VTD__COO,VTD__EART) - &
     &                               VTD%MOM%PLAN(2,VTD__COO,VTD__SUN) )**2 + &
     &                             ( VTD%MOM%PLAN(3,VTD__COO,VTD__EART)- &
     &                               VTD%MOM%PLAN(3,VTD__COO,VTD__SUN) )**2   )
           VE_SQ = &
     &                             ( VTD%MOM%PLAN(1,VTD__VEL,VTD__EART) -     &
     &                               VTD%MOM%PLAN(1,VTD__VEL,VTD__SUN) )**2 + &
     &                             ( VTD%MOM%PLAN(2,VTD__VEL,VTD__EART) - &
     &                               VTD%MOM%PLAN(2,VTD__VEL,VTD__SUN) )**2 + &
     &                             ( VTD%MOM%PLAN(3,VTD__VEL,VTD__EART)- &
     &                               VTD%MOM%PLAN(3,VTD__VEL,VTD__SUN) )**2
           PAR1 = 1.0D0 - 2.0D0*VTD__GM(VTD__SUN)/DIST_SUN_EARTH/VTD__C**2 &
     &            - VTD__LB + VTD__LG
           PAR2 = DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), COO_IN )/ &
     &            2.0D0/VTD__C**2
           PAR3 = 1.0D0 + VTD__GM(VTD__SUN)/DIST_SUN_EARTH/VTD__C**2 &
     &            + VE_SQ/2.0D0/VTD__C**2 - VTD__LB + VTD__LG
           CALL ADDC_VV ( 3,  PAR1, COO_IN, &
     &                       -PAR2, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                        COO_OUT )
           TIM_OUT = PAR3*TIM_IN 
         ELSE IF ( TRANSF_CODE == VTD__BRS_TO_I92 ) THEN
           DIST_SUN_EARTH = DSQRT (   &
     &                             ( VTD%MOM%PLAN(1,VTD__COO,VTD__EART) -     &
     &                               VTD%MOM%PLAN(1,VTD__COO,VTD__SUN) )**2 + &
     &                             ( VTD%MOM%PLAN(2,VTD__COO,VTD__EART) - &
     &                               VTD%MOM%PLAN(2,VTD__COO,VTD__SUN) )**2 + &
     &                             ( VTD%MOM%PLAN(3,VTD__COO,VTD__EART)- &
     &                               VTD%MOM%PLAN(3,VTD__COO,VTD__SUN) )**2   )
           VE_SQ = &
     &                             ( VTD%MOM%PLAN(1,VTD__VEL,VTD__EART) -     &
     &                               VTD%MOM%PLAN(1,VTD__VEL,VTD__SUN) )**2 + &
     &                             ( VTD%MOM%PLAN(2,VTD__VEL,VTD__EART) - &
     &                               VTD%MOM%PLAN(2,VTD__VEL,VTD__SUN) )**2 + &
     &                             ( VTD%MOM%PLAN(3,VTD__VEL,VTD__EART)- &
     &                               VTD%MOM%PLAN(3,VTD__VEL,VTD__SUN) )**2
           PAR1 = 1.0D0 + 2.0D0*VTD__GM(VTD__SUN)/DIST_SUN_EARTH/VTD__C**2 &
     &            + VTD__LB - VTD__LG
           PAR2 = DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), COO_IN )/ &
     &            2.0D0/VTD__C**2
           PAR3 = 1.0D0 - VTD__GM(VTD__SUN)/DIST_SUN_EARTH/VTD__C**2 &
     &            + VE_SQ/2.0D0/VTD__C**2 + VTD__LB - VTD__LG
           PAR4 = 1.0D0
           PAR5 = 1.0D0 - VTD__GM(VTD__SUN)/DIST_SUN_EARTH/VTD__C**2 &
     &            + VE_SQ/2.0D0/VTD__C**2 + VTD__LB - VTD__LG
           CALL ADDC_VV ( 3, PAR1, COO_IN, &
     &                       PAR2, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                       COO_OUT )
           CALL ADDC_VV ( 3, 1.0D0, COO_OUT, -PAR3*(TIM_IN+PAR4), &
     &                    VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), COO_OUT )
           TIM_OUT = PAR5*TIM_IN - &
     &               DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), COO_IN )/ &
     &               VTD__C**2*1.d-12
         ELSE IF ( TRANSF_CODE == VTD__GRS_TO_I92 ) THEN
         ELSE IF ( TRANSF_CODE == VTD__GRS_TO_BRS ) THEN
         ELSE IF ( TRANSF_CODE == VTD__BRS_TO_GRS ) THEN
         ELSE IF ( TRANSF_CODE == VTD__I92_TO_GRS ) THEN
         ELSE 
           CALL ERR_LOG ( 2001, -1, 'VTD_COOTRN', 'Trap of internal '// &
     &         'control: unsupported transfromation code' )
           CALL EXIT ( 1 )
      END IF
      RETURN
      END  SUBROUTINE  VTD_COOTRN  !#!  
