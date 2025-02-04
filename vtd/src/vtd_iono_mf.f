      FUNCTION   VTD_IONO_MF ( ELEV, IONO_HEI )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_IONO_MF computes the ionosphere mapping function,      *
! *   considering the ionosphere as a narrow shell with height           *
! *   IONO_HEI (in meteres).                                             *
! *                                                                      *
! *  ### 26-SEP-2010   VTD_IONO_MF  v1.1 (c) L. Petrov  06-OCT-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      REAL*8     VTD_IONO_MF 
      REAL*8     ELEV, IONO_HEI
      REAL*8     BETA, EPS, MF__MAX
      PARAMETER  ( EPS     = 1.D-4 )
      PARAMETER  ( MF__MAX =    32 )
!
      BETA = DASIN ( DCOS(ELEV)/(1.0D0 + IONO_HEI/VTD__RMN) )
      IF ( DABS(DCOS(BETA)) > EPS ) THEN
           VTD_IONO_MF = 1.D0/DCOS(BETA)
         ELSE 
           VTD_IONO_MF = 1.D0/DCOS(EPS)
      END IF
!
      RETURN
      END  FUNCTION  VTD_IONO_MF  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   VTD_IONO_SLM_MF ( ELEV )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_IONO_SLM_MF computes the ionosphere mapping function,  *
! *   considering the ionosphere as a narrow shell with height           *
! *   IONO__HEI (in meteres) with applying a scaling factor to the       *
! *   elevation factor following ...
! *                                                                      *
! * ### 26-SEP-2010 VTD_IONO_SLM_MF  v1.1 (c) L. Petrov  06-OCT-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      REAL*8     VTD_IONO_SLM_MF
      REAL*8     ELEV 
      REAL*8     BETA, EPS, MF__MAX, Z__FCT, IONO__HEI
      PARAMETER  ( EPS       =    1.D-4  )
      PARAMETER  ( MF__MAX   =       32  )
      PARAMETER  ( Z__FCT    = 0.9782D0  )
      PARAMETER  ( IONO__HEI = 506700.D0 ) ! wrt the volumetric mean radius
!
      IF ( ELEV > EPS ) THEN
           VTD_IONO_SLM_MF = 1.D0/DSQRT ( 1.0D0 - ( VTD__RMN/(VTD__RMN+IONO__HEI) * &
     &                                              DSIN(Z__FCT*(P2I-ELEV)) )**2 &
     &                                  )
         ELSE
           VTD_IONO_SLM_MF = 1.D0/DSQRT ( 1.0D0 - ( VTD__RMN/(VTD__RMN+IONO__HEI) * &
     &                                              DSIN(Z__FCT*(P2I-EPS)) )**2 &
     &                                  )
      END IF
!
      RETURN
      END  FUNCTION  VTD_IONO_SLM_MF  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   VTD_IONO_600_MF ( ELEV )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_IONO_SLM_MF computes the ionosphere mapping function,  *
! *   considering the ionosphere as a narrow shell with height           *
! *   IONO__HEI (in meteres) with applying a scaling factor to the       *
! *   elevation factor following 
! *                                                                      *
! * ### 26-SEP-2010  VTD_IONO_600_MF v1.1 (c) L. Petrov  06-OCT-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      REAL*8     VTD_IONO_600_MF
      REAL*8     ELEV 
      REAL*8     BETA, EPS, MF__MAX, Z__FCT, IONO__HEI
      PARAMETER  ( EPS       =    1.D-4  )
      PARAMETER  ( MF__MAX   =       32  )
      PARAMETER  ( Z__FCT    = 0.9782D0  )
      PARAMETER  ( IONO__HEI = 600000.D0 ) ! wrt the volumetric mean radius
!
      IF ( ELEV > EPS ) THEN
           VTD_IONO_600_MF = 1.D0/DSQRT ( 1.0D0 - ( VTD__RMN/(VTD__RMN+IONO__HEI) * &
     &                                              DSIN(Z__FCT*(P2I-ELEV)) )**2 &
     &                                  )
         ELSE
           VTD_IONO_600_MF = 1.D0/DSQRT ( 1.0D0 - ( VTD__RMN/(VTD__RMN+IONO__HEI) * &
     &                                              DSIN(Z__FCT*(P2I-EPS)) )**2 &
     &                                  )
      END IF
!
      RETURN
      END  FUNCTION  VTD_IONO_600_MF !#!#
