      SUBROUTINE VTD_GET_AZEL ( SOU_NAM, STA1_NAM, STA2_NAM, MJD, TAI, VTD, &
     &                          AZ, EL, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine VTD_GET_AZEL
! *                                                                      *
! *  ### 30-JAN-2006  VTD_GET_AZEL  v1.0 (c)  L. Petrov  30-JAN-2006 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      CHARACTER  SOU_NAM*(*), STA1_NAM*(*), STA2_NAM*(*)
      INTEGER*4  MJD, IUER
      REAL*8     TAI, AZ(2), EL(2)
      INTEGER*4  ISTA(2), ISOU
      INTEGER*4, EXTERNAL :: VTD_STA_INDEX, VTD_SOU_INDEX 
!
      ISTA(1) = VTD_STA_INDEX ( VTD, STA1_NAM )
      ISTA(2) = VTD_STA_INDEX ( VTD, STA2_NAM )
      ISOU    = VTD_SOU_INDEX ( VTD, SOU_NAM  )
!
      EL(1) = VTD%STA(ISTA(1))%ELEV
      EL(2) = VTD%STA(ISTA(2))%ELEV
      AZ(1) = VTD%STA(ISTA(1))%AZ
      AZ(2) = VTD%STA(ISTA(2))%AZ
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_GET_AZEL  !#!  
