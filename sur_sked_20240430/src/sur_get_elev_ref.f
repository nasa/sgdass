      FUNCTION SUR_GET_ELEV_REF ( SUR, VTD, CUR_TYP, IND_SRC, IUER )
! ************************************************************************
! *                                                                      *
! *   Function SUR_GET_ELEV_REF
! *                                                                      *
! * ### 15-DEC-2011  SUR_GET_ELEV_REF v1.0 (c) L. Petrov 15-DEC-2011 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      REAL*8     SUR_GET_ELEV_REF 
      INTEGER*4  CUR_TYP, IND_SRC, IUER
      REAL*8     AZ, EL, HA
      INTEGER*4  IER
!
! --- Compute for each station azimuth, elevation and hour angle
!
      CALL ERR_PASS ( IUER, IER )
      CALL SUR_AZEL ( SUR, VTD, CUR_TYP, SUR%MJD_CUR, SUR%TAI_CUR, &
                      SUR%REF_STA, IND_SRC, AZ, EL, HA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1761, IUER, 'SUR_ELEV_REF', &
               'Error in computing azimuth and elevation' )
           SUR_GET_ELEV_REF = -10.0D0
           RETURN
      END IF
!
      SUR_GET_ELEV_REF = EL
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!#  FUNCTION SUR_GET_ELEV_REF   !#!#
