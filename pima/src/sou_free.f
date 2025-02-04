      SUBROUTINE VIS_FREE ( VIS )
! ************************************************************************
! *                                                                      *
! *   Auxilliary program VIS_FREE frees dinamic memory allocated in the  *
! *   fields of the object VIS which keeps visilibities.                 *
! *                                                                      *
! *  ### 08-FEB-2007    VIS_FREE   v1.2 (c)  L. Petrov  30-SEP-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE    'sou_map.i'
      TYPE      ( VIS__TYPE ) :: VIS
!
! --- Initialization
!
      IF ( ASSOCIATED ( VIS%MJD     ) ) DEALLOCATE ( VIS%MJD )
      IF ( ASSOCIATED ( VIS%TAI     ) ) DEALLOCATE ( VIS%TAI )
      IF ( ASSOCIATED ( VIS%VIS     ) ) DEALLOCATE ( VIS%VIS )
      IF ( ASSOCIATED ( VIS%UV      ) ) DEALLOCATE ( VIS%UV  )
      IF ( ASSOCIATED ( VIS%WEI     ) ) DEALLOCATE ( VIS%WEI )
      IF ( ASSOCIATED ( VIS%IND_BAS ) ) DEALLOCATE ( VIS%IND_BAS )
      IF ( ASSOCIATED ( VIS%INT_TIM ) ) DEALLOCATE ( VIS%INT_TIM )
      IF ( ASSOCIATED ( VIS%SKY_FRQ ) ) DEALLOCATE ( VIS%SKY_FRQ )
      IF ( ASSOCIATED ( VIS%C_STA   ) ) DEALLOCATE ( VIS%C_STA   )
      IF ( ASSOCIATED ( VIS%L_STA   ) ) DEALLOCATE ( VIS%L_STA   )
      IF ( ASSOCIATED ( VIS%LIS_STA ) ) DEALLOCATE ( VIS%LIS_STA   )
      CALL NOUT ( SIZEOF(VIS), VIS )
      VIS%STATUS  = SMP__UNDF
!
      RETURN
      END  SUBROUTINE  VIS_FREE  !#!# 
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MAP_FREE ( MAP )
! ************************************************************************
! *                                                                      *
! *   Auxilliary program VIS_FREE frees dinamic memory allocated in the  *
! *   fields of the object MAP which keeps the source image.             *
! *                                                                      *
! *  ### 08-FEB-2007    MAP_FREE   v1.0 (c)  L. Petrov  08-FEB-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE    'sou_map.i'
      TYPE     ( SOUMAP__TYPE ) :: MAP
      IF ( ASSOCIATED ( MAP%FLUX_CC ) ) DEALLOCATE ( MAP%FLUX_CC )
      IF ( ASSOCIATED ( MAP%COOR_CC ) ) DEALLOCATE ( MAP%COOR_CC )
      IF ( ASSOCIATED ( MAP%IMAGE   ) ) DEALLOCATE ( MAP%IMAGE )
      CALL NOUT ( SIZEOF(MAP), MAP )
      MAP%STATUS_CC  = SMP__UNDF
      MAP%STATUS_MAP = SMP__UNDF
!
      RETURN
      END  SUBROUTINE  MAP_FREE  !#!#
